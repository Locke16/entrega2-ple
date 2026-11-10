module Interpreter

import IO;
import List;
import String;
import ParseTree;
import util::Math;
import AST;
import ASTBuilder;
import Parser;

// Interpreter for the current Objectilang AST.
// Entry point: ejecutar(|project://.../programa.ol|)

// capturado: variables visibles donde se escribio el override (el cuerpo puede usarlas)
data Met = met(list[str] parametrosM, CuerpoMetodo cuerpoM, map[str, Valor] capturado = ());

data Valor
    = vInt(int n)
    | vReal(real r)
    | vBool(bool b)
    | vChar(str ch)
    | vStr(str s)
    | vUnit()
    | vLista(list[Valor] elems)
    | vObj(str cl, map[str, Valor] cm, map[str, Met] overrides);

data Clase = clase(str nombre, str padre, list[str] campos,
                   list[str] parametrosCtor, map[str, Met] metodos,
                   list[tuple[str, Exp]] iniciales);

alias Env = map[str, Valor];

map[str, Clase] clases = ();
map[str, Met] globales = ();
set[str] builtins = {"print", "println", "List", "range"};

// ------------------------------------------------------------
// Entrada

Programa cargar(loc archivo) = construirAST(archivo);

void ejecutar(loc archivo) {
    Programa programa = cargar(archivo);
    registrar(programa);
    Valor resultado = ejecutarCuerpo(programa.principal.cuerpoMain, ());
    if (resultado != vUnit()) println("Resultado: <mostrar(resultado)>");
}

Valor ejecutarPrograma(loc archivo) {
    Programa programa = cargar(archivo);
    registrar(programa);
    return ejecutarCuerpo(programa.principal.cuerpoMain, ());
}

// ------------------------------------------------------------
// Registro de objetos y métodos

void registrar(Programa programa) {
    clases = ();
    globales = ();

    list[Definicion] definiciones = programa.antes + programa.despues;

    // Primero se registran todas las clases para permitir referencias cruzadas.
    for (definicion <- definiciones) {
        if (defObjeto(objeto(nombre, _, herencia, constructor, cuerpoObj)) := definicion) {
            str padre = herencia == [] ? "" : nombreTipo(herencia[0].padre);
            list[str] nombresCamposCtor = [];
            list[str] nombresParametrosCtor = [];
            if (constructor != []) {
                for (parametro <- constructor[0].lista) {
                    switch (parametro) {
                        case paramVal(n, _): {
                            nombresCamposCtor += [n];
                            nombresParametrosCtor += [n];
                        }
                        case paramVar(n, _): {
                            nombresCamposCtor += [n];
                            nombresParametrosCtor += [n];
                        }
                    }
                }
            }
            clases[nombre] = clase(nombre, padre, nombresCamposCtor,
                                   nombresParametrosCtor, (), []);
        }
    }

    // Luego se registran los métodos de los objetos y los métodos globales.
    for (definicion <- definiciones) {
        switch (definicion) {
            case defMetodo(metodo(nombre, parametros, _, cuerpo)):
                globales[nombre] = met(nombresParams(parametros), cuerpo);

            case defAsignacion(asignacion(objetoNombre, metodoNombre, parametros, _, cuerpos)): {
                if (objetoNombre notin clases)
                    throw "Objeto no definido: <objetoNombre>";
                if (cuerpos != []) {
                    Clase actual = clases[objetoNombre];
                    map[str, Met] nuevos = actual.metodos +
                        (metodoNombre : met(nombresParams(parametros), cuerpos[0]));
                    clases[objetoNombre] = clase(actual.nombre, actual.padre,
                        actual.campos, actual.parametrosCtor, nuevos, actual.iniciales);
                }
            }

            case defObjeto(objeto(nombre, _, _, _, cuerpos)): {
                if (cuerpos != []) {
                    Clase actual = clases[nombre];
                    map[str, Met] metodosObjeto = actual.metodos;
                    list[tuple[str, Exp]] inicialesObjeto = actual.iniciales;
                    for (cuerpoObjeto <- cuerpos) {
                        for (miembro <- cuerpoObjeto.miembros) {
                            if (miembroMetodo(metodo(nm, ps, _, cm)) := miembro) {
                                metodosObjeto += (nm : met(nombresParams(ps), cm));
                            }
                            if (miembroDecl(declVal(nv, _, ev)) := miembro) {
                                inicialesObjeto += [<nv, ev>];
                            }
                            if (miembroDecl(declVar(nr, _, er)) := miembro) {
                                inicialesObjeto += [<nr, er>];
                            }
                        }
                    }
                    clases[nombre] = clase(actual.nombre, actual.padre,
                        actual.campos, actual.parametrosCtor, metodosObjeto, inicialesObjeto);
                }
            }
        }
    }
}

str nombreTipo(simple(nombre)) = nombre;
str nombreTipo(generico(nombre, _)) = nombre;

list[str] nombresParams(list[Parametros] parametros) =
    parametros == [] ? [] : nombresParamsU(parametros[0]);

list[str] nombresParamsU(parametros(lista)) = [p.nombre | p <- lista];

list[str] camposTodos(str nombre, set[str] visitados = {}) {
    if (nombre notin clases) throw "Clase no definida: <nombre>";
    if (nombre in visitados) throw "Herencia ciclica en <nombre>";
    Clase actual = clases[nombre];
    list[str] camposPadre = actual.padre == "" ? [] :
        camposTodos(actual.padre, visitados = visitados + {nombre});
    return [c | c <- camposPadre, c notin actual.campos] + actual.campos;
}

// Campos declarados en el cuerpo de un objeto (con valor inicial), empezando por los del padre.
list[tuple[str, Exp]] inicialesTodos(str nombre, set[str] visitados = {}) {
    if (nombre notin clases || nombre in visitados) return [];
    Clase actual = clases[nombre];
    list[tuple[str, Exp]] delPadre = actual.padre == "" ? [] :
        inicialesTodos(actual.padre, visitados = visitados + {nombre});
    return delPadre + actual.iniciales;
}

list[Met] buscarMetodo(str claseNombre, str nombreMetodo, set[str] visitados = {}) {
    if (claseNombre notin clases || claseNombre in visitados) return [];
    Clase actual = clases[claseNombre];
    if (nombreMetodo in actual.metodos) return [actual.metodos[nombreMetodo]];
    if (actual.padre == "") return [];
    return buscarMetodo(actual.padre, nombreMetodo,
                        visitados = visitados + {claseNombre});
}

// ------------------------------------------------------------
// Ejecución de cuerpos y sentencias

Valor ejecutarCuerpo(CuerpoMetodo cuerpo, Env entorno) {
    Valor ultimo = vUnit();
    Env actual = entorno;
    for (sentencia <- cuerpo.sentencias) {
        <actual, ultimo> = ejecutarSentencia(sentencia, actual);
    }
    return ultimo;
}

tuple[Env, Valor] ejecutarSentencia(Sentencia sentencia, Env entorno) {
    switch (sentencia) {
        case sentDecl(declVal(nombre, _, expresion)):
            return <entorno + (nombre : evaluar(expresion, entorno)), vUnit()>;

        case sentDecl(declVar(nombre, _, expresion)):
            return <entorno + (nombre : evaluar(expresion, entorno)), vUnit()>;

        case sentAsig(nombre, expresion): {
            if (nombre notin entorno) throw "Variable no definida: <nombre>";
            return <entorno + (nombre : evaluar(expresion, entorno)), vUnit()>;
        }

        case sentExp(expresion):
            return <entorno, evaluar(expresion, entorno)>;

        case sentFor(paraCada(clausula(patron, coleccion), cuerpoFor)):
            return ejecutarForPatron(patron, coleccion, cuerpoFor, entorno);

        case sentFor(paraCada(clausulaIn(nombre, coleccion), cuerpoFor)):
            return ejecutarForNombre(nombre, coleccion, cuerpoFor, entorno);
    }
    return <entorno, vUnit()>;
}

// El for devuelve tambien el entorno: las asignaciones a variables que ya existian
// antes del ciclo se conservan (la variable del ciclo y las locales del cuerpo no).
tuple[Env, Valor] ejecutarForPatron(Patron patron, Exp coleccion, Sentencia cuerpo, Env entorno) {
    Valor resultado = vUnit();
    Env actual = entorno;
    for (elemento <- elementos(evaluar(coleccion, entorno))) {
        Env local = ligar(patron, elemento, actual);
        <local, resultado> = ejecutarSentencia(cuerpo, local);
        actual = soloExistentes(local, entorno);
    }
    return <actual, resultado>;
}

tuple[Env, Valor] ejecutarForNombre(str nombre, Exp coleccion, Sentencia cuerpo, Env entorno) {
    Valor resultado = vUnit();
    Env actual = entorno;
    for (elemento <- elementos(evaluar(coleccion, entorno))) {
        Env local = actual + (nombre : elemento);
        <local, resultado> = ejecutarSentencia(cuerpo, local);
        actual = soloExistentes(local, entorno);
    }
    return <actual, resultado>;
}

Env soloExistentes(Env local, Env original) = (n : local[n] | n <- original, n in local);

list[Valor] elementos(vLista(lista)) = lista;
list[Valor] elementos(vStr(cadena)) =
    [vChar(substring(cadena, i, i + 1)) | i <- [0 .. size(cadena)]];
default list[Valor] elementos(Valor valor) {
    throw "No se puede iterar sobre <mostrar(valor)>";
}

Env ligar(patronSimple(nombre), Valor valor, Env entorno) = entorno + (nombre : valor);
Env ligar(patronTupla(primero, resto), vLista(valores), Env entorno) {
    list[str] nombres = [primero] + resto;
    if (size(nombres) != size(valores))
        throw "El patron tiene <size(nombres)> elementos y el valor <size(valores)>";
    Env nuevo = entorno;
    for (i <- [0 .. size(nombres)]) nuevo += (nombres[i] : valores[i]);
    return nuevo;
}

// ------------------------------------------------------------
// Evaluación de expresiones

Valor evaluar(dato(valor), Env entorno) = evaluarDato(valor, entorno);
Valor evaluar(variable(nombre), Env entorno) {
    if (nombre in entorno) return entorno[nombre];
    throw "Variable no definida: <nombre>";
}
Valor evaluar(agrupada(expresion), Env entorno) = evaluar(expresion, entorno);

Valor evaluar(expSi(condicion, entonces, sino), Env entorno) {
    Valor valorCondicion = evaluar(condicion, entorno);
    if (vBool(b) := valorCondicion)
        return b ? evaluar(entonces, entorno) : evaluar(sino, entorno);
    throw "La condicion del if debe ser Bool";
}

Valor evaluar(negacion(_, expresion), Env entorno) {
    Valor valor = evaluar(expresion, entorno);
    if (vBool(b) := valor) return vBool(!b);
    throw "La negacion requiere un operando Bool";
}
Valor evaluar(menos(expresion), Env entorno) = menosValor(evaluar(expresion, entorno));

Valor evaluar(potencia(a, b), Env entorno) = aritmetica("**", evaluar(a, entorno), evaluar(b, entorno));
Valor evaluar(mult(a, b), Env entorno) = aritmetica("*", evaluar(a, entorno), evaluar(b, entorno));
Valor evaluar(division(a, b), Env entorno) = aritmetica("/", evaluar(a, entorno), evaluar(b, entorno));
Valor evaluar(modulo(a, b), Env entorno) = aritmetica("%", evaluar(a, entorno), evaluar(b, entorno));
Valor evaluar(suma(a, b), Env entorno) = aritmetica("+", evaluar(a, entorno), evaluar(b, entorno));
Valor evaluar(resta(a, b), Env entorno) = aritmetica("-", evaluar(a, entorno), evaluar(b, entorno));

Valor evaluar(menorQue(a, b), Env entorno) = vBool(comparar(evaluar(a, entorno), evaluar(b, entorno)) < 0);
Valor evaluar(mayorQue(a, b), Env entorno) = vBool(comparar(evaluar(a, entorno), evaluar(b, entorno)) > 0);
Valor evaluar(menorIgual(a, b), Env entorno) = vBool(comparar(evaluar(a, entorno), evaluar(b, entorno)) <= 0);
Valor evaluar(mayorIgual(a, b), Env entorno) = vBool(comparar(evaluar(a, entorno), evaluar(b, entorno)) >= 0);
Valor evaluar(igual(a, b), Env entorno) = vBool(sonIguales(evaluar(a, entorno), evaluar(b, entorno)));
Valor evaluar(distinto(a, b), Env entorno) = vBool(!sonIguales(evaluar(a, entorno), evaluar(b, entorno)));

Valor evaluar(conY(a, b), Env entorno) {
    Valor izquierda = evaluar(a, entorno);
    if (vBool(x) := izquierda) {
        if (!x) return vBool(false);
        if (vBool(y) := evaluar(b, entorno)) return vBool(y);
    }
    throw "and requiere operandos Bool";
}

Valor evaluar(conO(a, b), Env entorno) {
    Valor izquierda = evaluar(a, entorno);
    if (vBool(x) := izquierda) {
        if (x) return vBool(true);
        if (vBool(y) := evaluar(b, entorno)) return vBool(y);
    }
    throw "or requiere operandos Bool";
}

Valor evaluar(pertenece(a, b), Env entorno) {
    Valor buscado = evaluar(a, entorno);
    Valor contenedor = evaluar(b, entorno);
    if (vLista(lista) := contenedor) return vBool(buscado in lista);
    if (vStr(cadena) := contenedor && vChar(caracter) := buscado)
        return vBool(contains(cadena, caracter));
    throw "El operador in requiere una lista o cadena a la derecha";
}

Valor evaluar(flecha(a, b), Env entorno) {
    // La semantica de -> no esta definida completamente en la especificacion.
    evaluar(a, entorno);
    return evaluar(b, entorno);
}
Valor evaluar(flechaDoble(a, b), Env entorno) {
    // La semantica de => no esta definida completamente en la especificacion.
    evaluar(a, entorno);
    return evaluar(b, entorno);
}

Valor evaluar(llamada(nombre, argumentos), Env entorno) {
    list[Valor] valores = argumentosDe(argumentos, entorno);
    if (nombre in builtins) return builtin(nombre, valores);
    if (nombre in globales) return ejecutarMetodo(globales[nombre], valores, ());
    throw "Funcion no definida: <nombre>";
}

Valor evaluar(acceso(receptor, nombre, llamadas), Env entorno) {
    Valor valor = evaluar(receptor, entorno);
    if (llamadas == []) return obtenerMiembro(valor, nombre, [], entorno);
    Valor resultado = valor;
    for (i <- [0 .. size(llamadas)]) {
        resultado = obtenerMiembro(resultado, nombre,
            argumentosDe(llamadas[i], entorno), entorno);
    }
    return resultado;
}

Valor evaluar(expNuevo(tipo, argumentos, cuerpos), Env entorno) {
    str nombreClase = nombreTipo(tipo);
    if (nombreClase notin clases) throw "Clase no definida: <nombreClase>";
    list[str] campos = camposTodos(nombreClase);
    list[Valor] valores = argumentos == [] ? [] : argumentosDe(argumentos[0], entorno);
    if (size(campos) != size(valores))
        throw "new <nombreClase>: se esperaban <size(campos)> argumentos y se pasaron <size(valores)>";
    map[str, Valor] camposObjeto = ();
    for (i <- [0 .. size(campos)]) camposObjeto += (campos[i] : valores[i]);
    for (<nombreCampo, valorInicial> <- inicialesTodos(nombreClase))
        camposObjeto += (nombreCampo : evaluar(valorInicial, camposObjeto));
    map[str, Met] overrides = ();
    for (cuerpoObjeto <- cuerpos) {
        for (override <- cuerpoObjeto.metodos) {
            overrides += (override.nombre : met(nombresParamsU(override.params), override.cuerpoMetodo, capturado = entorno));
        }
    }
    return vObj(nombreClase, camposObjeto, overrides);
}

list[Valor] argumentosDe(ArgsParen argumentos, Env entorno) {
    list[Valor] resultado = [];
    for (grupo <- argumentos.argumentos) {
        if (argumentos(lista) := grupo) resultado += [evaluar(e, entorno) | e <- lista];
    }
    return resultado;
}

Valor obtenerMiembro(Valor receptor, str nombre, list[Valor] args, Env entorno) {
    if (vObj(claseNombre, campos, overrides) := receptor) {
        if (args == [] && nombre in campos) return campos[nombre];
        if (args == [] && nombre == "getClass" && nombre notin overrides && buscarMetodo(claseNombre, nombre) == [])
            return vStr(claseNombre);
        return invocarMetodo(receptor, nombre, args);
    }
    return miembroPrimitivo(receptor, nombre, args);
}

Valor invocarMetodo(Valor objeto, str nombre, list[Valor] args) {
    if (vObj(claseNombre, campos, overrides) := objeto) {
        list[Met] candidatos = nombre in overrides ? [overrides[nombre]] : buscarMetodo(claseNombre, nombre);
        if (candidatos == []) throw "<claseNombre> no tiene el metodo <nombre>";
        Env base = candidatos[0].capturado + campos + ("this" : objeto);
        return ejecutarMetodo(candidatos[0], args, base);
    }
    throw "No es un objeto: <mostrar(objeto)>";
}

Valor ejecutarMetodo(Met metodo, list[Valor] argumentos, Env base) {
    if (size(metodo.parametrosM) != size(argumentos))
        throw "Se esperaban <size(metodo.parametrosM)> argumentos y se pasaron <size(argumentos)>";
    Env entorno = base;
    for (i <- [0 .. size(argumentos)])
        entorno += (metodo.parametrosM[i] : argumentos[i]);
    return ejecutarCuerpo(metodo.cuerpoM, entorno);
}

Valor miembroPrimitivo(Valor valor, str nombre, list[Valor] argumentos) {
    if (nombre == "toString" && argumentos == []) return vStr(mostrar(valor));
    if (nombre == "getSimpleName" && argumentos == [] && vStr(simple) := valor) return vStr(simple);
    if ((nombre == "length" || nombre == "size") && argumentos == []) {
        if (vStr(cadena) := valor) return vInt(size(cadena));
        if (vLista(lista) := valor) return vInt(size(lista));
    }
    throw "<mostrar(valor)> no tiene el miembro <nombre>";
}

// ------------------------------------------------------------
// Funciones predefinidas

Valor builtin(str nombre, list[Valor] argumentos) {
    switch (nombre) {
        case "print": {
            print(intercalate(" ", [mostrar(v) | v <- argumentos]));
            return vUnit();
        }
        case "println": {
            println(intercalate(" ", [mostrar(v) | v <- argumentos]));
            return vUnit();
        }
        case "List": return vLista(argumentos);
        case "range": {
            if ([vInt(inicio), vInt(fin)] := argumentos)
                return vLista([vInt(i) | i <- [inicio .. fin]]);
            throw "range(a, b) requiere dos enteros";
        }
    }
    throw "Funcion predefinida desconocida: <nombre>";
}

// ------------------------------------------------------------
// Literales e interpolacion

Valor evaluarDato(dEntero(n), Env entorno) = vInt(n);
Valor evaluarDato(dFlotante(r), Env entorno) = vReal(r);
Valor evaluarDato(dBooleano(b), Env entorno) = vBool(b);
Valor evaluarDato(dCaracter(c), Env entorno) = vChar(c);
Valor evaluarDato(dCadena(s), Env entorno) = vStr(interpolar(s, entorno));

bool esCaracterIdentificador(str caracter) = /^[a-zA-Z0-9_]$/ := caracter;

str interpolar(str cadena, Env entorno) {
    str salida = "";
    int i = 0;
    int n = size(cadena);
    while (i < n) {
        str actual = substring(cadena, i, i + 1);
        if (actual == "$" && i + 1 < n) {
            if (substring(cadena, i + 1, i + 2) == "{") {
                int j = i + 2;
                int profundidad = 1;
                while (j < n && profundidad > 0) {
                    str c = substring(cadena, j, j + 1);
                    if (c == "{") profundidad += 1;
                    else if (c == "}") profundidad -= 1;
                    j += 1;
                }
                if (profundidad != 0) throw "Interpolacion sin cerrar";
                salida += mostrar(evaluarTexto(substring(cadena, i + 2, j - 1), entorno));
                i = j;
            } else {
                int j = i + 1;
                while (j < n && esCaracterIdentificador(substring(cadena, j, j + 1))) j += 1;
                if (j > i + 1) {
                    str nombre = substring(cadena, i + 1, j);
                    if (nombre notin entorno) throw "Variable no definida en interpolacion: <nombre>";
                    salida += mostrar(entorno[nombre]);
                    i = j;
                } else {
                    salida += actual;
                    i += 1;
                }
            }
        } else {
            salida += actual;
            i += 1;
        }
    }
    return salida;
}

Valor evaluarTexto(str fuente, Env entorno) {
    Tree arbol = parsearExp(trim(fuente));
    Exp expresion = implode(#Exp, arbol);
    return evaluar(expresion, entorno);
}

// ------------------------------------------------------------
// Aritmetica, comparacion y conversion

real aReal(vInt(n)) = n * 1.0;
real aReal(vReal(r)) = r;
default real aReal(Valor valor) {
    throw "Se esperaba un numero y se obtuvo <mostrar(valor)>";
}

bool esNumero(vInt(_)) = true;
bool esNumero(vReal(_)) = true;
default bool esNumero(Valor valor) = false;

Valor menosValor(vInt(n)) = vInt(-n);
Valor menosValor(vReal(r)) = vReal(-r);
default Valor menosValor(Valor valor) {
    throw "No se puede aplicar menos a <mostrar(valor)>";
}

int potenciaEntera(int base, int exponente) {
    int resultado = 1;
    for (_ <- [0 .. exponente]) resultado *= base;
    return resultado;
}

Valor aritmetica(str operador, Valor a, Valor b) {
    if (vInt(x) := a && vInt(y) := b) {
        switch (operador) {
            case "+": return vInt(x + y);
            case "-": return vInt(x - y);
            case "*": return vInt(x * y);
            case "/": {
                if (y == 0) throw "Division por cero";
                return vInt(x / y);
            }
            case "%": {
                if (y == 0) throw "Modulo por cero";
                return vInt(x % y);
            }
            case "**": {
                if (y >= 0) return vInt(potenciaEntera(x, y));
                return vReal(1.0 / (potenciaEntera(x, -y) * 1.0));
            }
        }
    }
    if (operador == "+" && (vStr(_) := a || vStr(_) := b))
        return vStr(mostrar(a) + mostrar(b));
    real x = aReal(a);
    real y = aReal(b);
    switch (operador) {
        case "+": return vReal(x + y);
        case "-": return vReal(x - y);
        case "*": return vReal(x * y);
        case "/": {
            if (y == 0.0) throw "Division por cero";
            return vReal(x / y);
        }
        case "%": {
            if (y == 0.0) throw "Modulo por cero";
            return vReal(x - y * floor(x / y));
        }
        case "**": return vReal(exp(y * ln(x)));
    }
    throw "Operador desconocido: <operador>";
}

int comparar(vStr(a), vStr(b)) = a < b ? -1 : (a == b ? 0 : 1);
int comparar(vChar(a), vChar(b)) = a < b ? -1 : (a == b ? 0 : 1);
default int comparar(Valor a, Valor b) {
    if (esNumero(a) && esNumero(b)) {
        real x = aReal(a);
        real y = aReal(b);
        return x < y ? -1 : (x == y ? 0 : 1);
    }
    throw "No se pueden comparar <mostrar(a)> y <mostrar(b)>";
}

bool sonIguales(Valor a, Valor b) {
    if (esNumero(a) && esNumero(b)) return aReal(a) == aReal(b);
    return a == b;
}

// ------------------------------------------------------------
// Representacion de valores

str mostrar(vInt(n)) = "<n>";
str mostrar(vReal(r)) = "<r>";
str mostrar(vBool(b)) = b ? "true" : "false";
str mostrar(vChar(c)) = c;
str mostrar(vStr(s)) = s;
str mostrar(vUnit()) = "()";
str mostrar(vLista(lista)) = "List(<intercalate(", ", [mostrar(x) | x <- lista])>)";
str mostrar(vObj(nombre, campos, _)) =
    "<nombre>(<intercalate(", ", ["<campo>=<mostrar(campos[campo])>" | campo <- campos])>)";
