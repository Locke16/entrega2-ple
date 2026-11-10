module Checker

// =====================================================================
// VERIFICADOR DE TIPOS DE OBJECTILANG (con TypePal)
//
// Que hace: recorre el arbol sintactico de un programa y revisa que
//   (punto 5) los tipos usados correspondan a los definidos, y que
//   (punto 6) todo lo que se nombra exista (mensajes "[Existencia]").
//
// Como usarlo (terminal de Rascal):
//   import Checker;
//   validar(|project://entrega2-ple/test/objetos_ok.ol|);
//
// Como esta organizado el archivo:
//   1. Tipos (AType) y tablas globales con los objetos del programa.
//   2. Subtipado con herencia y varianza.
//   3. Conversion de los tipos escritos por el usuario a AType.
//   4. Pre-paso: llenar las tablas ANTES de revisar (construirTabla).
//   5. "collect": reglas que recorren cada construccion del lenguaje.
//   6. Operadores, y al final el punto de entrada (validar).
//
// GLOSARIO de la API de TypePal (nombres en ingles que no se pueden cambiar):
//   collect(arbol, c)   = "recolectar": regla que se ejecuta para cada nodo del arbol.
//   c.define(...)       = declarar un nombre (variable, parametro) en el ambito actual.
//   c.use(...)          = usar un nombre; TypePal busca su declaracion (si no, "Undefined").
//   c.fact(arbol, tipo) = afirmar que ese nodo tiene ese tipo.
//   c.calculate(...)    = calcular el tipo de un nodo cuando ya se conocen los de sus hijos.
//   c.requireSubType    = exigir que un tipo sea subtipo de otro (si no, error).
//   c.enterScope/leaveScope = abrir/cerrar un ambito (bloque donde viven las variables).
//   c.report / s.report = reportar un error (c: al recolectar, s: al resolver tipos).
//   Solver s            = el "resolvedor": s.getType(arbol) da el tipo ya calculado de un nodo.
//   %t en un mensaje    = se reemplaza por el tipo (o el texto) del argumento correspondiente.
//   AType               = tipo de TypePal; aqui lo extendemos con nuestros propios tipos.
// =====================================================================

import Syntax;
extend analysis::typepal::TypePal;

import ParseTree;
import Parser;
import Message;
import IO;
import List;

// ---------- Tipos del lenguaje ----------

// Varianza de un parametro de tipo: invariante (A), covariante (+A) o contravariante (-A).
data Varianza = varianzaInv() | varianzaCov() | varianzaContra();

// Tipos del lenguaje. Los basicos y, para los objetos, tipoObjeto(nombre, argumentos):
//   Dog -> tipoObjeto("Dog", [])      List[Dog] -> tipoObjeto("List", [tipoObjeto("Dog", [])])
// tipoParametro es una variable de tipo dentro de una definicion (la A de List[A]).
// tipoError se pone a lo que ya dio error: es compatible con todo, para no repetir errores.
data AType
    = tipoInt()
    | tipoFloat()
    | tipoBoolean()
    | tipoChar()
    | tipoString()
    | tipoUnit()
    | tipoObjeto(str nombre, list[AType] args)    // instancia de un objeto: Lista[Dog]
    | tipoParametro(str nombre)                      // parametro de tipo dentro de una definicion
    | tipoClase()                              // resultado de getClass
    | tipoError()                              // tipo ya reportado como error (evita cascadas)
    ;

// Firma de un metodo: tipos de sus parametros y tipo de retorno (Unit si no se anota).
data InfoMetodo = infoMetodo(list[AType] tiposParams, AType tipoRetorno);

// Texto "A, B, C" a partir de una lista de tipos (para los mensajes de error).
str textoListaTipos(list[AType] ts) {
    list[str] partes = [prettyAType(t) | AType t <- ts];
    return intercalate(", ", partes);
}

// prettyAType es el nombre que usa TypePal para mostrar un tipo en los mensajes.
str prettyAType(tipoInt())   = "Int";
str prettyAType(tipoFloat()) = "Float";
str prettyAType(tipoBoolean())  = "Boolean";
str prettyAType(tipoChar())  = "Char";
str prettyAType(tipoString())   = "String";
str prettyAType(tipoUnit())  = "Unit";
str prettyAType(tipoClase()) = "Class";
str prettyAType(tipoError()) = "?";
str prettyAType(tipoParametro(str n)) = n;
str prettyAType(tipoObjeto(str n, list[AType] args)) = size(args) == 0 ? n : "<n>[<textoListaTipos(args)>]";

// ---------- Tablas de la definicion del programa ----------

// ---- Tablas globales: se llenan en construirTabla antes de revisar el programa ----
map[str, list[str]] tParametrosTipo = ();               // objeto -> nombres de sus parametros de tipo
map[str, list[Varianza]] tVarianzas = ();        // objeto -> varianza de cada parametro
map[str, AType] tPadre = ();                     // objeto -> instancia del padre (en terminos de sus parametros)
map[str, list[AType]] tConstructor = ();                // objeto -> tipos de los parametros del constructor
map[str, map[str, AType]] tCampos = ();          // objeto -> campo -> tipo
map[str, map[str, InfoMetodo]] tMetodos = ();    // objeto -> metodo -> firma
map[str, InfoMetodo] tFunciones = ();            // funciones de nivel superior
map[str, Tree] tArbol = ();                      // objeto -> arbol de su nombre (para reportar errores)
set[str] objetosVisitados = {};
set[str] tiposParamActuales = {};                       // parametros de tipo visibles en el codigo que se recolecta
str objetoActual = "";                           // objeto cuyo cuerpo se esta recolectando ("" = ninguno)

// Vacia las tablas (cada programa que se valida parte de cero).
void limpiarTablas() {
    tParametrosTipo = (); tVarianzas = (); tPadre = (); tConstructor = ();
    tCampos = (); tMetodos = (); tFunciones = (); tArbol = ();
    objetosVisitados = {}; tiposParamActuales = {}; objetoActual = "";
}

// Configuracion de TypePal: le decimos como decidir si un tipo es subtipo de otro.
TypePalConfig configuracion() = tconfig(
    isSubType = bool(AType a, AType b) { return subtipo(a, b); }
);

// ---------- Subtipado con herencia y varianza ----------

// Reemplaza los parametros de tipo por los argumentos reales.
//   sustituir(A, ["A"], [Dog]) = Dog      sustituir(List[A], ["A"], [Dog]) = List[Dog]
AType sustituir(AType t, list[str] nombres, list[AType] args) {
    switch (t) {
        case tipoParametro(str n): {
            int pos = indexOf(nombres, n);
            return (pos >= 0 && pos < size(args)) ? args[pos] : t;
        }
        case tipoObjeto(str n, list[AType] tiposArgs):
            return tipoObjeto(n, [sustituir(x, nombres, args) | AType x <- tiposArgs]);
        default:
            return t;
    }
}

// a es subtipo de b si: son iguales, o es un objeto que hereda (extends) de b,
// o son el mismo objeto generico y sus argumentos cumplen la varianza.
bool subtipo(AType a, AType b) {
    if (a == b) return true;
    if (a == tipoError() || b == tipoError()) return true;
    if (tipoObjeto(str n1, list[AType] args1) := a) {
        if (tipoObjeto(str n2, list[AType] args2) := b && n1 == n2) {
            return argsCompatibles(n1, args1, args2);
        }
        if (n1 in tPadre) {
            return subtipo(sustituir(tPadre[n1], tParametrosTipo[n1], args1), b);
        }
    }
    return false;
}

// Compara los argumentos de tipo de dos instancias del mismo objeto generico segun varianza:
//   invariante: deben ser equivalentes; covariante (+): a1 <: a2; contravariante (-): a2 <: a1.
bool argsCompatibles(str n, list[AType] a1, list[AType] a2) {
    if (n notin tVarianzas) return false;
    if (size(a1) != size(a2) || size(a1) != size(tVarianzas[n])) return false;
    for (int i <- [0..size(a1)]) {
        switch (tVarianzas[n][i]) {
            case varianzaInv():
                if (!(subtipo(a1[i], a2[i]) && subtipo(a2[i], a1[i]))) return false;
            case varianzaCov():
                if (!subtipo(a1[i], a2[i])) return false;
            case varianzaContra():
                if (!subtipo(a2[i], a1[i])) return false;
        }
    }
    return true;
}

// Ancestro comun mas cercano; tipoError() si no hay.
// Tipo comun mas cercano de dos tipos (se usa en el if: las dos ramas). tipoError si no hay.
AType ancestroComun(AType a, AType b) {
    if (subtipo(a, b)) return b;
    if (subtipo(b, a)) return a;
    if (tipoObjeto(str n, list[AType] tiposArgs) := a && n in tPadre) {
        return ancestroComun(sustituir(tPadre[n], tParametrosTipo[n], tiposArgs), b);
    }
    return tipoError();
}

// Busqueda de miembros subiendo por la herencia; los tipos salen ya
// sustituidos con los argumentos de tipo del receptor.
// Busca un campo en el objeto y luego en sus ancestros; el tipo sale con los parametros ya sustituidos.
// Devuelve <encontrado?, tipo>.
tuple[bool, AType] buscarCampo(AType t, str nombre) {
    if (tipoObjeto(str n, list[AType] tiposArgs) := t && n in tParametrosTipo) {
        if (nombre in tCampos[n]) {
            return <true, sustituir(tCampos[n][nombre], tParametrosTipo[n], tiposArgs)>;
        }
        if (n in tPadre) {
            return buscarCampo(sustituir(tPadre[n], tParametrosTipo[n], tiposArgs), nombre);
        }
    }
    return <false, tipoError()>;
}

// Igual que buscarCampo pero para metodos. Devuelve <encontrado?, tipos de parametros, tipo de retorno>.
tuple[bool, list[AType], AType] buscarMetodo(AType t, str nombre) {
    if (tipoObjeto(str n, list[AType] tiposArgs) := t && n in tParametrosTipo) {
        if (nombre in tMetodos[n]) {
            InfoMetodo im = tMetodos[n][nombre];
            list[AType] ps = [sustituir(p, tParametrosTipo[n], tiposArgs) | AType p <- im.tiposParams];
            return <true, ps, sustituir(im.tipoRetorno, tParametrosTipo[n], tiposArgs)>;
        }
        if (n in tPadre) {
            return buscarMetodo(sustituir(tPadre[n], tParametrosTipo[n], tiposArgs), nombre);
        }
    }
    return <false, [], tipoError()>;
}

// ---------- Tipos escritos por el usuario ----------
// reportar = false: se usa al recolectar firmas por adelantado cuando el
// mismo arbol se vuelve a convertir mas tarde (evita mensajes duplicados).
// permitirSinArgs = true: solo para "new X(...)", donde los argumentos de
// tipo de un objeto generico se infieren.

// Convierte un tipo escrito por el usuario (Tipo del arbol) en un AType.
//  - tiposParam: parametros de tipo visibles aqui (los de la definicion que se esta recolectando).
//  - reportar: false si el mismo arbol se vuelve a convertir despues (evita errores duplicados).
//  - permitirSinArgs: true solo para "new X(...)", donde los argumentos de tipo se infieren.
// Aqui vive buena parte de la regla de existencia (punto 6): un tipo desconocido da "[Existencia]".
AType tipoAType(Tipo t, set[str] tiposParam, Collector c, bool reportar, bool permitirSinArgs) {
    if ((Tipo) `<Identificador id>` := t) {
        str n = "<id>";
        switch (n) {
            case "Int":     return tipoInt();
            case "Float":   return tipoFloat();
            case "Boolean": return tipoBoolean();
            case "Char":    return tipoChar();
            case "String":  return tipoString();
        }
        if (n in tiposParam) return tipoParametro(n);
        if (n in tParametrosTipo) {
            if (size(tParametrosTipo[n]) == 0 || permitirSinArgs) return tipoObjeto(n, []);
            if (reportar) c.report(error(t, "El objeto <n> es generico: requiere <size(tParametrosTipo[n])> argumento(s) de tipo, por ejemplo <n>[...]"));
            return tipoError();
        }
        if (reportar) c.report(error(t, "[Existencia] El tipo <n> no existe (no es Int, Float, Boolean, Char, String, un objeto definido ni un parametro de tipo declarado)"));
        return tipoError();
    }
    if ((Tipo) `<Identificador id> [ <{Tipo ","}+ ts> ]` := t) {
        str n = "<id>";
        list[AType] tiposArgs = [tipoAType(x, tiposParam, c, reportar, false) | Tipo x <- ts];
        if (n in tParametrosTipo) {
            if (size(tParametrosTipo[n]) != size(tiposArgs)) {
                if (reportar) c.report(error(t, "El objeto <n> espera <size(tParametrosTipo[n])> argumento(s) de tipo, pero recibio <size(tiposArgs)>"));
                return tipoError();
            }
            return tipoObjeto(n, tiposArgs);
        }
        if (reportar) c.report(error(t, "[Existencia] El objeto <n> no existe, no se puede usar con argumentos de tipo"));
        return tipoError();
    }
    return tipoError();
}

// Cada tipo escrito en el programa recibe su AType como "hecho" (c.fact).
void collect(current: (Tipo) `<Identificador _>`, Collector c) {
    c.fact(current, tipoAType(current, tiposParamActuales, c, true, false));
}

void collect(current: (Tipo) `<Identificador _> [ <{Tipo ","}+ _> ]`, Collector c) {
    c.fact(current, tipoAType(current, tiposParamActuales, c, true, false));
}

// ---------- Recoleccion por adelantado de objetos, metodos y funciones ----------

// Lee los tipos de los parametros y el tipo de retorno de un metodo.
InfoMetodo infoDeMetodo(list[Parametros] ps, list[AnotTipo] ret, set[str] tiposParam, Collector c) {
    list[AType] ptipos = [];
    for (Parametros p <- ps) {
        if ((Parametros) `( <{Parametro ","}* xs> )` := p) {
            for (Parametro x <- xs) {
                if ((Parametro) `<Identificador _> : <Tipo t>` := x) {
                    ptipos += [tipoAType(t, tiposParam, c, true, false)];
                }
            }
        }
    }
    AType rtipo = tipoUnit();
    for (AnotTipo a <- ret) {
        if ((AnotTipo) `: <Tipo t>` := a) {
            rtipo = tipoAType(t, tiposParam, c, true, false);
        }
    }
    return infoMetodo(ptipos, rtipo);
}

// Paso A del pre-paso: registrar el NOMBRE de cada objeto, sus parametros de tipo y su varianza.
// (Primero todos los nombres, para que un objeto pueda mencionar a otro definido mas abajo.)
bool declararObjeto(DefinicionObjeto ob, Collector c) {
    if ((DefinicionObjeto) `object <Identificador id> <ParametrosTipo? pt> <ClausulaExtends? _> <ParametrosConstructor? _> <CuerpoDeObjeto? _>` := ob) {
        str n = "<id>";
        if (n in tParametrosTipo) {
            c.report(error(id, "El objeto <n> ya esta definido"));
            return false;
        }
        list[str] nombres = [];
        list[Varianza] vars = [];
        for ((ParametrosTipo) `[ <{ParametroTipo ","}+ ps> ]` <- pt) {
            for (ParametroTipo p <- ps) {
                if ((ParametroTipo) `+ <Identificador i>` := p) {
                    nombres += ["<i>"]; vars += [varianzaCov()];
                } else if ((ParametroTipo) `- <Identificador i>` := p) {
                    nombres += ["<i>"]; vars += [varianzaContra()];
                } else if ((ParametroTipo) `<Identificador i>` := p) {
                    nombres += ["<i>"]; vars += [varianzaInv()];
                }
            }
        }
        tParametrosTipo[n] = nombres;
        tVarianzas[n] = vars;
        tConstructor[n] = [];
        tCampos[n] = ();
        tMetodos[n] = ();
        tArbol[n] = id;
        return true;
    }
    return false;
}

// Registra un campo (de constructor o de cuerpo) en la tabla del objeto; avisa si esta repetido.
void agregarCampo(str n, Identificador i, Tipo t, set[str] tiposParam, Collector c, bool esCtor) {
    str nombre = "<i>";
    AType tipo = tipoAType(t, tiposParam, c, esCtor, false);   // los del cuerpo se reportan al recolectar la declaracion
    if (nombre in tCampos[n]) {
        c.report(error(i, "El campo <nombre> ya esta definido en el objeto <n>"));
    }
    tCampos[n][nombre] = tipo;
    if (esCtor) tConstructor[n] += [tipo];
}

// Campos declarados dentro del cuerpo { ... } del objeto: deben llevar tipo anotado.
void registrarCampoCuerpo(str n, DeclaracionVariable dv, set[str] tiposParam, Collector c) {
    if ((DeclaracionVariable) `val <Identificador i> : <Tipo t> = <Exp _>` := dv) {
        agregarCampo(n, i, t, tiposParam, c, false);
    } else if ((DeclaracionVariable) `var <Identificador i> : <Tipo t> = <Exp _>` := dv) {
        agregarCampo(n, i, t, tiposParam, c, false);
    } else if ((DeclaracionVariable) `val <Identificador i> = <Exp _>` := dv) {
        c.report(error(dv, "El campo <i> del objeto <n> debe llevar anotacion de tipo"));
    } else if ((DeclaracionVariable) `var <Identificador i> = <Exp _>` := dv) {
        c.report(error(dv, "El campo <i> del objeto <n> debe llevar anotacion de tipo"));
    }
}

// Metodo declarado dentro del cuerpo del objeto: se guarda su firma.
void registrarMetodoMiembro(str n, DefinicionMetodo dm, set[str] tiposParam, Collector c) {
    if ((DefinicionMetodo) `method <Identificador m> <Parametros? ps> <AnotTipo? ret> <CuerpoMetodo _>` := dm) {
        str mn = "<m>";
        InfoMetodo info = infoDeMetodo([p | Parametros p <- ps], [r | AnotTipo r <- ret], tiposParam, c);
        if (mn in tMetodos[n]) {
            c.report(error(m, "El metodo <mn> ya esta definido en el objeto <n>"));
        } else {
            tMetodos[n][mn] = info;
        }
    }
}

// Paso B: para cada objeto, registrar su padre (extends), campos del constructor,
// campos y metodos del cuerpo. Aqui se valida que el padre y los tipos existan.
void completarObjeto(DefinicionObjeto ob, Collector c) {
    if ((DefinicionObjeto) `object <Identificador id> <ParametrosTipo? _> <ClausulaExtends? ext> <ParametrosConstructor? ctor> <CuerpoDeObjeto? cuerpo>` := ob) {
        str n = "<id>";
        set[str] tiposParam = toSet(tParametrosTipo[n]);

        for ((ClausulaExtends) `extends <Tipo p>` <- ext) {
            AType tipoPadre = tipoAType(p, tiposParam, c, true, false);
            if (tipoObjeto(_, _) := tipoPadre) {
                tPadre[n] = tipoPadre;
            } else if (tipoPadre != tipoError()) {
                c.report(error(p, "[Existencia] Un objeto solo puede extender a otro objeto, pero <n> extiende a <prettyAType(tipoPadre)>"));
            }
        }

        for ((ParametrosConstructor) `( <{ParametroConstructor ","}+ ps> )` <- ctor) {
            for (ParametroConstructor pc <- ps) {
                if ((ParametroConstructor) `val <Identificador i> : <Tipo t>` := pc) {
                    agregarCampo(n, i, t, tiposParam, c, true);
                } else if ((ParametroConstructor) `var <Identificador i> : <Tipo t>` := pc) {
                    agregarCampo(n, i, t, tiposParam, c, true);
                }
            }
        }

        for ((CuerpoDeObjeto) `{ <MiembroObjeto* ms> }` <- cuerpo) {
            for (MiembroObjeto m <- ms) {
                if ((MiembroObjeto) `<DeclaracionVariable dv>` := m) {
                    registrarCampoCuerpo(n, dv, tiposParam, c);
                } else if ((MiembroObjeto) `<DefinicionMetodo dm>` := m) {
                    registrarMetodoMiembro(n, dm, tiposParam, c);
                }
            }
        }
    }
}

// Funcion de nivel superior (method f(...) { ... } fuera de un objeto): se guarda su firma.
void registrarFuncion(DefinicionMetodo dm, Collector c) {
    if ((DefinicionMetodo) `method <Identificador m> <Parametros? ps> <AnotTipo? ret> <CuerpoMetodo _>` := dm) {
        str mn = "<m>";
        InfoMetodo info = infoDeMetodo([p | Parametros p <- ps], [r | AnotTipo r <- ret], {}, c);
        if (mn in tFunciones) {
            c.report(error(m, "La funcion <mn> ya esta definida"));
        } else {
            tFunciones[mn] = info;
        }
    }
}

// "Obj.m = method ...": agrega el metodo m al objeto Obj. Regla de existencia: Obj debe existir.
void registrarAsignacion(AsignacionMetodo am, Collector c) {
    if ((AsignacionMetodo) `<Identificador ob> . <Identificador m> = method <Parametros? ps> <AnotTipo? ret> <CuerpoMetodo? _>` := am) {
        str nombreObjeto = "<ob>";
        str nombreMetodo = "<m>";
        if (nombreObjeto notin tParametrosTipo) {
            c.report(error(ob, "[Existencia] El objeto <nombreObjeto> no existe, no se le puede asignar el metodo <nombreMetodo>"));
            return;
        }
        InfoMetodo info = infoDeMetodo([p | Parametros p <- ps], [r | AnotTipo r <- ret], toSet(tParametrosTipo[nombreObjeto]), c);
        if (nombreMetodo in tMetodos[nombreObjeto]) {
            c.report(error(m, "El metodo <nombreMetodo> ya esta definido en el objeto <nombreObjeto>"));
        } else {
            tMetodos[nombreObjeto][nombreMetodo] = info;
        }
    }
}

// Detecta herencia circular (A extends B, B extends A) y la rompe para evitar ciclos infinitos.
void verificarHerencia(Collector c) {
    for (str n <- [k | str k <- tPadre]) {
        if (n in tPadre) {
            set[str] visitados = {n};
            str actual = n;
            while (actual in tPadre) {
                if (tipoObjeto(str pn, _) := tPadre[actual]) {
                    if (pn in visitados) {
                        c.report(error(tArbol[n], "[Existencia] Herencia circular: el objeto <n> termina heredando de si mismo"));
                        tPadre = (k : tPadre[k] | str k <- tPadre, k != n);
                        break;
                    }
                    visitados += {pn};
                    actual = pn;
                } else {
                    break;
                }
            }
        }
    }
}

// Pre-paso completo: llena todas las tablas globales leyendo las definiciones del programa.
void construirTabla(list[Definicion] defs, Collector c) {
    limpiarTablas();
    list[DefinicionObjeto] objetos = [];
    for (Definicion d <- defs) {
        if ((Definicion) `<DefinicionObjeto ob>` := d) {
            if (declararObjeto(ob, c)) objetos += [ob];
        }
    }
    for (DefinicionObjeto ob <- objetos) {
        completarObjeto(ob, c);
    }
    for (Definicion d <- defs) {
        if ((Definicion) `<DefinicionMetodo dm>` := d) {
            registrarFuncion(dm, c);
        } else if ((Definicion) `<AsignacionMetodo am>` := d) {
            registrarAsignacion(am, c);
        }
    }
    verificarHerencia(c);
}

// ---------- Programa y definiciones ----------

// Recolecta cualquier nodo del arbol (listas de definiciones, sentencias, miembros...).
// Existe solo para llamar a collect con un argumento de tipo Tree; el editor avisa si no.
void recolectar(Tree arbol, Collector c) {
    collect(arbol, c);
}

// ---------- Reglas collect: se ejecutan al recorrer el arbol del programa ----------
// Punto de partida: primero se construyen las tablas y luego se recorre todo el programa.
void collect(current: (Programa) `<Definicion* antes> <Principal p> <Definicion* despues>`, Collector c) {
    list[Definicion] defs = [d | Definicion d <- antes] + [d | Definicion d <- despues];
    construirTabla(defs, c);
    recolectar(antes, c);
    collect(p, c);
    recolectar(despues, c);
}

// El metodo main abre su propio ambito.
void collect(current: (Principal) `method main <CuerpoMetodo cuerpo>`, Collector c) {
    objetoActual = "";
    tiposParamActuales = {};
    c.enterScope(current);
        collect(cuerpo, c);
    c.leaveScope(current);
}

void collect(current: (CuerpoMetodo) `{ <{Sentencia ";"}* sentencias> }`, Collector c) {
    for (Sentencia sentencia <- sentencias) {
        recolectarSentencia(sentencia, c);
    }
}

// Declara los parametros de un metodo como variables (con los tipos de su firma) en el ambito actual.
void definirParametros(list[Parametros] ps, list[AType] tipos, Collector c) {
    for (Parametros p <- ps) {
        if ((Parametros) `( <{Parametro ","}* xs> )` := p) {
            int idx = 0;
            for (Parametro x <- xs) {
                if ((Parametro) `<Identificador i> : <Tipo _>` := x) {
                    AType tp = idx < size(tipos) ? tipos[idx] : tipoError();
                    c.define("<i>", variableId(), i, defType(tp));
                }
                idx += 1;
            }
        }
    }
}

// El cuerpo de un metodo vale lo que vale su ultima sentencia.
// El cuerpo de un metodo vale lo que vale su ultima sentencia: debe ser subtipo del retorno declarado.
void chequearRetorno(CuerpoMetodo cuerpo, AType ret, Collector c) {
    if (ret == tipoUnit() || ret == tipoError()) return;
    if ((CuerpoMetodo) `{ <{Sentencia ";"}* ss> }` := cuerpo) {
        list[Sentencia] lista = [s | Sentencia s <- ss];
        if (size(lista) == 0) {
            c.report(error(cuerpo, "El cuerpo esta vacio, pero se esperaba un valor de tipo %t", ret));
            return;
        }
        Sentencia ultima = lista[-1];
        if ((Sentencia) `<Exp e>` := ultima) {
            c.requireSubType(e, ret, error(e, "El metodo devuelve %t pero se esperaba %t", e, ret));
        } else {
            c.report(error(ultima, "El metodo debe terminar en una expresion de tipo %t", ret));
        }
    }
}

// Definicion de objeto: se abre un ambito y los campos del constructor quedan como variables;
// luego se recorren los miembros del cuerpo (campos y metodos).
void collect(current: (DefinicionObjeto) `object <Identificador id> <ParametrosTipo? _> <ClausulaExtends? _> <ParametrosConstructor? ctor> <CuerpoDeObjeto? cuerpo>`, Collector c) {
    str n = "<id>";
    if (n notin tParametrosTipo || n in objetosVisitados) return;
    objetosVisitados += {n};

    set[str] tpsAntes = tiposParamActuales;
    str objAntes = objetoActual;
    tiposParamActuales = toSet(tParametrosTipo[n]);
    objetoActual = n;

    c.enterScope(current);
        // los campos del constructor son variables visibles en el objeto
        for ((ParametrosConstructor) `( <{ParametroConstructor ","}+ ps> )` <- ctor) {
            int idx = 0;
            for (ParametroConstructor pc <- ps) {
                AType tp = idx < size(tConstructor[n]) ? tConstructor[n][idx] : tipoError();
                if ((ParametroConstructor) `val <Identificador i> : <Tipo _>` := pc) {
                    c.define("<i>", variableId(), i, defType(tp));
                } else if ((ParametroConstructor) `var <Identificador i> : <Tipo _>` := pc) {
                    c.define("<i>", variableId(), i, defType(tp));
                }
                idx += 1;
            }
        }
        for ((CuerpoDeObjeto) `{ <MiembroObjeto* ms> }` <- cuerpo) {
            for (MiembroObjeto m <- ms) {
                recolectar(m, c);
            }
        }
    c.leaveScope(current);

    tiposParamActuales = tpsAntes;
    objetoActual = objAntes;
}

// Definicion de metodo (de un objeto o de nivel superior): parametros como variables y cuerpo revisado.
void collect(current: (DefinicionMetodo) `method <Identificador m> <Parametros? ps> <AnotTipo? _> <CuerpoMetodo cuerpo>`, Collector c) {
    str nombre = "<m>";
    InfoMetodo info = infoMetodo([], tipoError());
    if (objetoActual != "") {
        if (nombre notin tMetodos[objetoActual]) return;
        info = tMetodos[objetoActual][nombre];
    } else {
        if (nombre notin tFunciones) return;
        info = tFunciones[nombre];
    }
    c.enterScope(current);
        definirParametros([p | Parametros p <- ps], info.tiposParams, c);
        collect(cuerpo, c);
        chequearRetorno(cuerpo, info.tipoRetorno, c);
    c.leaveScope(current);
}

// "Obj.m = method ... { cuerpo }": revisa el cuerpo contra la firma registrada en construirTabla.
void collect(current: (AsignacionMetodo) `<Identificador ob> . <Identificador m> = method <Parametros? ps> <AnotTipo? _> <CuerpoMetodo? cuerpo>`, Collector c) {
    str nombreObjeto = "<ob>";
    str nombreMetodo = "<m>";
    if (nombreObjeto notin tParametrosTipo || nombreMetodo notin tMetodos[nombreObjeto]) return;
    InfoMetodo info = tMetodos[nombreObjeto][nombreMetodo];

    set[str] tpsAntes = tiposParamActuales;
    str objAntes = objetoActual;
    tiposParamActuales = toSet(tParametrosTipo[nombreObjeto]);
    objetoActual = nombreObjeto;

    for (CuerpoMetodo cb <- cuerpo) {
        c.enterScope(current);
            definirParametros([p | Parametros p <- ps], info.tiposParams, c);
            collect(cb, c);
            chequearRetorno(cb, info.tipoRetorno, c);
        c.leaveScope(current);
    }

    tiposParamActuales = tpsAntes;
    objetoActual = objAntes;
}

// ---------- Declaraciones de variables ----------

// ---------- val / var ----------
// Sin anotacion: la variable toma el tipo de su valor. Con anotacion: el valor debe ser subtipo de ella.
void collect(current: (DeclaracionVariable) `val <Identificador id> = <Exp e>`, Collector c) {
    c.define("<id>", variableId(), id, defType(e));
    collect(e, c);
}

void collect(current: (DeclaracionVariable) `var <Identificador id> = <Exp e>`, Collector c) {
    c.define("<id>", variableId(), id, defType(e));
    collect(e, c);
}

void collect(current: (DeclaracionVariable) `val <Identificador id> : <Tipo t> = <Exp e>`, Collector c) {
    c.define("<id>", variableId(), id, defType(t));
    c.requireSubType(e, t, error(e, "El valor es de tipo %t pero la anotacion dice %t", e, t));
    collect(t, e, c);
}

void collect(current: (DeclaracionVariable) `var <Identificador id> : <Tipo t> = <Exp e>`, Collector c) {
    c.define("<id>", variableId(), id, defType(t));
    c.requireSubType(e, t, error(e, "El valor es de tipo %t pero la anotacion dice %t", e, t));
    collect(t, e, c);
}

// ---------- Asignacion: x = expresion ----------
// "x = e" como sentencia no se puede escribir como patron concreto (es ambiguo con la igualdad),
// asi que se detecta por la etiqueta de la produccion (esAsignacion, definida en Parser.rsc)
// y se sacan sus partes del arbol: args = [variable, layout, "=", layout, expresion].
// Regla: la variable debe existir y el valor debe ser subtipo del tipo de la variable.
void recolectarSentencia(Sentencia sentencia, Collector c) {
    if (esAsignacion(sentencia)) {
        list[Tree] partes = sentencia.args;
        if (Identificador variable := partes[0], Exp valor := partes[4]) {
            c.use(variable, {variableId()});
            c.requireSubType(valor, variable, error(sentencia, "No se puede asignar un valor de tipo %t a la variable <variable>, que es de tipo %t", valor, variable));
            collect(valor, c);
            return;
        }
    }
    recolectar(sentencia, c);
}

// ---------- for ----------
// El lenguaje no define tipos de coleccion, asi que las variables del
// patron quedan con tipo tipoError() (compatible con todo).

// Variables que declara el patron de un for. El lenguaje no define colecciones,
// asi que quedan con tipoError (compatible con todo).
void definirPatron(Patron p, Collector c) {
    if ((Patron) `<Identificador i>` := p) {
        c.define("<i>", variableId(), i, defType(tipoError()));
    } else if ((Patron) `( <Identificador a> , <{Identificador ","}+ resto> )` := p) {
        c.define("<a>", variableId(), a, defType(tipoError()));
        for (Identificador r <- resto) {
            c.define("<r>", variableId(), r, defType(tipoError()));
        }
    }
}

// for: ambito propio con las variables del patron.
void collect(current: (SentenciaFor) `for ( <ClausulaFor cl> ) <Sentencia cuerpo>`, Collector c) {
    c.enterScope(current);
        if ((ClausulaFor) `<Patron p> \<- <Exp col>` := cl) {
            definirPatron(p, c);
            collect(col, c);
        } else if ((ClausulaFor) `<Identificador i> in <Exp col>` := cl) {
            c.define("<i>", variableId(), i, defType(tipoError()));
            collect(col, c);
        }
        recolectarSentencia(cuerpo, c);
    c.leaveScope(current);
}

// ---------- Literales ----------

// ---------- Literales ----------
void collect(current: (Exp) `<Dato d>`, Collector c) {
    c.fact(current, d);
    collect(d, c);
}

void collect(current: (Dato) `<Entero _>`, Collector c)   { c.fact(current, tipoInt()); }
void collect(current: (Dato) `<Flotante _>`, Collector c) { c.fact(current, tipoFloat()); }
void collect(current: (Dato) `<Booleano _>`, Collector c) { c.fact(current, tipoBoolean()); }
void collect(current: (Dato) `<Caracter _>`, Collector c) { c.fact(current, tipoChar()); }

void collect(current: (Dato) `<Cadena s>`, Collector c) {
    c.fact(current, tipoString());
    interpolaciones(s, c);
}

// Las expresiones dentro de "${...}" y los "$nombre" se verifican como cualquier otra expresion.
// Las expresiones dentro de "${...}" y los "$nombre" se revisan como cualquier otra expresion.
void interpolaciones(Cadena s, Collector c) {
    top-down-break visit (s) {
        case Interpolacion i: {
            if (/Exp e := i) {
                collect(e, c);
            } else if (/Identificador id := i) {
                c.use(id, {variableId()});
            }
        }
    }
}

// ---------- Variables, agrupacion, if ----------

// ---------- Variables, parentesis, if ----------
// Uso de una variable: TypePal busca su declaracion (si no existe, "Undefined variable").
void collect(current: (Exp) `<Identificador id>`, Collector c) {
    c.use(id, {variableId()});
}

void collect(current: (Exp) `( <Exp e> )`, Collector c) {
    c.fact(current, e);
    collect(e, c);
}

// if: la condicion debe ser Boolean y el tipo del if es el ancestro comun de las dos ramas.
void collect(current: (Exp) `if <Exp condicion> then <Exp entonces> else <Exp sino>`, Collector c) {
    c.calculate("if", current, [condicion, entonces, sino],
        AType(Solver s) {
            AType tc = s.getType(condicion);
            if (tc != tipoBoolean() && tc != tipoError()) {
                s.report(error(condicion, "La condicion del if debe ser Boolean, pero es %t", condicion));
            }
            AType tt = s.getType(entonces);
            AType tf = s.getType(sino);
            AType comun = ancestroComun(tt, tf);
            if (comun == tipoError() && tt != tipoError() && tf != tipoError()) {
                s.report(error(current, "Las dos ramas del if no tienen un tipo en comun: son %t y %t", entonces, sino));
            }
            return comun;
        });
    collect(condicion, entonces, sino, c);
}

// ---------- Llamadas ----------

// ---------- Llamadas ----------
// Saca la lista de argumentos de "( a, b, c )" (vacia si es "()").
list[Exp] argumentosDe(ArgsParen a) {
    if ((ArgsParen) `( <Argumentos? xs> )` := a) {
        for (Argumentos ar <- xs) {
            if ((Argumentos) `<{Exp ","}+ es>` := ar) {
                return [e | Exp e <- es];
            }
        }
    }
    return [];
}

// Compara cantidad y tipos de los argumentos reales contra los parametros de la firma.
void verificarArgumentos(Tree current, str nombre, list[AType] tiposRecibidos, list[AType] tiposEsperados, Solver s) {
    if (size(tiposRecibidos) != size(tiposEsperados)) {
        s.report(error(current, "<nombre> espera <size(tiposEsperados)> argumento(s), pero recibio <size(tiposRecibidos)>"));
        return;
    }
    for (int i <- [0..size(tiposRecibidos)]) {
        if (!subtipo(tiposRecibidos[i], tiposEsperados[i])) {
            s.report(error(current, "El argumento <i + 1> de <nombre> es de tipo %t, pero se esperaba %t", tiposRecibidos[i], tiposEsperados[i]));
        }
    }
}

// Tipo de f(args): busca f entre las funciones y, si no, entre los metodos del objeto actual.
// Si no existe: error de existencia.
AType resultadoLlamada(Tree current, str nombre, list[AType] tiposArgs, str contexto, Solver s) {
    if (nombre in tFunciones) {
        InfoMetodo im = tFunciones[nombre];
        verificarArgumentos(current, nombre, tiposArgs, im.tiposParams, s);
        return im.tipoRetorno;
    }
    if (contexto != "" && contexto in tParametrosTipo) {
        AType yo = tipoObjeto(contexto, [tipoParametro(p) | str p <- tParametrosTipo[contexto]]);
        tuple[bool, list[AType], AType] r = buscarMetodo(yo, nombre);
        if (r[0]) {
            verificarArgumentos(current, nombre, tiposArgs, r[1], s);
            return r[2];
        }
    }
    s.report(error(current, "[Existencia] La funcion o metodo <nombre> no esta definido"));
    return tipoError();
}

// Llamada f(args). print/println valen lo mismo que su argumento (asi un metodo puede terminar en print).
void collect(current: (Exp) `<Identificador f> <ArgsParen a>`, Collector c) {
    str nombre = "<f>";
    list[Exp] args = argumentosDe(a);
    if (nombre == "print" || nombre == "println") {
        // print(x) vale lo mismo que x (asi un metodo puede terminar en print, como en snippet2)
        c.calculate("print", current, args,
            AType(Solver s) {
                list[AType] ts = [s.getType(x) | Exp x <- args];
                return size(ts) == 1 ? ts[0] : tipoUnit();
            });
    } else if (nombre == "range" || nombre == "List") {
        // funciones predefinidas: el lenguaje no define tipos de coleccion, asi que su resultado no se restringe
        c.fact(current, tipoError());
    } else {
        str contexto = objetoActual;
        c.calculate("llamada <nombre>", current, args,
            AType(Solver s) {
                list[AType] tiposArgs = [s.getType(x) | Exp x <- args];
                return resultadoLlamada(current, nombre, tiposArgs, contexto, s);
            });
    }
    for (Exp e <- args) {
        collect(e, c);
    }
}

// ---------- Acceso a campos y metodos: expresion.nombre / expresion.nombre(args) ----------

// ---------- Acceso: expresion.campo  /  expresion.metodo(args)  /  Objeto.metodo(args) ----------
// Se busca el miembro en el tipo del receptor (y sus ancestros). getClass y getSimpleName se tratan aparte.
void collect(current: (Exp) `<Exp e> . <Identificador f> <ArgsParen? a>`, Collector c) {
    str nombre = "<f>";
    bool conParentesis = false;
    list[Exp] args = [];
    for (ArgsParen ap <- a) {
        conParentesis = true;
        args = argumentosDe(ap);
    }
    // Objeto.metodo(...): el receptor es el nombre de un objeto, no una variable
    str receptorObjeto = "";
    if ((Exp) `<Identificador r>` := e && "<r>" in tParametrosTipo) {
        receptorObjeto = "<r>";
    }
    list[Tree] deps = receptorObjeto == "" ? [e] + args : args;
    c.calculate("acceso <nombre>", current, deps,
        AType(Solver s) {
            AType te = receptorObjeto != ""
                ? tipoObjeto(receptorObjeto, [tipoParametro(p) | str p <- tParametrosTipo[receptorObjeto]])
                : s.getType(e);
            list[AType] tiposArgs = [s.getType(x) | Exp x <- args];
            if (te == tipoError()) return tipoError();
            if (te == tipoClase()) {
                if (nombre == "getSimpleName" || nombre == "getName") return tipoString();
                s.report(error(current, "Class solo tiene getSimpleName y getName, no <nombre>"));
                return tipoError();
            }
            if (tipoObjeto(_, _) := te) {
                if (conParentesis) {
                    tuple[bool, list[AType], AType] m = buscarMetodo(te, nombre);
                    if (!m[0]) {
                        if (nombre == "getClass") return tipoClase();
                        s.report(error(current, "[Existencia] El objeto %t no tiene un metodo <nombre>", te));
                        return tipoError();
                    }
                    verificarArgumentos(current, nombre, tiposArgs, m[1], s);
                    return m[2];
                }
                tuple[bool, AType] cp = buscarCampo(te, nombre);
                if (cp[0]) return cp[1];
                if (nombre == "getClass") return tipoClase();
                s.report(error(current, "[Existencia] El objeto %t no tiene un campo <nombre>", te));
                return tipoError();
            }
            s.report(error(current, "No se puede acceder a <nombre> en un valor de tipo %t", te));
            return tipoError();
        });
    if (receptorObjeto == "") {
        collect(e, c);
    }
    for (Exp x <- args) {
        collect(x, c);
    }
}

// ---------- new: new T(args) { override ... } ----------

// Inferencia: compara el tipo del constructor (con parametros, ej. A) con el tipo real recibido (ej. Dog)
// y va guardando A -> Dog. Si A aparece dos veces, se toma el ancestro comun.
map[str, AType] unificar(AType patron, AType tipoReal, map[str, AType] en) {
    if (tipoParametro(str p) := patron) {
        if (p in en) {
            en[p] = ancestroComun(en[p], tipoReal);
        } else {
            en[p] = tipoReal;
        }
        return en;
    }
    if (tipoObjeto(str n1, list[AType] a1) := patron && tipoObjeto(str n2, list[AType] a2) := tipoReal && n1 == n2 && size(a1) == size(a2)) {
        for (int i <- [0..size(a1)]) {
            en = unificar(a1[i], a2[i], en);
        }
    }
    return en;
}

// Tipo de "new T(args) { override ... }":
//  1) cantidad y tipos de los argumentos contra el constructor;
//  2) si T es generico y no trae argumentos de tipo, se infieren (new List(new Dog) -> List[Dog]);
//  3) cada override debe existir en el objeto y coincidir con la firma del metodo.
AType resultadoNuevo(Tree current, AType tipo, list[AType] tiposArgs,
                   list[tuple[str, list[AType], AType, Tree]] overrides, Solver s) {
    if (tipo == tipoError()) return tipoError();
    if (tipoObjeto(str n, list[AType] argsDeclarados) := tipo) {
        list[str] tiposParam = tParametrosTipo[n];
        list[AType] tiposCtor = tConstructor[n];

        if (size(tiposArgs) != size(tiposCtor)) {
            s.report(error(current, "El constructor de <n> espera <size(tiposCtor)> argumento(s), pero recibio <size(tiposArgs)>"));
            return tipo;
        }

        // inferencia de los argumentos de tipo: new Lista(new Dog) -> Lista[Dog]
        if (size(tiposParam) > 0 && size(argsDeclarados) == 0) {
            map[str, AType] enlacesTipo = ();
            for (int k <- [0..size(tiposCtor)]) {
                enlacesTipo = unificar(tiposCtor[k], tiposArgs[k], enlacesTipo);
            }
            list[AType] tiposInferidos = [];
            for (str p <- tiposParam) {
                if (p in enlacesTipo) {
                    tiposInferidos += [enlacesTipo[p]];
                } else {
                    s.report(error(current, "No se puede inferir el argumento de tipo <p> de <n>; escribelo explicitamente: <n>[...]"));
                    tiposInferidos += [tipoError()];
                }
            }
            argsDeclarados = tiposInferidos;
            tipo = tipoObjeto(n, argsDeclarados);
        }

        for (int q <- [0..size(tiposCtor)]) {
            AType esperado = sustituir(tiposCtor[q], tiposParam, argsDeclarados);
            if (!subtipo(tiposArgs[q], esperado)) {
                s.report(error(current, "El argumento <q + 1> del constructor de <n> es de tipo %t, pero se esperaba %t", tiposArgs[q], esperado));
            }
        }

        for (<str mn, list[AType] ps, AType rt, Tree src> <- overrides) {
            tuple[bool, list[AType], AType] m = buscarMetodo(tipo, mn);
            if (!m[0]) {
                s.report(error(src, "[Existencia] No se puede hacer override de <mn>: el objeto %t no tiene ese metodo", tipo));
            } else {
                if (size(ps) != size(m[1])) {
                    s.report(error(src, "El override de <mn> debe tener <size(m[1])> parametro(s)"));
                } else {
                    for (int j <- [0..size(ps)]) {
                        if (!subtipo(ps[j], m[1][j]) || !subtipo(m[1][j], ps[j])) {
                            s.report(error(src, "El parametro <j + 1> del override de <mn> es %t, pero el metodo declara %t", ps[j], m[1][j]));
                        }
                    }
                }
                if (!subtipo(rt, m[2])) {
                    s.report(error(src, "El override de <mn> devuelve %t, pero el metodo declara %t", rt, m[2]));
                }
            }
        }
        return tipo;
    }
    s.report(error(current, "No se puede crear una instancia de %t con new", tipo));
    return tipoError();
}

// new T(args) { override ... }: se revisa cada override en su propio ambito.
void collect(current: (Exp) `new <Tipo t> <ArgsParen? a> <CuerpoObjeto? cuerpo>`, Collector c) {
    AType tipoDelNew = tipoAType(t, tiposParamActuales, c, true, true);
    list[Exp] args = [];
    for (ArgsParen ap <- a) {
        args = argumentosDe(ap);
    }

    list[tuple[str, list[AType], AType, Tree]] overrides = [];
    for ((CuerpoObjeto) `{ <MetodoOverride* ms> }` <- cuerpo) {
        for (MetodoOverride mo <- ms) {
            if ((MetodoOverride) `override <Identificador m> <Parametros ps> <AnotTipo? ret> = <CuerpoMetodo cb>` := mo) {
                InfoMetodo info = infoDeMetodo([ps], [r | AnotTipo r <- ret], tiposParamActuales, c);
                overrides += [<"<m>", info.tiposParams, info.tipoRetorno, m>];
                c.enterScope(mo);
                    definirParametros([ps], info.tiposParams, c);
                    collect(cb, c);
                    chequearRetorno(cb, info.tipoRetorno, c);
                c.leaveScope(mo);
            }
        }
    }

    c.calculate("new", current, args,
        AType(Solver s) {
            list[AType] tiposArgs = [s.getType(x) | Exp x <- args];
            return resultadoNuevo(current, tipoDelNew, tiposArgs, overrides, s);
        });
    for (Exp x <- args) {
        collect(x, c);
    }
}

// ---------- Operadores ----------

// ---------- Operadores ----------
bool esNumerico(AType t) = t == tipoInt() || t == tipoFloat();
bool hayError(AType a, AType b) = a == tipoError() || b == tipoError();

AType resultadoNumerico(AType l, AType r) = (l == tipoFloat() || r == tipoFloat()) ? tipoFloat() : tipoInt();

// Aritmetica: dos numeros (Int/Float); mezclar Int y Float da Float. soloEntero = true para % (solo Int).
void operadorNumerico(Tree current, Tree l, Tree r, str nombre, bool soloEntero, Collector c) {
    c.calculate("operador <nombre>", current, [l, r],
        AType(Solver s) {
            AType tl = s.getType(l);
            AType tr = s.getType(r);
            if (hayError(tl, tr)) return tipoError();
            if (soloEntero) {
                if (tl == tipoInt() && tr == tipoInt()) return tipoInt();
                s.report(error(current, "El operador <nombre> requiere dos Int, pero recibio %t y %t", l, r));
                return tipoInt();
            }
            if (esNumerico(tl) && esNumerico(tr)) return resultadoNumerico(tl, tr);
            s.report(error(current, "El operador <nombre> requiere numeros, pero recibio %t y %t", l, r));
            return tipoInt();
        });
    collect(l, r, c);
}

void collect(current: (Exp) `<Exp l> ** <Exp r>`, Collector c) { operadorNumerico(current, l, r, "potencia", false, c); }
void collect(current: (Exp) `<Exp l> * <Exp r>`, Collector c)  { operadorNumerico(current, l, r, "multiplicacion", false, c); }
void collect(current: (Exp) `<Exp l> / <Exp r>`, Collector c)  { operadorNumerico(current, l, r, "division", false, c); }
void collect(current: (Exp) `<Exp l> % <Exp r>`, Collector c)  { operadorNumerico(current, l, r, "modulo", true, c); }
void collect(current: (Exp) `<Exp l> - <Exp r>`, Collector c)  { operadorNumerico(current, l, r, "resta", false, c); }

// La suma ademas concatena dos String.
// La suma ademas concatena dos String.
void collect(current: (Exp) `<Exp l> + <Exp r>`, Collector c) {
    c.calculate("operador suma", current, [l, r],
        AType(Solver s) {
            AType tl = s.getType(l);
            AType tr = s.getType(r);
            if (hayError(tl, tr)) return tipoError();
            if (esNumerico(tl) && esNumerico(tr)) return resultadoNumerico(tl, tr);
            if (tl == tipoString() && tr == tipoString()) return tipoString();
            s.report(error(current, "El operador suma requiere dos numeros o dos String, pero recibio %t y %t", l, r));
            return tipoInt();
        });
    collect(l, r, c);
}

void collect(current: (Exp) `- <Exp e>`, Collector c) {
    c.calculate("menos unario", current, [e],
        AType(Solver s) {
            AType t = s.getType(e);
            if (t == tipoError() || esNumerico(t)) return t;
            s.report(error(current, "El menos unario requiere un numero, pero recibio %t", e));
            return tipoInt();
        });
    collect(e, c);
}

void collect(current: (Exp) `<Negacion _> <Exp e>`, Collector c) {
    c.calculate("neg", current, [e],
        AType(Solver s) {
            AType t = s.getType(e);
            if (t != tipoBoolean() && t != tipoError()) {
                s.report(error(current, "neg requiere un Boolean, pero recibio %t", e));
            }
            return tipoBoolean();
        });
    collect(e, c);
}

// < > <= >=: dos numeros o dos Char; el resultado es Boolean.
void comparacion(Tree current, Tree l, Tree r, str nombre, Collector c) {
    c.calculate("comparacion <nombre>", current, [l, r],
        AType(Solver s) {
            AType tl = s.getType(l);
            AType tr = s.getType(r);
            if (!hayError(tl, tr) && !((esNumerico(tl) && esNumerico(tr)) || (tl == tipoChar() && tr == tipoChar()))) {
                s.report(error(current, "La comparacion <nombre> requiere dos numeros o dos Char, pero recibio %t y %t", l, r));
            }
            return tipoBoolean();
        });
    collect(l, r, c);
}

void collect(current: (Exp) `<Exp l> \< <Exp r>`, Collector c)  { comparacion(current, l, r, "menor que", c); }
void collect(current: (Exp) `<Exp l> \> <Exp r>`, Collector c)  { comparacion(current, l, r, "mayor que", c); }
void collect(current: (Exp) `<Exp l> \<= <Exp r>`, Collector c) { comparacion(current, l, r, "menor o igual", c); }
void collect(current: (Exp) `<Exp l> \>= <Exp r>`, Collector c) { comparacion(current, l, r, "mayor o igual", c); }

// = y <>: los tipos deben ser comparables (uno subtipo del otro, o ambos numericos).
void igualdad(Tree current, Tree l, Tree r, str nombre, Collector c) {
    c.calculate("igualdad <nombre>", current, [l, r],
        AType(Solver s) {
            AType tl = s.getType(l);
            AType tr = s.getType(r);
            if (!hayError(tl, tr) && !(subtipo(tl, tr) || subtipo(tr, tl) || (esNumerico(tl) && esNumerico(tr)))) {
                s.report(error(current, "No se puede comparar <nombre> un %t con un %t", l, r));
            }
            return tipoBoolean();
        });
    collect(l, r, c);
}

void collect(current: (Exp) `<Exp l> = <Exp r>`, Collector c)      { igualdad(current, l, r, "por igualdad", c); }
void collect(current: (Exp) `<Exp l> \<\> <Exp r>`, Collector c)   { igualdad(current, l, r, "por desigualdad", c); }

// and / or: ambos lados Boolean.
void logico(Tree current, Tree l, Tree r, str nombre, Collector c) {
    c.calculate("operador <nombre>", current, [l, r],
        AType(Solver s) {
            AType tl = s.getType(l);
            AType tr = s.getType(r);
            if ((tl != tipoBoolean() && tl != tipoError()) || (tr != tipoBoolean() && tr != tipoError())) {
                s.report(error(current, "El operador <nombre> requiere Boolean en ambos lados, pero recibio %t y %t", l, r));
            }
            return tipoBoolean();
        });
    collect(l, r, c);
}

void collect(current: (Exp) `<Exp l> and <Exp r>`, Collector c) { logico(current, l, r, "and", c); }
void collect(current: (Exp) `<Exp l> or <Exp r>`, Collector c)  { logico(current, l, r, "or", c); }

// Operadores sin semantica definida en el enunciado: "in" da Boolean; "->" y "=>"
// se aceptan (con sus operandos verificados) y su resultado no se restringe.
// in, ->, => : el enunciado no les da semantica de tipos; se revisan los operandos y no se restringe el resultado.
void operadorLibre(Tree current, Tree l, Tree r, AType resultado, Collector c) {
    c.calculate("operador libre", current, [l, r],
        AType(Solver s) {
            s.getType(l);
            s.getType(r);
            return resultado;
        });
    collect(l, r, c);
}

void collect(current: (Exp) `<Exp l> in <Exp r>`, Collector c)    { operadorLibre(current, l, r, tipoBoolean(), c); }
void collect(current: (Exp) `<Exp l> -\> <Exp r>`, Collector c)   { operadorLibre(current, l, r, tipoError(), c); }
void collect(current: (Exp) `<Exp l> =\> <Exp r>`, Collector c)   { operadorLibre(current, l, r, tipoError(), c); }

// ---------- Punto de entrada ----------

// ---------- Punto de entrada ----------
// Ejecuta TypePal sobre el arbol: recolecta, resuelve y devuelve el modelo con los mensajes.
TModel verificarArbol(Tree arbol)
    = collectAndSolve(arbol, config = configuracion(), modelName = "objectilang");

// Uso: validar(|project://entrega2-ple/test/objetos_ok.ol|);
// Uso: validar(|project://entrega2-ple/test/objetos_ok.ol|);
// Parsea el archivo, verifica tipos y muestra "OK" o la lista de errores ordenada por linea.
// Verifica el programa, imprime el resultado y devuelve true si no hay errores de tipos.
bool verificar(loc archivo) {
    Tree arbol = parsearArchivo(archivo);
    TModel tm = verificarArbol(arbol);
    list[Message] errores = [m | Message m <- tm.messages, m is error];
    errores = sort(errores, bool(Message a, Message b) { return a.at.begin.line < b.at.begin.line; });
    if (errores == []) {
        println("OK: el programa pasa la verificacion de tipos");
        return true;
    }
    println("<size(errores)> error(es):");
    for (Message m <- errores) {
        println("  linea <m.at.begin.line>: <m.msg>");
    }
    return false;
}

void validar(loc archivo) {
    verificar(archivo);
}
