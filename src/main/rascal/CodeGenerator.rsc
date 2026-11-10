module CodeGenerator

import AST;
import ASTBuilder;
import List;

// Covierte un dato del AST en texto
str generarDato(Dato valor) {
    switch (valor) {    
        case dEntero(e):
            return "<e>";
        case dFlotante(f):
            return "<f>";
        case dBooleano(b):
            return "<b>";
        case dCaracter(c):
            return "\'" + c + "\'";
        case dCadena(c):
            return "\"" + c + "\"";

        default:
            return "Error";
    }
}

// Convierte una expresión del AST en texto
str generarExp(Exp expresion) {
    switch (expresion) {
        case dato(valor):
            return generarDato(valor);

        case variable(nombre):
            return nombre;

        case llamada(nombre, argumentos):
            return nombre + generarArgs(argumentos);

        case agrupada(exp):
            return "(<generarExp(exp)>)";

        case negacion(operador, exp):
            return "<operador> <generarExp(exp)>";

        case menos(exp):
            return "-<generarExp(exp)>";

        case potencia(izq, der):
            return "(<generarExp(izq)> ** <generarExp(der)>)";

        case mult(izq, der):
            return "(<generarExp(izq)> * <generarExp(der)>)";

        case division(izq, der):
            return "(<generarExp(izq)> / <generarExp(der)>)";

        case modulo(izq, der):
            return "(<generarExp(izq)> % <generarExp(der)>)";

        case suma(izq, der):
            return "(<generarExp(izq)> + <generarExp(der)>)";

        case resta(izq, der):
            return "(<generarExp(izq)> - <generarExp(der)>)";

        case menorQue(izq, der):
            return "(<generarExp(izq)> \< <generarExp(der)>)";

        case mayorQue(izq, der):
            return "(<generarExp(izq)> \> <generarExp(der)>)";

        case menorIgual(izq, der):
            return "(<generarExp(izq)> \<= <generarExp(der)>)";

        case mayorIgual(izq, der):
            return "(<generarExp(izq)> \>= <generarExp(der)>)";

        case distinto(izq, der):
            return "(<generarExp(izq)> \<\> <generarExp(der)>)";

        case igual(izq, der):
            return "(<generarExp(izq)> = <generarExp(der)>)";

        case conY(izq, der):
            return "(<generarExp(izq)> and <generarExp(der)>)";

        case conO(izq, der):
            return "(<generarExp(izq)> or <generarExp(der)>)";

        case acceso(exp, nombre, llamadaParem):
            return generarExp(exp) + "." + nombre + (size(llamadaParem) > 0 ? generarArgs(llamadaParem[0]) : ""); // como en TypeScript el ? se usa para verificar que llamadaParem es mayor a 0, si lo es llama a generarArgs, si no solo usa "".

        case expSi(condicion, entonces, sino):
            return "(if "+ generarExp(condicion) + " then " + generarExp(entonces) + " else " + generarExp(sino) + ")";

        case expNuevo(tipo, argumentos, cuerpoNuevo):
            return "new " + generarTipo(tipo) + (size(argumentos) > 0 ? generarArgs(argumentos[0]) : "") + (size(cuerpoNuevo) > 0 ? generarCuerpoNuevo(cuerpoNuevo[0]) : ""); // mismo uso que en caso del acceso.
        
        case pertenece(izq, der):
            return "(" + generarExp(izq) + " in " + generarExp(der) + ")";
        
        case flecha(izq, der):
            return "(" + generarExp(izq) + " -\> " + generarExp(der) + ")";

        case flechaDoble(izq, der):
            return "(" + generarExp(izq) + " =\> " + generarExp(der) + ")";

        default:
            return "Error";
    }
}

str generarTipo(Tipo tipo) {
    switch (tipo) {
        case simple(nombre):
            return nombre;

        case generico(nombre, argumentosTipo):
            return nombre + "[" + intercalate(", ", [generarTipo(t) | t <- argumentosTipo]) + "]";

        default:
            return "Error";
    }
}

str generarAnotTipo(AnotTipo anotacion) {
    switch (anotacion) {
        case anotTipo(tipo):
            return ": "+ generarTipo(tipo);
        
        default:
            return "Error";
    }
}

str generarArgumentos(Argumentos argumento) {
    switch (argumento) {
        case argumentos(lista):
            return intercalate(", ", [generarExp(exp) | exp <- lista]);
        
        default:
            return "Error";
    }
}

str generarArgs(ArgsParen argumentos) {
    switch (argumentos) {
        case argsParen(lista):
            return "(" + intercalate(", ", [generarArgumentos(arg) | arg <- lista]) + ")"; // Intercalate toma una lista de argumentos y los convierte en texto mientras los separa por comas

        default:
            return "Error";
    }
}

// Convierte una declaracion val o var en texto
str generarDeclaracion(DeclaracionVariable declaracion) {
    switch (declaracion) {
        case declVal(nombre, tipo, valor):
            return "val " + nombre + (size(tipo) > 0 ? generarAnotTipo(tipo[0]) : "") + " = " + generarExp(valor); // mismo uso que en caso del acceso con el valor tipo.
        case declVar(nombre, tipo, valor):
            return "var " + nombre + (size(tipo) > 0 ? generarAnotTipo(tipo[0]) : "") + " = " + generarExp(valor); // mismo uso que en caso del acceso con el valor tipo.

        default:
            return "Error";
    }
}

// Convierte una sentencia a texto
str generarSentencia(Sentencia sentencia) {
    switch (sentencia) {
        case sentDecl(declaracion):
            return generarDeclaracion(declaracion);
        case sentExp(expresion):
            return generarExp(expresion);
        case sentFor(ciclo):
            return generarFor(ciclo);
        case sentAsig(nombre, valor):
            return nombre + " = " + generarExp(valor);

        default:
            return "Error";
    }
}

// Convierte un ciclo for a texto
str generarPatron(Patron patron) {
    switch (patron) {
        case patronSimple(nombre):
            return nombre;
        case patronTupla(primero, resto):
            return "(" + primero + ", " + intercalate(", ", resto) + ")"; // intercalate es una funcion de List para separar por ", " los objetos de resto.

        default:
            return "Error";
    }
}

str generarClausulaFor(ClausulaFor clausulaFor) {
    switch (clausulaFor) {
        case clausula(patron, coleccion):
            return generarPatron(patron) + " \<- " + generarExp(coleccion);
        case clausulaIn(nombre, coleccion):
            return nombre + " in " + generarExp(coleccion);

        default:
            return "Error";
    }
}

str generarFor(SentenciaFor sentenciaFor) {
    switch (sentenciaFor) {
        case paraCada(clausula, cuerpo):
            return "for (" + generarClausulaFor(clausula) + ") " + generarSentencia(cuerpo);
        
        default:
            return "Error";
    }
}

// Convierte el cuerpo de un metodo en texto
str generarCuerpo(CuerpoMetodo cuerpoMetodo) {
    switch (cuerpoMetodo) {
        case cuerpo(sentencias):
            return "{\n" + intercalate(";\n", [generarSentencia(s) | s <- sentencias]) // mismo uso de intercalate separando los elementos por ; y un salto de linea
                    + (size(sentencias) > 0 ? ";\n" : "") + "}"; // mismo uso de verificar tamaño de sentencias, si es mayor a 0 se añade un ; con salto de linea, si no solo queda vacio.

        default:
            return "Error";
    }
}

// Convierte un parametro en texto
str generarParametro(Parametro parametro) {
    switch (parametro) {
        case parametro(nombre, tipo):
            return nombre + ": " + generarTipo(tipo);
        
        default:
            return "Error";
    }
}

// Convierte la lista de parametros a texto
str generarParametros(list[Parametros] parametrosLista) {
    list[str] resultado = [];

    for (Parametros parametros <- parametrosLista) { // parametro de parametrosLista
        switch (parametros) {
            case parametros(lista):
                resultado += [generarParametro(p) | p <- lista];

            default:
                resultado += ["Error"];
        }
    }

    return "(" + intercalate (", ", resultado) + ")";
}

// Convierte un metodo a texto
str generarMetodo(DefinicionMetodo definicion) {
    switch(definicion) {
        case metodo(nombre, params, retorno, cuerpoMetodo):
            return "method " + nombre + generarParametros(params) 
                    + (size(retorno) > 0 ? generarAnotTipo(retorno[0]) : "") + " " + generarCuerpo(cuerpoMetodo);

        default:
            return "Error";
    }
}

// Convierte un miembro de objeto en texto 
str generarMiembro(MiembroObjeto miembro) {
    switch (miembro) {
        case miembroDecl(declaracion):
            return generarDeclaracion(declaracion);
        case miembroMetodo(definicion):
            return generarMetodo(definicion);
        
        default:
            return "Error";
    }
}

// Convierte el cuerpo de un objeto en texto
str generarCuerpoObjeto(CuerpoDeObjeto cuerpoObjeto) {
    switch (cuerpoObjeto) {
        case cuerpoDeObjeto(miembros):
            return "{\n" +
                    intercalate("\n", [generarMiembro(m)  | m <- miembros]) // intercalade con un salto de linea, + generarMiembro de cada m de miembros.
                    + "\n}";

        default:
            return "Error";
    }
}

// Convierte parametro de tipo a texto 
str generarParametroTipo(ParametroTipo parametro) {
    switch (parametro) {
        case invariante(nombre):
            return nombre;
        case covariante(nombre):
            return "+" + nombre;
        case contravariante(nombre):
            return "-" + nombre;

        default:
            return "Error";
    }
}

str generarParametrosTipo(list[ParametrosTipo] parametrosLista) {
    list[str] resultado = [];

    for (ParametrosTipo parametros <- parametrosLista) {
        switch (parametros) {
            case tipos(lista):
                resultado += [generarParametroTipo(p) | p <- lista];

            default:
                resultado += ["Error"];
        }
    }

    return size(resultado) > 0 ? "[" + intercalate(", ", resultado) + "]" : "";
}

// Convierte una clausula extends en texto
str generarExtends(list[ClausulaExtends] herencia) {
     list[str] padres = [];

    for (ClausulaExtends clausula <- herencia) {
        switch (clausula) {
            case extiende(padre):
                padres += [generarTipo(padre)];

            default:
                padres += ["Error"];
        }
    }

    return size(padres) > 0 ? " extends " + intercalate(", ", padres) : "";
}

// Convierte un parametro del constructor en texto
str generarParametroConstructor(ParametroConstructor parametro) {
    switch (parametro) {
        case paramVal(nombre, tipo):
            return "val " + nombre + ": " + generarTipo(tipo);

        case paramVar(nombre, tipo):
            return "var " + nombre + ": " + generarTipo(tipo);

        default:
            return "Error";
    }
}

// Convierte los parametros del constructor en texto
str generarParametrosConstructor(list[ParametrosConstructor] parametrosLista) {
    list[str] resultado = [];

    for (ParametrosConstructor parametros <- parametrosLista) {
        switch (parametros) {
            case ctorParams(lista):
                resultado += [generarParametroConstructor(p) | p <- lista];

            default:
                resultado += ["Error"];
        }
    }

    return size(resultado) > 0 ? "(" + intercalate(", ", resultado) + ")" : "";
}

// Convierte un Objeto a texto
str generarObjeto(DefinicionObjeto definicion) {
    switch (definicion) {
        case objeto(nombre, parametrosTipo, herencia, constructor, cuerpoObj):
            return "object " + nombre
                + generarParametrosTipo(parametrosTipo)
                + generarExtends(herencia)
                + generarParametrosConstructor(constructor)
                + (size(cuerpoObj) > 0 ? " " + generarCuerpoObjeto(cuerpoObj[0]) : "");

        default:
            return "Error";
    }
}

// Convierte metodo override a texto
str generarMetodoOverride(MetodoOverride definicion) {
    switch (definicion) {
        case metodoOverride(nombre, params, retorno, cuerpoMetodo):
            return "override " + nombre
                + generarParametros([params]) 
                + (size(retorno) > 0 ? generarAnotTipo(retorno[0]) : "")
                + " = " + generarCuerpo(cuerpoMetodo);

        default:
            return "Error";
    }
}

// Convierte el nuevo cuerpo del override a texto
str generarCuerpoNuevo(CuerpoObjeto cuerpoObjeto) {
    switch (cuerpoObjeto) {
        case cuerpoObjeto(metodos):
            return "{\n"
                + intercalate("\n", [generarMetodoOverride(m) | m <- metodos])
                + "\n}";

        default:
            return "Error";
    }
}

str generarAsignacionMetodo(AsignacionMetodo definicion) {
    switch (definicion) {
        case asignacion(nombreObjeto, nombreMetodo, params, retorno, cuerpoMetodo):
            return nombreObjeto + "." + nombreMetodo + " = method"
                + generarParametros(params)
                + (size(retorno) > 0 ? generarAnotTipo(retorno[0]) : "")
                + (size(cuerpoMetodo) > 0 ? " " + generarCuerpo(cuerpoMetodo[0]) : "");

        default:
            return "Error";
    }
}

// Convierte una definicion a texto
str generarDefinicion(Definicion definicion) {
    switch (definicion) {
        case defObjeto(defObj):
            return generarObjeto(defObj);

        case defMetodo(defMet):
            return generarMetodo(defMet);

        case defAsignacion(defAsig):
            return generarAsignacionMetodo(defAsig);

        default:
            return "Error";
    }
}

// Convierte un programa a texto
str generarPrincipal(Principal principal) {
    switch (principal) {
        case principal(cuerpoMain):
            return "method main " + generarCuerpo(cuerpoMain);

        default:
            return "Error";
    }
}

str generarPrograma(Programa programa) {
    switch (programa) {
        case programa(antes, principal, despues):
            return intercalate("\n\n", 
                [generarDefinicion(d) | d <- antes] + 
                [generarPrincipal(principal)] + 
                [generarDefinicion(d) | d <- despues]);

        default:
            return "Error";
    }
}

// Funcion general de todo el programa
str generarDesdeArchivo(loc archivo) {
    Programa ast = construirAST(archivo);
    return generarPrograma(ast);
}
