module JavaGenerator

import AST;
import ASTBuilder;
import List;

// 1. Generación de expresiones

str javaDato(Dato d) {
    switch (d) {
        case dEntero(n):
            return "<n>";

        case dFlotante(n):
            return "<n>";

        case dBooleano(b):
            return "<b>";

        case dCadena(s):
            return "\"" + s + "\"";

        case dCaracter(s):
            return "\'" + s + "\'";
    }
    return "null";
}

str javaArgumentos(Argumentos a) {
    switch (a) {
        case argumentos(lista):
            return intercalate(", ", [javaExp(e) | e <- lista]);
    }
    return "";
}

str javaArgs(ArgsParen a) {
    switch (a) {
        case argsParen(grupos):
            return "(" + intercalate(", ", [javaArgumentos(g) | g <- grupos]) + ")";
    }
    return "()";
}

str javaExp(Exp e) {
    switch (e) {
        case dato(d):
            return javaDato(d);

        case variable(nombre):
            return nombre;

        case agrupada(expresion):
            return "(" + javaExp(expresion) + ")";

        case llamada(nombre, argumentos):
            return nombre + javaArgs(argumentos);

        case acceso(expresion, nombre, llamadas):
            return javaExp(expresion) + "." + nombre
                + intercalate("", [javaArgs(a) | a <- llamadas]);

        case suma(izq, der):
            return "(" + javaExp(izq) + " + " + javaExp(der) + ")";

        case resta(izq, der):
            return "(" + javaExp(izq) + " - " + javaExp(der) + ")";

        case mult(izq, der):
            return "(" + javaExp(izq) + " * " + javaExp(der) + ")";

        case division(izq, der):
            return "(" + javaExp(izq) + " / " + javaExp(der) + ")";

        case modulo(izq, der):
            return "(" + javaExp(izq) + " % " + javaExp(der) + ")";

        case potencia(izq, der):
            return "Math.pow(" + javaExp(izq) + ", " + javaExp(der) + ")";

        case menos(expresion):
            return "(-" + javaExp(expresion) + ")";

        case negacion(operador, expresion):
            return "(!" + javaExp(expresion) + ")";

        case menorQue(izq, der):
            return "(" + javaExp(izq) + " \< " + javaExp(der) + ")";

        case mayorQue(izq, der):
            return "(" + javaExp(izq) + " \> " + javaExp(der) + ")";

        case menorIgual(izq, der):
            return "(" + javaExp(izq) + " \<= " + javaExp(der) + ")";

        case mayorIgual(izq, der):
            return "(" + javaExp(izq) + " \>= " + javaExp(der) + ")";

        case distinto(izq, der):
            return "(" + javaExp(izq) + " != " + javaExp(der) + ")";

        case igual(izq, der):
            return "(" + javaExp(izq) + " == " + javaExp(der) + ")";

        case conY(izq, der):
            return "(" + javaExp(izq) + " && " + javaExp(der) + ")";

        case conO(izq, der):
            return "(" + javaExp(izq) + " || " + javaExp(der) + ")";

        case expSi(condicion, entonces, sino):
            return "(" + javaExp(condicion) + " ? "
                + javaExp(entonces) + " : " + javaExp(sino) + ")";
    }

    return "null";
}


// --------------------------------------------------
// 2. Generación de sentencias
// --------------------------------------------------

str javaSentencia(Sentencia s) {
    switch (s) {
        case sentDecl(declVal(nombre, tipo, valor)):
            return "final var " + nombre + " = " + javaExp(valor) + ";";

        case sentDecl(declVar(nombre, tipo, valor)):
            return "var " + nombre + " = " + javaExp(valor) + ";";

        case sentAsig(nombre, valor):
            return nombre + " = " + javaExp(valor) + ";";

        case sentExp(llamada("print", argumentos)):
            return "System.out.print" + javaArgs(argumentos) + ";";

        case sentExp(llamada("println", argumentos)):
            return "System.out.println" + javaArgs(argumentos) + ";";

        case sentExp(expresion):
            return javaExp(expresion) + ";";
    }

    return "// Sentencia todavía no soportada";
}

str javaCuerpo(CuerpoMetodo c) {
    switch (c) {
        case cuerpo(sentencias):
            return intercalate(
                "\n",
                ["      " + javaSentencia(s) | s <- sentencias]
            );
    }

    return "";
}


// --------------------------------------------------
// 3. Generación del programa Java
// --------------------------------------------------

str generarJava(Programa p) {
    switch (p) {
        case programa(antes, principal(cuerpoMain), despues):
            return
                "public class ObjectilangProgram {\n"
                + "  public static void main(String[] args) {\n"
                + javaCuerpo(cuerpoMain) + "\n"
                + "  }\n"
                + "}\n";
    }

    return "";
}

// 4. Generación desde un archivo .ol

str generarJavaDesdeArchivo(loc archivo) {
    Programa ast = construirAST(archivo);
    return generarJava(ast);
}
