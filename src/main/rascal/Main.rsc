module Main

import IO;
import Exception;
import AST;
import ASTBuilder;
import Checker;
import Interpreter;

// Punto de entrada: lee un archivo .ol, verifica sus tipos y, si no hay errores,
// lo ejecuta y muestra el resultado en la consola.
//
// Uso desde la terminal de Rascal:
//   import Main;
//   main();                                                  // usa el archivo por defecto
//   main(archivo=|project://entrega2-ple/test/prueba.ol|);
int main(loc archivo=|project://entrega2-ple/instance/snippet1.obl|) {
    try {
        if (!verificar(archivo)) return 1;
        ejecutar(archivo);
        return 0;
    } catch ParseError(loc donde): {
        println("Error de sintaxis en la linea <donde.begin.line>, columna <donde.begin.column>");
        return 1;
    } catch Ambiguity(_, _, _): {
        println("Error de sintaxis: el programa es ambiguo");
        return 1;
    } catch value error: {
        println("Error de ejecucion: <error>");
        return 1;
    }
}

// Imprime el AST de un archivo (para depurar el parser).
void mostrarAST(loc archivo=|project://entrega2-ple/instance/snippet1.obl|) {
    iprintln(construirAST(archivo));
}
