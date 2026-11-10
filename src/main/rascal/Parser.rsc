module Parser

import ParseTree;
import Syntax;
import Exception;

// Parsea un archivo .ol y devuelve su arbol concreto.
//
// "x = 2" como sentencia es ambiguo en la gramatica: puede ser una asignacion (sentAsig)
// o una comparacion de igualdad (sentExp con igual). Se parsea permitiendo esa ambiguedad
// y, donde ocurre, se elige la asignacion. Cualquier OTRA ambiguedad sigue siendo un error.
Tree parsearArchivo(loc archivo) {
    start[Programa] pt = parse(#start[Programa], archivo, allowAmbiguity=true);
    return validarYResolver(pt.top, archivo);
}

// Parsea codigo fuente que ya esta en memoria (por ejemplo, el str generado
// por CodeGenerator) con la misma politica de ambiguedades que un archivo.
Tree parsearTexto(str texto) {
    start[Programa] pt = parse(#start[Programa], texto, allowAmbiguity=true);
    return validarYResolver(pt.top, |unknown:///|);
}


Tree validarYResolver(Tree arbol, loc origen) {
    Tree resuelto = preferirAsignacion(arbol);
    if (amb(_) := resuelto) {
        throw Ambiguity(origen, "Programa", "ambiguedad no resuelta");
    }
    return resuelto;
}

// Reemplaza cada ambiguedad que incluya una asignacion por esa asignacion.
Tree preferirAsignacion(Tree arbol) = bottom-up visit (arbol) {
    case amb(set[Tree] alternativas) => elegida
        when Tree elegida <- alternativas, esAsignacion(elegida)
};

bool esAsignacion(Tree t) = appl(prod(label("sentAsig", _), _, _), _) := t;

// Parsea una expresion suelta (se usa para las interpolaciones ${...} de las cadenas).
Tree parsearExp(str texto) = parse(#Exp, texto);

Tree parsearOverride(str texto) = parse(#MetodoOverride, texto);
Tree parsearCuerpo(str texto) = parse(#CuerpoMetodo, texto);
