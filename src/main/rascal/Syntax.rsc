module Syntax

// ---------- Layout (espacios y comentarios) ----------
layout Layout = WhitespaceAndComment* !>> [\ \t\n\r];
lexical WhitespaceAndComment = [\ \t\n\r] | @category="comment" "//" ![\n]* $;

// ---------- Identificadores y palabras reservadas ----------
keyword Palabras = "object" | "extends" | "method" | "val" | "var" | "new" | "override"
                  | "if" | "then" | "else" | "for" | "in"
                  | "true" | "false" | "and" | "or" | "neg" | "main";

lexical Identificador = ([a-zA-Z][a-zA-Z0-9]* !>> [a-zA-Z0-9]) \ Palabras;
// Cambio - evidence 1.4: "neg" es palabra reservada, no se puede usar como identificador.
lexical Negacion = "neg" !>> [a-zA-Z0-9];

// ---------- Literales (con categoria para resaltado de sintaxis) ----------
// @category solo se aplica a UN simbolo (una referencia a otra regla),
// nunca a una secuencia de varios -- confirmado que eso genera errores
// (Ambiguous o Parse error, de forma inconsistente segun el caso).
lexical DigitosEntero = [0-9]+ !>> [0-9];
lexical Entero = @category="number" DigitosEntero;

lexical DigitosFlotante = [0-9]+ "." [0-9]+ !>> [0-9];
lexical Flotante = @category="number" DigitosFlotante;

lexical Booleano = @category="keyword" "true" | @category="keyword" "false";

lexical CaracterCadena = ![\"$];
// Cambio - evidence 1.4: "${x}" funcionaba pero "${ x }" fallaba porque no lleva layout.
lexical Interpolacion = "$" "{" Layout Exp Layout "}" | "$" Identificador;
lexical ContenidoCadena = CaracterCadena | Interpolacion;
lexical TextoCadena = [\"] ContenidoCadena* [\"];
lexical Cadena = @category="string" TextoCadena;

lexical CaracterSimple = ![\'];
lexical TextoCaracter = [\'] CaracterSimple [\'];
lexical Caracter = @category="string" TextoCaracter;

syntax Dato = dEntero: Entero | dFlotante: Flotante | dBooleano: Booleano | dCaracter: Caracter | dCadena: Cadena;

// ---------- Tipos ----------
syntax Tipo
  = simple: Identificador
  | generico: Identificador "[" {Tipo ","}+ "]"
  ;

// ---------- Objetos ----------
// Cambio - evidence 1.4: un objeto no puede declarar variables ni metodos 
syntax DefinicionObjeto
  = objeto: "object" !>> [a-zA-Z0-9] Identificador ParametrosTipo? ClausulaExtends? ParametrosConstructor? CuerpoDeObjeto?
  ;

syntax CuerpoDeObjeto = cuerpoDeObjeto: "{" MiembroObjeto* "}";
syntax MiembroObjeto
  = miembroDecl: DeclaracionVariable
  | miembroMetodo: DefinicionMetodo
  ;

syntax ParametrosTipo = tipos: "[" {ParametroTipo ","}+ "]";
syntax ParametroTipo
  = invariante: Identificador
  | covariante: "+" Identificador
  | contravariante: "-" Identificador
  ;

syntax ClausulaExtends = extiende: "extends" !>> [a-zA-Z0-9] Tipo;

syntax ParametrosConstructor = ctorParams: "(" {ParametroConstructor ","}+ ")";
syntax ParametroConstructor
  = paramVal: "val" !>> [a-zA-Z0-9] Identificador ":" Tipo
  | paramVar: "var" !>> [a-zA-Z0-9] Identificador ":" Tipo
  ;

// ---------- Métodos ----------
syntax DefinicionMetodo
  = metodo: "method" !>> [a-zA-Z0-9] Identificador Parametros? AnotTipo? CuerpoMetodo
  ;

syntax AsignacionMetodo
  = asignacion: Identificador "." Identificador "=" "method" !>> [a-zA-Z0-9] Parametros? AnotTipo? CuerpoMetodo?
  ;

syntax Parametros = parametros: "(" {Parametro ","}* ")";
syntax Parametro = parametro: Identificador ":" Tipo;

// Se necesita un separador real entre sentencias -- "print(...)" sin nada
// de por medio es ambiguo entre "una llamada" y "dos sentencias pegadas".
// Se probo primero exigir un salto de linea (como en los snippets del
// enunciado original), pero el separador de salto de linea entra en
// conflicto con el Layout automatico que Rascal inserta en todo syntax
// (ambos compiten por el mismo caracter \n en un parser scannerless).
// Punto y coma es la alternativa simple y confirmada que no tiene ese
// problema -- documentado como cambio real al lenguaje en el Task 2.
syntax Sentencia
  = sentDecl: DeclaracionVariable
  | sentAsig: Identificador "=" Exp
  | sentExp: Exp
  | sentFor: SentenciaFor
  ;
// Cambio - evidence 1.1: quitar ";" genera mas ambiguedades.
syntax CuerpoMetodo = cuerpo: "{" (Sentencia ";")* "}";


syntax SentenciaFor = paraCada: "for" !>> [a-zA-Z0-9] "(" ClausulaFor ")" Sentencia;
// Cambio - evidence 1.2: de acuerdo a la regla ForBody ::= Pattern "<-" Expression | IDENTIFIER "in" Expression
syntax ClausulaFor 
  = clausula: Patron "\<-" Exp
  | clausulaIn: Identificador "in" !>> [a-zA-Z0-9] Exp                 
  ;
syntax Patron
  = patronSimple: Identificador
  | patronTupla: "(" Identificador "," {Identificador ","}+ ")"
  ;

// ---------- Variables ----------
syntax DeclaracionVariable
  = declVal: "val" !>> [a-zA-Z0-9] Identificador AnotTipo? "=" Exp
  | declVar: "var" !>> [a-zA-Z0-9] Identificador AnotTipo? "=" Exp
  ;

// ---------- Expresiones (precedencia nativa de Rascal) ----------
syntax Exp
  = dato: Dato
  // Cambio - Se separan variables de llamadas y se exije un sufijo para una llamada
  | variable: Identificador
  | llamada: Identificador ArgsParen
  | acceso: Exp "." Identificador ArgsParen?
  | agrupada: "(" Exp ")"
  | expSi: "if" !>> [a-zA-Z0-9] Exp "then" !>> [a-zA-Z0-9] Exp "else" !>> [a-zA-Z0-9] Exp
  | expNuevo: "new" !>> [a-zA-Z0-9] Tipo ArgsParen? CuerpoObjeto?
  > negacion: Negacion Exp
  | menos: "-" Exp
  > right potencia: Exp "**" Exp
  > left ( mult: Exp "*" Exp
         | division: Exp "/" Exp
         | modulo: Exp "%" Exp
         )
  > left ( suma: Exp "+" Exp
         | resta: Exp "-" Exp
         )
  // Cambio - evidence 1.4: "in", "->" y "=>" son operadores validos de acuerdo al enunciado de la entrega 1, ->  y => no tiene definicion o ejemplo de uso
  > non-assoc ( menorQue: Exp '\<' Exp
              | mayorQue: Exp '\>' Exp
              | menorIgual: Exp '\<=' Exp
              | mayorIgual: Exp '\>=' Exp
              | distinto: Exp '\<\>' Exp
              | igual: Exp '=' Exp
              | pertenece: Exp "in" !>> [a-zA-Z0-9] Exp
              | flecha: Exp "-\>" Exp
              | flechaDoble: Exp "=\>" Exp
              )
  > left conY: Exp "and" !>> [a-zA-Z0-9] Exp
  > left conO: Exp "or" !>> [a-zA-Z0-9] Exp
  ;

syntax Llamada
  = conArgumentos: "(" Argumentos? ")"
  | conPunto: "." Identificador ArgsParen?
  ;

syntax Argumentos = argumentos: {Exp ","}+;

syntax ArgsParen = argsParen: "(" Argumentos? ")";
syntax AnotTipo = anotTipo: ":" Tipo;

syntax CuerpoObjeto = cuerpoObjeto: "{" MetodoOverride* "}";
syntax MetodoOverride = metodoOverride: "override" !>> [a-zA-Z0-9] Identificador Parametros AnotTipo? "=" CuerpoMetodo;

// ---------- Punto de entrada ----------
syntax Principal = principal: "method" !>> [a-zA-Z0-9] "main" !>> [a-zA-Z0-9] CuerpoMetodo;

syntax Definicion
  = defObjeto: DefinicionObjeto
  | defMetodo: DefinicionMetodo
  | defAsignacion: AsignacionMetodo
  ;

start syntax Programa = programa: Definicion* Principal Definicion*;
