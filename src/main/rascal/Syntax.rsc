module Syntax

// ---------- Layout (espacios y comentarios) ----------
layout Layout = WhitespaceAndComment* !>> [\ \t\n\r];
lexical WhitespaceAndComment = [\ \t\n\r] | @category="comment" "//" ![\n]* $;

// ---------- Identificadores y palabras reservadas ----------
keyword Palabras = "object" | "extends" | "method" | "val" | "var" | "new" | "override"
                  | "if" | "then" | "else" | "for" | "in"
                  | "true" | "false" | "and" | "or" | "neg" | "main";

lexical Identificador = ([a-zA-Z][a-zA-Z0-9]* !>> [a-zA-Z0-9]) \ Palabras;

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
lexical Interpolacion = "$" "{" Exp "}" | "$" Identificador;
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
syntax DefinicionObjeto
  = objeto: "object" Identificador ParametrosTipo? ClausulaExtends? ParametrosConstructor?
  ;

syntax ParametrosTipo = tipos: "[" {ParametroTipo ","}+ "]";
syntax ParametroTipo
  = invariante: Identificador
  | covariante: "+" Identificador
  | contravariante: "-" Identificador
  ;

syntax ClausulaExtends = extiende: "extends" Tipo;

syntax ParametrosConstructor = ctorParams: "(" {ParametroConstructor ","}+ ")";
syntax ParametroConstructor
  = paramVal: "val" Identificador ":" Tipo
  | paramVar: "var" Identificador ":" Tipo
  ;

// ---------- Métodos ----------
syntax DefinicionMetodo
  = metodo: "method" Identificador Parametros? (":" Tipo)? CuerpoMetodo
  ;

syntax AsignacionMetodo
  = asignacion: Identificador "." Identificador "=" "method" Parametros? (":" Tipo)? CuerpoMetodo?
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
  | sentExp: Exp
  | sentFor: SentenciaFor
  ;
syntax CuerpoMetodo = cuerpo: "{" {Sentencia ";"}* "}";

syntax SentenciaFor = paraCada: "for" "(" ClausulaFor ")" Sentencia;
syntax ClausulaFor = clausula: Patron "in" Exp;
syntax Patron
  = patronSimple: Identificador
  | patronTupla: "(" Identificador "," {Identificador ","}+ ")"
  ;

// ---------- Variables ----------
syntax DeclaracionVariable
  = declVal: "val" Identificador (":" Tipo)? "=" Exp
  | declVar: "var" Identificador (":" Tipo)? "=" Exp
  ;

// ---------- Expresiones (precedencia nativa de Rascal) ----------
syntax Exp
  = dato: Dato
  | llamada: Identificador Llamada*
  | agrupada: "(" Exp ")"
  | expSi: "if" Exp "then" Exp "else" Exp
  | expNuevo: "new" Tipo ("(" Argumentos? ")")? CuerpoObjeto?
  > negacion: "neg" Exp
  | menos: "-" Exp
  > right potencia: Exp "**" Exp
  > left ( mult: Exp "*" Exp
         | division: Exp "/" Exp
         | modulo: Exp "%" Exp
         )
  > left ( suma: Exp "+" Exp
         | resta: Exp "-" Exp
         )
  > non-assoc ( menorQue: Exp '\<' Exp
              | mayorQue: Exp '\>' Exp
              | menorIgual: Exp '\<=' Exp
              | mayorIgual: Exp '\>=' Exp
              | distinto: Exp '\<\>' Exp
              | igual: Exp '=' Exp
              )
  > left conY: Exp "and" Exp
  > left conO: Exp "or" Exp
  ;

syntax Llamada
  = conArgumentos: "(" Argumentos? ")"
  | conPunto: "." Identificador ("(" Argumentos? ")")?
  ;

syntax Argumentos = argumentos: {Exp ","}+;

syntax CuerpoObjeto = cuerpoObjeto: "{" MetodoOverride* "}";
syntax MetodoOverride = metodoOverride: "override" Identificador Parametros (":" Tipo)? "=" CuerpoMetodo;

// ---------- Punto de entrada ----------
syntax Principal = principal: "method" "main" CuerpoMetodo;

syntax Definicion
  = defObjeto: DefinicionObjeto
  | defMetodo: DefinicionMetodo
  | defAsignacion: AsignacionMetodo
  ;

start syntax Programa = programa: Definicion* Principal Definicion*;
