module AST

// ---------- Programa ----------
data Programa
  = programa(list[Definicion] antes, Principal principal, list[Definicion] despues)
  ;

data Definicion 
  = defObjeto(DefinicionObjeto defObj)
  | defMetodo(DefinicionMetodo defMet)
  | defAsignacion(AsignacionMetodo defAsig)
  ;

data Principal
  = principal(CuerpoMetodo cuerpoMain)
  ;

// ----------- Literales ----------
data Dato
  = dEntero(int entero)
  | dFlotante(real flotante)
  | dBooleano(bool booleano)
  | dCaracter(str caracter)
  | dCadena(str cadena)
  ;

//----------- Tipos ----------
data Tipo
  = simple(str nombre)
  | generico(str nombre, list[Tipo] argumentosTipo)
  ;

// ---------- Objetos ----------
data DefinicionObjeto
  = objeto(str nombre,
           list[ParametrosTipo] parametrosDeTipo,
           list[ClausulaExtends] herencia,
           list[ParametrosConstructor] constructor)
  ;

data ParametrosTipo
  = tipos(list[ParametroTipo] lista)
  ;

data ParametroTipo
  = invariante(str nombre)
  | covariante(str nombre)
  | contravariante(str nombre)
  ;

data ClausulaExtends
  = extiende(Tipo padre)
  ;

data ParametrosConstructor
  = ctorParams(list[ParametroConstructor] lista)
  ;

data ParametroConstructor
  = paramVal(str nombre, Tipo tipo)
  | paramVar(str nombre, Tipo tipo)
  ;

// --------- Métodos ----------
data DefinicionMetodo
  = metodo(str nombre,
           list[Parametros] params,
           list[AnotTipo] retorno,
           CuerpoMetodo cuerpoMetodo)
  ;

data AsignacionMetodo
  = asignacion(str nombreObjeto,
               str nombreMetodo,
               list[Parametros] params,
               list[AnotTipo] retorno,
               list[CuerpoMetodo] cuerpoMetodo)
  ;

data Parametros
  = parametros(list[Parametro] lista)
  ;

data Parametro
  = parametro(str nombre, Tipo tipo)
  ;

// ---------- Sentencias ----------
data Sentencia
  = sentDecl(DeclaracionVariable declaracion)
  | sentExp(Exp expresion)
  | sentFor(SentenciaFor ciclo)
  ;

data CuerpoMetodo
  = cuerpo(list[Sentencia] sentencias)
  ;

data SentenciaFor
  = paraCada(ClausulaFor clausulaFor, Sentencia cuerpoFor)
  ;

data ClausulaFor
  = clausula(Patron patron, Exp coleccion)
  ;

data Patron
  = patronSimple(str nombre)
  | patronTupla(str primero, list[str] resto)
  ;

data DeclaracionVariable
  = declVal(str nombre, list[AnotTipo] tipo, Exp valor)
  | declVar(str nombre, list[AnotTipo] tipo, Exp valor)
  ;

// ---------- Expresiones ----------
data Exp
  = dato(Dato valor)
  | llamada(str nombre, list[Llamada] llamadas)
  | agrupada(Exp expresion)
  | expSi(Exp condicion, Exp entonces, Exp sino)
  | expNuevo(Tipo tipo, list[ArgsParen] argumentos, list[CuerpoObjeto] cuerpoNuevo)
  | negacion(Exp expresion)
  | menos(Exp expresion)
  | potencia(Exp izq, Exp der)
  | mult(Exp izq, Exp der)
  | division(Exp izq, Exp der)
  | modulo(Exp izq, Exp der)
  | suma(Exp izq, Exp der)
  | resta(Exp izq, Exp der)
  | menorQue(Exp izq, Exp der)
  | mayorQue(Exp izq, Exp der)
  | menorIgual(Exp izq, Exp der)
  | mayorIgual(Exp izq, Exp der)
  | distinto(Exp izq, Exp der)
  | igual(Exp izq, Exp der)
  | conY(Exp izq, Exp der)
  | conO(Exp izq, Exp der)
  ;

data Llamada
  = conArgumentos(list[Argumentos] argumentos)
  | conPunto(str nombre)
  ;

data Argumentos
  = argumentos(list[Exp] lista)
  ;

data ArgsParen
  = argsParen(list[Argumentos] argumentos)
  ;

data AnotTipo
  = anotTipo(Tipo tipo)
  ;

data CuerpoObjeto
  = cuerpoObjeto(list[MetodoOverride] metodos)
  ;

data MetodoOverride
  = metodoOverride(str nombre,
                   Parametros params,
                   list[AnotTipo] retorno,
                   CuerpoMetodo cuerpoMetodo)
  ;