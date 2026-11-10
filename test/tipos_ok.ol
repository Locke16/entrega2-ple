method main {
  val entero: Int = 10;
  val decimal = 3.5;
  var producto: Float = decimal * 2.0;
  val texto: String = "hola";
  val letra: Char = 'x';
  val valido: Boolean = entero < 20 and neg false;
  val resultado = if valido then entero else 0;
  val mensaje = "valor: ${entero + 1} y $texto";
  print(mensaje)
}
