object Animal(val nombre: String)
object Perro extends Animal(val nombre: String)
object Perdido extends Fantasma
object Caja[+A](val contenido: A)
object Fila(val elementos: Cosa)
Fantasma.hablar = method(): String

method doble(numero: Int): Int { numero * 2; }

method main {
  val perroFalso: Perro = new Animal("x");
  val argumentoMalo = new Perro(5);
  val argumentosDeMas = new Perro("a", "b");
  val cajaMala: Caja[Perro] = new Caja(new Animal("y"));
  val misterio = new Misterio;
  val vuelo = perroFalso.volar();
  val edad = perroFalso.edad;
  val duplicado: Int = doble("x");
  val sobrantes = doble(1, 2);
  val llamada = desconocida(3);
  val nombreEntero: Int = perroFalso.nombre;
  val cajaOverride: Caja[Animal] = new Caja[Animal](new Animal("z")) {
    override inexistente(x: Int): Int = { x; }
  };
}
