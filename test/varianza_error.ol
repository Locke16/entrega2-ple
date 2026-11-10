object Animal
object Perro extends Animal
object Cachorro extends Perro

object Caja[+A](val contenido: A)
object Funcion[-Entrada, +Salida]
Funcion.aplicar = method(x: Entrada): Salida

method main {
  val cajaAnimales: Caja[Animal] = new Caja[Animal](new Animal);
  val cajaPerros: Caja[Perro] = cajaAnimales;
  val aPerro: Funcion[Perro, Animal] = new Funcion[Perro, Animal] {
    override aplicar(x: Perro): Animal = { x }
  };
  val invalida: Funcion[Animal, Perro] = aPerro;
  val valida: Funcion[Cachorro, Animal] = aPerro;
  print("fin")
}
