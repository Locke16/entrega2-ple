object Animal(val nombre: String) {
  val patas: Int = 4
  method describir(): String { nombre }
}
object Perro extends Animal(val nombre: String, val raza: String)

object Caja[+A](val contenido: A)
object Par[A, B](val primero: A, val segundo: B)

object Funcion[-Entrada, +Salida]
Funcion.aplicar = method(x: Entrada): Salida

method doble(numero: Int): Int { numero * 2 }
method saludo(animal: Animal): String { animal.describir() }

method main {
  val rex = new Perro("Rex", "Labrador");
  val animal: Animal = rex;
  val nombre: String = animal.nombre;
  val patas: Int = rex.patas;
  val total: Int = doble(patas) + 1;

  val caja: Caja[Perro] = new Caja(rex);
  val cajaAnimal: Caja[Animal] = caja;
  val interior: Animal = cajaAnimal.contenido;

  val par = new Par(1, "uno");
  val par2: Par[Int, String] = par;

  val aPerro: Funcion[Animal, Perro] = new Funcion[Animal, Perro] {
    override aplicar(x: Animal): Perro = { rex }
  };
  val aAnimal: Funcion[Perro, Animal] = aPerro;
  val resultado: Animal = aAnimal.aplicar(rex);

  val texto: String = saludo(rex);
  print("${rex.describir()} / ${rex.raza} / ${rex.getClass.getSimpleName}")
}
