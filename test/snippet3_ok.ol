object Animal
object Perro extends Animal
object Cachorro extends Perro

object Comida
object ComidaPerro extends Comida

object Transformador[-Entrada, +Salida]
Transformador.transformar = method(entrada: Entrada): Salida

method main {
  val alimentadorAnimal: Transformador[Animal, ComidaPerro] = new Transformador[Animal, ComidaPerro] {
    override transformar(animal: Animal): ComidaPerro = {
      println("Preparando comida especializada para un animal...");
      new ComidaPerro
    }
  };

  val alimentadorCachorro: Transformador[Cachorro, Comida] = alimentadorAnimal;

  val miCachorro = new Cachorro;
  val comidaResultante: Comida = alimentadorCachorro.transformar(miCachorro);

  print("Tipo de comida resultante: ${comidaResultante.getClass.getSimpleName}")
}
