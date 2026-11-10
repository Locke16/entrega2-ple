
object Animal[T] (val nombre: String, var edad: Int) {
    val peso: Int = 10
    var activo: Boolean = true
    method sonido(): String {
        val respuesta = "Sonido animal";
        print(respuesta);
    }
}

object Perro[T] extends Animal[T] (val nombre: String, var edad: Int) {
    val raza: String = "Labrador"
}

method calcular(x: Int, y: Int): Int {
    val suma = x + y;
    val resta = x - y;
    val producto = x * y;
    val division = x / y;
    val modulo = x % y;
    val potencia = x ** 2;
}

method saludar(nombre: String): String {
    print(nombre);
}

Animal.sonido = method(): String {
    val respuesta = "Guau";
    print(respuesta);
}

method main {
    val entero: Int = 10;
    var decimal: Float = 3.5;
    val booleano: Boolean = true;
    val caracter: Char = 'a';
    val texto: String = "Hola mundo";
    val interpolado: String = "Hola ${entero}";
    val interpoladoVariable: String = "Valor: $entero";

    val negado = neg booleano;
    val negativo = -entero;
    val agrupado = (entero + 5) * 2;
    val suma = entero + 5;
    val resta = entero - 2;
    val multiplicacion = entero * 3;
    val division = entero / 2;
    val modulo = entero % 3;
    val potencia = entero ** 2;

    val menor = entero < 20;
    val mayor = entero > 5;
    val menorIgual = entero <= 10;
    val mayorIgual = entero >= 10;
    val diferente = entero <> 5;
    val igual = entero = 10;
    val logicoY = booleano and false;
    val logicoO = booleano or false;

    val condicion = if entero > 5 then "Mayor" else "Menor";
    val pertenencia = entero in texto;
    val flecha = entero -> 5;
    val flechaDoble = entero => 5;

    val perro = new Perro[Int]("Max", 3);
    val animal = new Animal[String]("Firulais", 5);
    val llamada = calcular(entero, 2);
    val acceso = perro.sonido();
    val accesoSinArgs = perro.raza;

    for (i <- entero) print(i);
    for (x in texto) print(x);
    for ((a, b) <- texto) print(a);

    print(interpolado);
    print(condicion);
    print(llamada);
}
