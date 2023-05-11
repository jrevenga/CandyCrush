import scala.annotation.tailrec
import scala.util.Random

  class GameLogic {

    // Función para jugar manualmente al juego
    def gameManual(tablero: List[Int], fila: Int, col: Int, dificultad: Int, vidas: Int, borrar: Int): (List[Int], Int) = {

      // Función auxiliar para modificar el tablero
      def modificarTablero(tableroMod: List[Int], bonus: Int): (List[Int], Int) = {
        mostrarTablero(tableroMod, col)
        val tableroFin = subeYrellena(tableroMod, dificultad, col)
        mostrarTablero(tableroFin, col)
        (tableroFin, vidas)
      }
      val elem = getElem(borrar, tablero)
      if (elem >= 7) {
        // Bloque especial
        val tableroMod = elem match {
          case 7 => bloqueB(tablero, borrar, fila, col)
          case 8 => bloqueTNT(tablero, borrar, fila, col)
          case _ => bloqueR(ponerValor(tablero, borrar, elem - 8), elem - 8)
        }
        val bonus = elem match {
          case 7 => 5
          case 8 => 10
          case _ => 15
        }
        modificarTablero(tableroMod, bonus)
      } else {
        // Bloque normal
        val tableroMod = posBorradas(tablero, borrar, fila, col, dificultad)
        tableroMod match {
          case Nil => println("Fallo! -1 vida"); (tablero, vidas - 1)
          case _ => modificarTablero(tableroMod, 0)
        }
      }
    }

    // Función para jugar automáticamente al juego
    def gameAuto(tablero: List[Int], fila: Int, col: Int, dificultad: Int, vidas: Int): (List[Int], Int) = {

      // Función auxiliar para aplicar una modificación al tablero
      def modificarTableroYsubir(tableroMod: List[Int]): (List[Int], Int) = {
        val tableroFin = subeYrellena(tableroMod, dificultad, col) // Sube los ceros y rellena los huecos
        mostrarTablero(tableroFin, col)
        (tableroFin, vidas) // Devuelve el nuevo tablero junto con las vidas actuales
      }

      // Encuentra la mejor posición para borrar en el tablero
      val (mejorPos, borrados) = mejorPosicion(tablero, 0, -1, -1, fila, col)
      println(s"Posición optima: $mejorPos que borra $borrados bloques")

      // Obtén el elemento en la mejor posición
      val elem = getElem(mejorPos, tablero)

      // Verifica si el elemento es un bloque especial (>= 7)
      if (elem >= 7) {
        // Bloque especial
        val tableroMod = elem match {
          // Bloque Bomba
          case 7 if fila == col => bloqueB(tablero, mejorPos, fila, col)
          case 7 if borrados == fila => borrarFila(tablero, 0, mejorPos / col, fila, col)
          case 7 => borrarColumna(tablero, mejorPos % col, fila, col)
          // Bloque TNT
          case 8 => bloqueTNT(tablero, mejorPos, fila, col)
          // Bloque Rompecabezas
          case _ => bloqueR(ponerValor(tablero, mejorPos, elem - 8), elem - 8)
        }
        // Aplica la modificación al tablero y luego sube y rellena los ceros
        modificarTableroYsubir(tableroMod)
      } else {
        // Bloque normal
        // Borrar posiciones en el tablero basado en la mejor posición encontrada
        val tableroMod = posBorradas(tablero, mejorPos, fila, col, dificultad)
        tableroMod match {
          // Si no se borran posiciones, el jugador pierde una vida
          case Nil => (tablero, vidas - 1)
          // Si se borran posiciones, vuelve a modificar
          case _ => modificarTableroYsubir(tableroMod)
        }
      }
    }

    // Genera una lista de ceros con longitud n
    def inicializarTablero(n: Int): List[Int] = {
      n match {
        case 0 => Nil
        case _ => 0 :: inicializarTablero(n - 1)
      }
    }

    private val random = new Random()

    private def posBorradas(tablero: List[Int], posBorrar: Int, fila: Int, col: Int, dificultad: Int): List[Int] = {
      val (tableroFin, nElim) = borrarPosicion(tablero, posBorrar, fila, col)
      if (nElim == 1) {
        Nil   // Si solo se ha eliminado un elemento, devolvemos Nil
      } else {
        // Dependiendo del número de elementos eliminados y la dificultad del juego, modificamos el tablero de diferentes maneras
        nElim match {
          case x if x < 5 => tableroFin
          case 5 => ponerValor(tableroFin, posBorrar, 7)
          case 6 => ponerValor(tableroFin, posBorrar, 8)
          case _ if dificultad == 1 => ponerValor(tableroFin, posBorrar, random.nextInt(4) + 9)
          case _ if dificultad == 2 => ponerValor(tableroFin, posBorrar, random.nextInt(6) + 9)
        }
      }
    }

    private def ponerValor(tablero: List[Int], pos: Int, valor: Int): List[Int] = {
      pos match {
        case 0 => valor :: tablero.tail
        case _ => tablero.head :: ponerValor(tablero.tail, pos - 1, valor)
      }
    }

    // Sube los ceros y rellena el tablero con números aleatorios
    private def subeYrellena(tablero: List[Int], dificultad: Int, col: Int): List[Int] = {
      def aux(pos: Int): List[Int] = pos match {
        case _ if pos == col => Nil
        case _ =>
          val columna = getColumna(pos, tablero, col)
          val nCeros = contarCeros(columna, 0)
          val numeros = listaNumeros(columna)
          val relleno = rellena(listaDeCeros(nCeros), dificultad)
          concatenar(concatenar(relleno, numeros), aux(pos + 1))
      }
      traspuesta(aux(0), col)
    }

    def rellena(l: List[Int], dificultad: Int): List[Int] = l match {
      case Nil => Nil
      // Si el primer elemento es cero, genera un número aleatorio según el dificultad y lo agrega a la lista resultante
      case 0 :: tail =>
        val random = if (dificultad == 1) new Random().nextInt(4) + 1 else new Random().nextInt(6) + 1
        random :: rellena(tail, dificultad)
      // Si el primer elemento no es cero, simplemente lo agrega a la lista resultante
      case head :: tail => head :: rellena(tail, dificultad)
    }

    private def listaDeCeros(n: Int): List[Int] = n match {
      case 0 => Nil
      case _ => 0 :: listaDeCeros(n - 1)
    }

    private def listaNumeros(l: List[Int]): List[Int] = {
      l match {
        case Nil => Nil
        case head :: tail if head == 0 => listaNumeros(tail)
        case head :: tail => head :: listaNumeros(tail)
      }
    }

    private def bloqueB(tablero: List[Int], borrar: Int, fila: Int, col: Int): List[Int] = {
      val n = util.Random.nextInt(2) // Generar un número aleatorio entre 0 y 1
      if (n == 0) {
        // Borrar fila
        val filaBorrar = borrar / col
        borrarFila(tablero, 0, filaBorrar, fila, col)
      } else {
        // Borrar columna
        val colBorrar = borrar % col
        borrarColumna(tablero, colBorrar, fila, col)
      }
    }

    private def borrarColumna(tablero: List[Int], colBorrar: Int, fila: Int, col: Int): List[Int] = {
      @tailrec
      def borrarColumnaAux(tablero: List[Int], colActual: Int, colBorrar: Int, acc: List[Int]): List[Int] = {
        if (colActual == col) {
          acc.reverse // Devolver la lista acumulada invertida
        } else if (colBorrar == 0) {
          val filaInicial = inicializarTablero(fila) // Crear una fila de ceros
          borrarColumnaAux(tablero, colActual + 1, colBorrar - 1, concatenar(filaInicial, acc)) // Concatenar la fila de ceros con la lista acumulada
        } else {
          val columna = getColumna(colActual, tablero, col) // Obtener la columna actual del tablero
          borrarColumnaAux(tablero, colActual + 1, colBorrar - 1, concatenar(columna, acc)) // Concatenar la columna con la lista acumulada
        }
      }
      val matrizAux = borrarColumnaAux(tablero, 0, colBorrar, List.empty) // Llamar a la función auxiliar para realizar el borrado de columna
      traspuesta(matrizAux, fila) // Obtener la matriz traspuesta del resultado y devolverla
    }


    private def borrarFila(tablero: List[Int], filaActual: Int, filaBorrar: Int, fila: Int, col: Int): List[Int] = {
      if (filaActual == fila) {
        Nil // Caso base: se ha alcanzado la última fila, devuelve una lista vacía
      } else if (filaBorrar == 0) {
        concatenar(inicializarTablero(col), borrarFila(tablero, filaActual + 1, filaBorrar - 1, fila, col))
      } else {
        concatenar(getFila(filaActual, tablero, col), borrarFila(tablero, filaActual + 1, filaBorrar - 1, fila, col))
      }
    }

    private def bloqueTNT(tablero: List[Int], borrar: Int, fila: Int, col: Int): List[Int] = {
      // Calcular la fila y columna correspondientes a la posición "borrar"
      val f = borrar / col
      val c = borrar % col

      // Calcular las posiciones de inicio y fin de la sección a borrar
      val primPos = (c - 3).max(0)
      val ultPos = (c + 4).min(col)

      // Crear la lista de filas que se van a borrar
      val filas = concatenar(filasNegativas(f, fila), descartar(1, filasPositivas(f, fila)))

      // Borrar la sección específica del tablero
      val tableroFin = borrarSeccion(tablero, 0, primPos, ultPos, filas, fila, col)

      tableroFin
    }

    private def borrarSeccion(tablero: List[Int], a: Int, pPos: Int, ultPos: Int, filas: List[Int], fila: Int, col: Int): List[Int] = {
      if (a == fila) {
        Nil // Caso base: se ha alcanzado la última fila, devuelve una lista vacía
      } else {
        val ff = getFila(a, tablero, col) // Obtener la fila actual del tablero
        val filaFinal =
          if (pertenece(a, filas)) {
            val filaInicial = tomar(pPos, ff) // Tomar los primeros pPos elementos de la fila
            val filaCentral = inicializarTablero(ultPos - pPos) // Inicializar una fila de tamaño (ultPos - pPos) con ceros
            val filaRestante = descartar(ultPos, ff) // Descartar los elementos desde ultPos hasta el final de la fila original
            concatenar(concatenar(filaInicial, filaCentral), filaRestante) // Concatenar las tres partes para formar la nueva fila
          } else {
            ff // Si la fila no está en la lista de filas a borrar, mantener la fila original sin cambios
          }
        concatenar(filaFinal, borrarSeccion(tablero, a + 1, pPos, ultPos, filas, fila, col)) // Concatenar la fila modificada con el resultado de la recursión en la siguiente fila
      }
    }

    private def bloqueR(tablero: List[Int], valor: Int): List[Int] = {
      tablero match {
        case Nil => Nil // Caso base: si la lista está vacía, se devuelve una lista vacía
        case cabeza :: cola if cabeza == valor => 0 :: bloqueR(cola, valor) // Si la cabeza de la lista coincide con el valor, se reemplaza por 0 y se realiza la recursión en la cola
        case cabeza :: cola => cabeza :: bloqueR(cola, valor) // Si la cabeza de la lista no coincide con el valor, se mantiene en la lista resultante y se realiza la recursión en la cola
      }
    }

    // Devuelve la posición del tablero que borra el mayor número de bloques
    private def mejorPosicion(tablero: List[Int], posActual: Int, mejorPos: Int, nMejorPos: Int, fila: Int, col: Int): (Int, Int) = {
      posActual match {
        // Caso base: se ha recorrido todo el tablero
        case x if x == fila * col => (mejorPos, nMejorPos)
        case _ =>
          val elem = getElem(posActual, tablero)
          if (elem >= 7) {
            elem match {
              case 7 =>
                // Caso 7: borrar fila o columna al azar
                val randomValue = new Random().nextInt(2)
                val (borrarPos, borrarNum) = if (randomValue == 0) (fila, fila) else (col, col)
                val (mejorPosActualizada, nMejorPosAct) = if (borrarPos > nMejorPos) (posActual, borrarNum) else (mejorPos, nMejorPos)
                mejorPosicion(tablero, posActual + 1, mejorPosActualizada, nMejorPosAct, fila, col)
              case 8 =>
                // Caso 8: borrar una sección
                val f = posActual / col
                val c = posActual % col
                val primPos = (c - 3).max(0)
                val ultPos = (c + 4).min(col)
                val filas = concatenar(filasNegativas(f, fila), descartar(1, filasPositivas(f, fila)))
                val colBorrar = ultPos - primPos
                val nFilas = longitud(filas)
                val total = colBorrar * nFilas
                val (mejorPosActualizada, nMejorPosAct) = if (total > nMejorPos) (posActual, total) else (mejorPos, nMejorPos)
                mejorPosicion(tablero, posActual + 1, mejorPosActualizada, nMejorPosAct, fila, col)
              case _ =>
                // Otros casos: borrar un número específico
                val num = elem - 8
                val numElem = contar(tablero, num, 0)
                val total = numElem + 1
                val (mejorPosActualizada, nMejorPosAct) = if (total > nMejorPos) (posActual, total) else (mejorPos, nMejorPos)
                mejorPosicion(tablero, posActual + 1, mejorPosActualizada, nMejorPosAct, fila, col)
            }
          } else {
            // Caso: borrar un bloque menor a 7
            val borrado = borrarPosicion(tablero, posActual, fila, col)
            val nElim = borrado._2
            val (mejorPosActualizada, nMejorPosAct) = if (nElim > nMejorPos) (posActual, nElim) else (mejorPos, nMejorPos)
            mejorPosicion(tablero, posActual + 1, mejorPosActualizada, nMejorPosAct, fila, col)
          }
      }
    }

    private def borrarPosicion(tablero: List[Int], posBorrar: Int, fila: Int, col: Int): (List[Int], Int) = {
      val valor = getElem(posBorrar, tablero) // Obtener posicion seleccionada
      val matrizAux = borrar(tablero, posBorrar) // Matriz con posicion borrada
      val tableroFin = borrarAdyacentes(matrizAux, 0, valor, fila, col) // Realizar el proceso de borrado en la matriz auxiliar
      val nElim = contarCeros(tableroFin, 0) // Contar la cantidad de elementos eliminados
      (tableroFin, nElim)
    }

    // Esta función se encarga de borrar elementos adyacentes en una matriz.
    private def borrarAdyacentes(matriz: List[Int], pos: Int, valor: Int, fila: Int, col: Int): List[Int] = {
      // Esta función auxiliar se encarga de comprobar una condición y, si se cumple,
      // borra el elemento en la posición actual y reinicia la búsqueda desde el inicio de la matriz.
      def borrarSiCumpleCondicion(condicion: => Boolean, matriz: List[Int], pos: Int): List[Int] = {
        if (condicion) {
          val nuevaMatriz = borrar(matriz, pos)
          borrarAdyacentes(nuevaMatriz, 0, valor, fila, col)
        } else matriz
      }
      if (col * fila == pos) matriz // Si ya hemos llegado al final de la matriz, la devolvemos tal cual.
      else {
        val matrizAux1 = borrarSiCumpleCondicion( // Comprobamos si podemos borrar por la derecha
          (pos + 1) < fila * col && getElem(pos, matriz) == valor && getElem(pos + 1, matriz) == 0 && (pos + 1) % col != 0,
          matriz,
          pos
        )
        val matrizAux2 = borrarSiCumpleCondicion( // Comprobamos si podemos borrar por la izquierda
          (pos - 1) >= 0 && getElem(pos, matriz) == valor && getElem(pos - 1, matriz) == 0 && pos % col != 0,
          matrizAux1,
          pos
        )
        // Comprobamos si podemos borrar por arriba
        val matrizAux3 = borrarSiCumpleCondicion(
          (pos + col) < fila * col && getElem(pos, matriz) == valor && getElem(pos + col, matriz) == 0,
          matrizAux2,
          pos
        )
        // Comprobamos si podemos borrar por abajo
        val matrizAux4 = borrarSiCumpleCondicion(
          (pos - col) >= 0 && getElem(pos, matriz) == valor && getElem(pos - col, matriz) == 0,
          matrizAux3,
          pos
        )
        // Si no se cumplen las condiciones para borrar el elemento actual, pasamos al siguiente elemento.
        borrarAdyacentes(matrizAux4, pos + 1, valor, fila, col)
      }
    }

    // Devuelve el número de ceros que hay en una lista
    @tailrec
    private def contarCeros(l: List[Int], n: Int): Int = {
      l match {
        case Nil => n
        case head :: tail if head == 0 => contarCeros(tail, n + 1)
        case _ :: tail => contarCeros(tail, n)
      }
    }

    // Pone un 0 en la posicion que se quiere eliminar
    private def borrar(tablero: List[Int], pos: Int): List[Int] = {
      tablero match {
        case Nil => Nil
        case _ if pos == 0 => 0 :: borrar(tablero.tail, pos - 1)
        case _ => tablero.head :: borrar(tablero.tail, pos - 1)
      }
    }

    private def filasPositivas(f: Int, fila: Int): List[Int] = {
      if (f > fila || f >= 4) {
        Nil // Caso base: si f es mayor que la cantidad total de filas o igual a 4, devuelve una lista vacía
      } else {
        f :: filasPositivas(f + 1, fila) // Agrega f a la lista y continúa con la siguiente fila
      }
    }

    private def filasNegativas(f: Int, fila: Int): List[Int] = {
      if (f < 0 || -f > fila || -f >= 4) {
        Nil // Caso base: si f es menor que 0, mayor que la cantidad total de filas o igual a -4, devuelve una lista vacía
      } else {
        f :: filasNegativas(f - 1, fila) // Agrega f a la lista y continúa con la fila anterior
      }
    }

    // Imprime el tablero
    def mostrarTablero(tablero: List[Int], col: Int): Unit = {
      @tailrec
      def mostrarTableroRec(tablero: List[Int], col: Int, fila: Int): Unit = {
        if (tablero.isEmpty) {
          println("-" * (col * 4 + 5))
          println()
        } else {
          if (fila == 0) {
            print("     ")
            for (c <- 0 until col) {
              print(f"${c + 1}%4d")
            }
            println()
            println("-" * (col * 4 + 5))
          }
          val filaActual = tablero.take(col)
          print(f"${fila + 1}%3d | ")
          imprimirFila(filaActual, col)
          mostrarTableroRec(tablero.drop(col), col, fila + 1)
        }
      }
      mostrarTableroRec(tablero, col, 0)
    }

    private def imprimirFila(l: List[Int], n: Int): Unit = {
      @tailrec
      def imprimirFilaRec(l: List[Int], n: Int, separadorImpreso: Boolean): Unit = {
        if (n == 0) {
          println()
        } else {
          val elemento = l.head
          l.head match {
            case 7 => print("  B ")
            case 8 => print("  T ")
            case 9 => print("  R1")
            case 10 => print(" R2 ")
            case 11 => print(" R3 ")
            case 12 => print(" R4 ")
            case 13 => print(" R5 ")
            case 14 => print(" R6 ")
            case _ => print(f" $elemento%2d ")
          }
          imprimirFilaRec(l.tail, n - 1, true)
        }
      }

      imprimirFilaRec(l, n, false)
    }

    private def traspuesta(tablero: List[Int], col: Int): List[Int] = {
      traspuestaAux(tablero, 0, col)
    }

    private def traspuestaAux(tablero: List[Int], columnaActual: Int, col: Int): List[Int] = {
      columnaActual match {
        case x if x == col => Nil
        case _ => concatenar(getColumna(columnaActual, tablero, col), traspuestaAux(tablero, columnaActual + 1, col))
      }
    }

    @tailrec
    private def contar(tablero: List[Int], num: Int, total: Int): Int = {
      tablero match {
        case Nil => total
        case cabeza :: cola if cabeza == num => contar(cola, num, total + 1)
        case _ :: cola => contar(cola, num, total)
      }
    }

    private def getFila(fila: Int, tablero: List[Int], col: Int): List[Int] = {
      tomar(col, descartar(fila * col, tablero))
    }

    def getColumna(columna: Int, tablero: List[Int], col: Int): List[Int] = {
      def _getColumna(cont: Int, tab: List[Int]): List[Int] = tab match {
        case Nil => Nil
        case cabeza :: cola =>
          if (cont % col == columna) cabeza :: _getColumna(cont + 1, cola)
          else _getColumna(cont + 1, cola)
      }

      _getColumna(0, tablero)
    }

    def getElem[A](n: Int, lista: List[A]): A = (n, lista) match {
      case (0, cabeza :: _) => cabeza
      case (n, _ :: cola) if n > 0 => getElem(n - 1, cola)
      case _ => throw new NoSuchElementException
    }

    def tomar[A](n: Int, lista: List[A]): List[A] = (n, lista) match {
      case (_, Nil) => Nil
      case (0, _) => Nil
      case (n, cabeza :: cola) => cabeza :: tomar(n - 1, cola)
    }

    def descartar[A](n: Int, lista: List[A]): List[A] = (n, lista) match {
      case (_, Nil) => Nil
      case (0, lista) => lista
      case (n, cabeza :: cola) => descartar(n - 1, cola)
    }

    def pertenece[A](elemento: A, lista: List[A]): Boolean = lista match {
      case Nil => false
      case cabeza :: cola => if (cabeza == elemento) true else pertenece(elemento, cola)
    }

    def concatenar[A](lista1: List[A], lista2: List[A]): List[A] = lista1 match {
      case Nil => lista2
      case cabeza :: cola => cabeza :: concatenar(cola, lista2)
    }

    def longitud[A](lista: List[A]): Int = lista match {
      case Nil => 0
      case _ :: cola => 1 + longitud(cola)
    }

    def esVacia[A](lista: List[A]): Boolean = lista match {
      case Nil => true
      case _ => false
    }

    def reversa[A](lista: List[A]): List[A] = {
      def _reversa(res: List[A], rem: List[A]): List[A] = rem match {
        case Nil => res
        case cabeza :: cola => _reversa(cabeza :: res, cola)
      }

      _reversa(Nil, lista)
    }


  }