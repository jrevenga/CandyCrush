import scala.annotation.tailrec
import scala.util.Random

  class GameLogic {
    // Función que ejecuta el juego de manera automática (aleatoria)
    def juegoAutomatico(matriz: List[Int], fila: Int, col: Int, nivel: Int, vidas: Int, puntuacion: Int): (List[Int],Int,Int) ={
      val pos = mejorPosicion(matriz,0,-1,-1,fila,col)
      val mejorPos = pos._1
      val numBorrados = pos._2
      println(s"La posición que se va a borrar es: $mejorPos y borra $numBorrados bloques")
      val elem = getElem(mejorPos,matriz)
      if(elem >= 7){
        // Bloque especial
        elem match {
          case 7 if fila == col => val matrizElim = bloqueB(matriz, mejorPos, fila, col)
            val matrizFinal = restaurarMatriz(matrizElim, nivel, fila, col)
            (matrizFinal,vidas,puntuacion)
          case 7 =>
            if(numBorrados == fila){
              // Borrar fila
              val filaBorrar = mejorPos / col
              val matrizElim = borrarFila(matriz,0,filaBorrar,fila,col)
              val matrizFinal = restaurarMatriz(matrizElim, nivel, fila, col)
              (matrizFinal,vidas,puntuacion)
            }else{
              // Borrar columna
              val colBorrar = mejorPos % col
              val matrizElim = borrarColumna(matriz,colBorrar,fila,col)
              val matrizFinal = restaurarMatriz(matrizElim, nivel, fila, col)
              (matrizFinal,vidas,puntuacion)
            }
          case 8 => val matrizElim = bloqueTNT(matriz, mejorPos, fila, col)
            val matrizFinal = restaurarMatriz(matrizElim, nivel, fila, col)
            (matrizFinal,vidas,puntuacion)
          case _ => val valor = elem - 8
            val matrizAux = ponerValor(matriz, mejorPos, valor)
            val matrizElim = eliminarTodos(matrizAux, valor)
            val matrizFinal = restaurarMatriz(matrizElim, nivel, fila, col)
            (matrizFinal,vidas,puntuacion)
        }
      }else{
        // Bloque normal
        val matrizElim = eliminar(matriz, mejorPos, fila, col, nivel)
        matrizElim match{
          case Nil => (matriz, vidas-1,puntuacion)
          case _ => val matrizFinal = restaurarMatriz(matrizElim, nivel, fila, col)
            (matrizFinal,vidas,puntuacion)
        }
      }
    }

    // Función que ejecuta el juego de manera manual
    def juegoManual(matriz: List[Int], fila: Int, col: Int, nivel: Int, vidas: Int, borrar: Int, puntuacion: Int): (List[Int],Int,Int) = {
      imprimirMatriz(matriz, col)
      val elem = getElem(borrar,matriz)
      if(elem >= 7){
        // Bloque especial
        elem match {
          case 7 => val matrizElim = bloqueB(matriz, borrar, fila, col)
            val numBloq = contar0(matrizElim, 0)
            val puntExtra = numBloq / 10
            val matrizFinal = restaurarMatriz(matrizElim, nivel, fila, col)
            (matrizFinal,vidas,puntuacion+numBloq+puntExtra+5)
          case 8 => val matrizElim = bloqueTNT(matriz, borrar, fila, col)
            val numBloq = contar0(matrizElim, 0)
            val puntExtra = numBloq / 10
            val matrizFinal = restaurarMatriz(matrizElim, nivel, fila, col)
            (matrizFinal,vidas,puntuacion+numBloq+puntExtra+10)
          case _ => val valor = elem - 8
            val matrizAux = ponerValor(matriz, borrar, valor)
            val matrizElim = eliminarTodos(matrizAux,valor)
            val numBloq = contar0(matrizElim, 0)
            val puntExtra = numBloq / 10
            val matrizFinal = restaurarMatriz(matrizElim, nivel, fila, col)
            (matrizFinal,vidas,puntuacion+numBloq+puntExtra+15)
        }
      }else{
        // Bloque normal
        val matrizElim = eliminar(matriz, borrar, fila, col, nivel)
        matrizElim match {
          case Nil => println("Posicion incorrecta -1 vidas"); (matriz, vidas-1,puntuacion)
          case _ => val numBloq = contar0(matrizElim,0)
            val puntExtra = numBloq/10
            val matrizFinal = restaurarMatriz(matrizElim, nivel, fila, col)
            (matrizFinal,vidas,puntuacion+numBloq+puntExtra)
        }
      }
    }

    // Función restaura la matriz, subiendo los ceros y rellenandolos posteriormente
    private def restaurarMatriz(matriz: List[Int], nivel: Int, fila: Int, col: Int): List[Int] ={
      val matrizSig = cubrirBloques(matriz, fila, col)
      val matrizFinal = rellenarMatriz(matrizSig, nivel)
      matrizFinal
    }

    // Genera una lista de ceros con longitud n
    def generarMatrizVacia(n: Int): List[Int] ={
      n match {
        case 0 => Nil
        case _ => 0::generarMatrizVacia(n-1)
      }
    }

    // Cambia los 0 por un numero aleatorio en funcion del nivel
    def rellenarMatriz(matriz: List[Int], nivel: Int): List[Int] ={
      matriz match {
        case Nil => Nil
        case head::tail if head == 0 =>
          if(nivel == 1){
            val random = new Random().nextInt(4) + 1
            random::rellenarMatriz(tail, nivel)
          }else{
            val random = new Random().nextInt(6) + 1
            random::rellenarMatriz(tail, nivel)
          }
        case head::tail => head::rellenarMatriz(tail, nivel)
      }
    }

    // Imprime la matriz por pantalla
    @tailrec
    private def imprimirMatriz(matriz: List[Int], col: Int): Unit = {
      matriz match {
        case Nil => println("-" * (col * 3 + 1))
        case _ if length(matriz) % col != 0 => throw new Error(s"No es multiplo de $col")
        case _ => println("-" * (col * 3 + 1)); imprimirNElem(matriz, col); imprimirMatriz(deja(col, matriz), col)
      }
    }

    @tailrec
    private def borrarAux(matriz: List[Int], pos: Int, valor: Int, fila: Int, col: Int): List[Int] = {
      pos match {
        case x if col * fila == x => matriz
        // Se pueda borrar por la derecha
        case x if (x + 1) < fila * col && getElem(x, matriz) == valor && getElem(x + 1, matriz) == 0 && (x + 1) % col != 0 =>
          val matrizAux = poner0(matriz, x)
          borrarAux(matrizAux, 0, valor, fila, col)
        // Se pueda borrar por la izquierda
        case x if (x - 1) >= 0 && getElem(x, matriz) == valor && getElem(x - 1, matriz) == 0 && x % col != 0 =>
          val matrizAux = poner0(matriz, x)
          borrarAux(matrizAux, 0, valor, fila, col)
        // Se pueda borrar por arriba
        case x if (x + col) < fila * col && getElem(x, matriz) == valor && getElem(x + col, matriz) == 0 =>
          val matrizAux = poner0(matriz, x)
          borrarAux(matrizAux, 0, valor, fila, col)
        // Se pueda borrar por abajo
        case x if (x - col) >= 0 && getElem(x, matriz) == valor && getElem(x - col, matriz) == 0 =>
          val matrizAux = poner0(matriz, x)
          borrarAux(matrizAux, 0, valor, fila, col)
        case _ => borrarAux(matriz, pos + 1, valor, fila, col)
      }
    }

    private def borrarPos(matriz: List[Int], posBorrar: Int, fila: Int, col: Int): (List[Int], Int) = {
      // Saber cual es el valor a borrar
      val valor = getElem(posBorrar, matriz)
      // Matriz con la posBorrar a 0
      val matrizAux = poner0(matriz, posBorrar)
      // matriz borrada
      val matrizFinal = borrarAux(matrizAux, 0, valor, fila, col)
      // Cantidad de elementos borrados
      val nElim = contar0(matrizFinal, 0)
      (matrizFinal, nElim)
    }

    // Función que devuelve una lista con las posiciones borradas dada la posición posBorrar,
    // devuelve una lista vacia si no se elimina ninguno
    private def eliminar(matriz: List[Int], posBorrar: Int, fila: Int, col: Int, nivel: Int): List[Int] = {
      val borrado = borrarPos(matriz, posBorrar, fila, col)
      val nElim = borrado._2

      if (nElim == 1) {
        Nil
      } else {
        val matrizFinal = borrado._1
        nElim match {
          case x if x < 5 => matrizFinal
          case 5 => ponerValor(matrizFinal, posBorrar, 7)
          case 6 => ponerValor(matrizFinal, posBorrar, 8)
          case _ if nivel == 1 => val ale = new Random().nextInt(4) + 1; ponerValor(matrizFinal, posBorrar, ale + 8)
          case _ if nivel == 2 => val ale = new Random().nextInt(6) + 1; ponerValor(matrizFinal, posBorrar, ale + 8)
        }
      }
    }

    // Establece un 0 en la lista matriz en la posición pos
    private def poner0(matriz: List[Int], pos: Int): List[Int] = {
      matriz match {
        case Nil => Nil
        case _ if pos == 0 => 0 :: poner0(matriz.tail, pos - 1)
        case _ => matriz.head :: poner0(matriz.tail, pos - 1)
      }
    }

    // Sube los 0 a las posiciones más altas de la matriz
    private def cubrirBloques(matriz: List[Int], fila: Int, col: Int): List[Int] ={
      val matrizAux = subir0(matriz,0,col)
      val matrizTrasp = traspuesta(matrizAux,fila)
      matrizTrasp
    }

    // Función auxiliar que sube los ceros a las posiciones mas altas de la matriz
    private def subir0(matriz: List[Int], pos: Int, col: Int): List[Int] = {
      pos match {
        case _ if pos == col => Nil
        case _ => val c = getColumna(pos, matriz, col)
          val nCeros = contar0(c,0)
          val lNum = listaNumeros(c)
          concatenar(concatenar(generarMatrizVacia(nCeros), lNum), subir0(matriz, pos + 1, col))
      }
    }

    // Devuelve el número de ceros que hay en una lista
    @tailrec
    private def contar0(l: List[Int], n: Int): Int = {
      l match {
        case Nil => n
        case head :: tail if head == 0 => contar0(tail, n+1)
        case _ :: tail => contar0(tail, n)
      }
    }

    // Devuelve una lista eliminando los ceros en la lista l
    private def listaNumeros(l: List[Int]): List[Int] = {
      l match {
        case Nil => Nil
        case head :: tail if head == 0 => listaNumeros(tail)
        case head :: tail => head :: listaNumeros(tail)
      }
    }

    // Función que elimina una lista o una columna de manera aleatoria
    private def bloqueB(matriz: List[Int], borrar: Int, fila: Int, col: Int): List[Int] ={
      val numAle = new Random().nextInt(2)
      if(numAle == 0){
        // Borrar fila
        val filaBorrar = borrar/col
        borrarFila(matriz,0,filaBorrar,fila,col)
      }else{
        // Borrar columna
        val colBorrar = borrar%col
        borrarColumna(matriz,colBorrar,fila,col)
      }
    }

    // Establece todos los valores de la columna colBorrar a 0
    private def borrarColumna(matriz: List[Int], colBorrar: Int, fila: Int, col: Int): List[Int] = {
      val matrizAux = borrarColumnaAux(matriz, 0, colBorrar, fila, col)
      val matrizTrasp = traspuesta(matrizAux, fila)
      matrizTrasp
    }

    // Función auxiliar que borra una columna de la matriz
    private def borrarColumnaAux(matriz: List[Int], colActual: Int, colBorrar: Int, fila: Int, col: Int): List[Int] = {
      colActual match {
        case _ if colActual == col => Nil
        case _ if colBorrar == 0 => concatenar(generarMatrizVacia(fila), borrarColumnaAux(matriz, colActual + 1, colBorrar - 1, fila, col))
        case _ => concatenar(getColumna(colActual, matriz, col), borrarColumnaAux(matriz, colActual + 1, colBorrar - 1, fila, col))
      }
    }

    // Función que borra todos los valores de la fila filaBorrar
    private def borrarFila(matriz: List[Int], filaActual: Int, filaBorrar: Int, fila: Int, col: Int): List[Int] = {
      filaActual match {
        case _ if filaActual == fila => Nil
        case _ if filaBorrar == 0 => concatenar(generarMatrizVacia(col), borrarFila(matriz, filaActual + 1, filaBorrar - 1, fila, col))
        case _ => concatenar(getFila(filaActual, matriz, col), borrarFila(matriz, filaActual + 1, filaBorrar - 1, fila, col))
      }
    }

    // Función que elimina una submatriz de la matriz
    private def bloqueTNT(matriz: List[Int], borrar: Int, fila: Int, col: Int): List[Int] = {
      val f = borrar/col
      val c = borrar%col
      val primPos = (c - 3).max(0)
      val ultPos = (c + 4).min(col)
      val filas = concatenar(filasNegativas(f, fila, 0), deja(1, filasPositivas(f, fila, 0)))
      val matrizFinal = borrarSubmatriz(matriz, 0, primPos, ultPos, filas, fila, col)
      matrizFinal
    }

    // Devuelve las filas mayores de f que se van a borrar
    private def filasPositivas(f: Int, fila: Int, a: Int): List[Int] = {
      a match {
        case _ if f > fila || a == 4 => Nil
        case _ => f :: filasPositivas(f + 1, fila, a + 1)
      }
    }

    // Devuelve las filas menores de f que se van a borrar
    private def filasNegativas(f: Int, fila: Int, a: Int): List[Int] = {
      a match {
        case _ if f < 0 || a == 4 => Nil
        case _ => f :: filasNegativas(f - 1, fila, a + 1)
      }
    }

    // Funcion que pone a 0 todos los valores de la submatriz
    private def borrarSubmatriz(matriz: List[Int], a: Int, pPos: Int, ultPos: Int, filas: List[Int], fila: Int, col: Int): List[Int] = {
      a match {
        case x if x == fila => Nil
        case _ => val ff = getFila(a, matriz, col)
          if (contiene(a, filas)) {
            val filaFinal = concatenar(concatenar(toma(pPos, ff), generarMatrizVacia(ultPos - pPos)), deja(ultPos, ff))
            concatenar(filaFinal, borrarSubmatriz(matriz, a + 1, pPos, ultPos, filas, fila, col))
          } else {
            concatenar(ff, borrarSubmatriz(matriz, a + 1, pPos, ultPos, filas, fila, col))
          }
      }
    }

    // Elimina todos los valores igual a valor en la matriz
    private def eliminarTodos(matriz: List[Int], valor: Int): List[Int] = {
      matriz match {
        case Nil => Nil
        case head :: tail if head == valor => 0 :: eliminarTodos(tail, valor)
        case head :: tail => head :: eliminarTodos(tail, valor)
      }
    }

    // Funcion que establece en la posición pos el valor
    private def ponerValor(matriz: List[Int], pos: Int, valor: Int): List[Int] ={
      pos match{
        case 0 => valor::matriz.tail
        case _ => matriz.head::ponerValor(matriz.tail,pos-1,valor)
      }
    }

    // Devuelve la posición de la matriz que borra un mayor número de bloques
    private def mejorPosicion(matriz: List[Int], posActual: Int, mejorPos: Int, numMejorPos: Int, fila: Int, col: Int): (Int,Int) = {
      posActual match {
        case x if x == fila * col => (mejorPos,numMejorPos)
        case _ => val elem = getElem(posActual, matriz)
          if (elem >= 7) {
            elem match {
              case 7 => val random = new Random().nextInt(2)
                if (random == 0) {
                  // Borrar fila
                  if (fila > numMejorPos) {
                    mejorPosicion(matriz, posActual + 1, posActual, fila, fila, col)
                  } else {
                    mejorPosicion(matriz, posActual + 1, mejorPos, numMejorPos, fila, col)
                  }
                } else {
                  // Borrar Col
                  if (col > numMejorPos) {
                    mejorPosicion(matriz, posActual + 1, posActual, col, fila, col)
                  } else {
                    mejorPosicion(matriz, posActual + 1, mejorPos, numMejorPos, fila, col)
                  }
                }
              case 8 => val f = posActual / col
                val c = posActual % col
                val primPos = (c - 3).max(0)
                val ultPos = (c + 4).min(col)
                val filas = concatenar(filasNegativas(f, fila, 0), deja(1, filasPositivas(f, fila, 0)))
                val colBorrar = ultPos - primPos
                val nFilas = length(filas)
                val total = colBorrar * nFilas
                if (total > numMejorPos) {
                  mejorPosicion(matriz, posActual + 1, posActual, total, fila, col)
                } else {
                  mejorPosicion(matriz, posActual + 1, mejorPos, numMejorPos, fila, col)
                  mejorPosicion(matriz, posActual + 1, mejorPos, numMejorPos, fila, col)
                }
              case _ => val num = elem - 8
                val numElem = contarElem(matriz, num, 0)
                val total = numElem + 1
                if (total > numMejorPos) {
                  mejorPosicion(matriz, posActual + 1, posActual, total, fila, col)
                } else {
                  mejorPosicion(matriz, posActual + 1, mejorPos, numMejorPos, fila, col)
                }
            }
          } else {
            val borrado = borrarPos(matriz, posActual, fila, col)
            val nElim = borrado._2
            if (nElim > numMejorPos) {
              mejorPosicion(matriz, posActual + 1, posActual, nElim, fila, col)
            } else {
              mejorPosicion(matriz, posActual + 1, mejorPos, numMejorPos, fila, col)
            }
          }
      }
    }

    // Devuelve el número de veces que aparece el elemento num en la lista matriz
    @tailrec
    private def contarElem(matriz: List[Int], num: Int, total: Int): Int = {
      matriz match {
        case Nil => total
        case head :: tail if head == num => contarElem(tail, num, total + 1)
        case _ :: tail => contarElem(tail, num, total)
      }
    }

    // Devuelve la longitud de la lista l
    private def length(l: List[Int]): Int = {
      l match {
        case Nil => 0
        case _ => 1 + length(l.tail)
      }
    }

    // Imprime n elementos consecutivos de la lista l
    @tailrec
    private def imprimirNElem(l: List[Int], n: Int): Unit = {
      n match {
        case 0 => print("|"); println()
        case _ if l.head >= 7 =>
          l.head match {
            case 7 => print(s"|B ")
            case 8 => print(s"|T ")
            case 9 => print(s"|R1")
            case 10 => print(s"|R2")
            case 11 => print(s"|R3")
            case 12 => print(s"|R4")
            case 13 => print(s"|R5")
            case 14 => print(s"|R6")
          }
          imprimirNElem(l.tail, n - 1)
        case _ => print(s"|${l.head} "); imprimirNElem(l.tail, n - 1)
      }
    }

    // Devuelve una lista con los n primeros elementos de l
    private def toma(n: Int, l: List[Int]): List[Int] = {
      n match {
        case 0 => Nil
        case _ => l.head :: toma(n - 1, l.tail)
      }
    }

    // Devuelve una lista sin los n primeros elementos de l
    @tailrec
    private def deja(n: Int, l: List[Int]): List[Int] = {
      n match {
        case 0 => l
        case _ => deja(n - 1, l.tail)
      }
    }

    // Devuelve el elemento de la posición index de la matriz
    def getElem(index: Int, matriz: List[Int]): Int = {
      index match {
        case _ if matriz == Nil => throw new Error("El indice excede la longitud de la matriz")
        case 0 => matriz.head
        case _ => getElem(index - 1, matriz.tail)
      }
    }

    // Función que realiza la traspuesta de la matriz
    private def traspuesta(matriz: List[Int], col: Int): List[Int] = {
      trasp_aux(matriz, 0, col)
    }

    // Función auxiliar que traspone una matriz concatenando las columnas
    private def trasp_aux(matriz: List[Int], columnaActual: Int, col: Int): List[Int] = {
      columnaActual match {
        case x if x == col => Nil
        case _ => concatenar(getColumna(columnaActual, matriz, col), trasp_aux(matriz, columnaActual + 1, col))
      }
    }

    // Devuelve la fila fila de la matriz
    private def getFila(fila: Int, matriz: List[Int], col: Int): List[Int] = {
      toma(col, deja(fila * col, matriz))
    }

    // Devuelve la columna columna de la matriz
    private def getColumna(columna: Int, matriz: List[Int], col: Int): List[Int] = {
      matriz match {
        case Nil => Nil
        case _ => getElem(columna, matriz) :: getColumna(columna, deja(col, matriz), col)
      }
    }

    // Devuelve true si el elem esta en la lista l,
    // false en caso contrario
    @tailrec
    private def contiene(elem: Int, l: List[Int]): Boolean = {
      l match {
        case Nil => false
        case _ if l.head == elem => true
        case _ => contiene(elem, l.tail)
      }
    }

    // Concatena l1 y l2
    private def concatenar(l1: List[Int], l2: List[Int]): List[Int] = {
      l1 match {
        case Nil => l2
        case head :: tail => head :: concatenar(tail, l2)
      }
    }
  }