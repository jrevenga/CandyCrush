import scala.io.StdIn.readLine
import scala.util.Random
import scala.swing._
import scala.swing.event._

object Game {
  var removedBlocksCount = 0

  def main(args: Array[String]): Unit = {
    println("Elija el modo de ejecución (m: manual, a: automático):")
    val mode = readLine().toLowerCase

    println("Elija el nivel de dificultad (1: fácil, 2: difícil):")
    val difficultyLevel = readLine().toInt

    println("Ingrese el número de filas del tablero:")
    val numRows = readLine().toInt

    println("Ingrese el número de columnas del tablero:")
    val numColumns = readLine().toInt

    val board = generateBoard(numRows, numColumns, difficultyLevel)
    printBoard(board)

    mode match {
      case "m" => manualMode(board, difficultyLevel)
      case "a" => automaticMode(board, difficultyLevel)
    }
  }

  def generateBoard(numRows: Int, numColumns: Int, difficultyLevel: Int): Array[Array[String]] = {
    Array.fill(numRows, numColumns) {
      val color = (difficultyLevel match {
        case 1 => Random.nextInt(4) + 1
        case 2 => Random.nextInt(6) + 1
      }).toString
      color
    }
  }

  def printBoard(board: Array[Array[String]]): Unit = {
    print("   ")
    for (j <- board(0).indices) {
      print(f"$j%3d")
    }
    println()

    print("   ")
    for (_ <- board(0).indices) {
      print("---")
    }
    println()

    for (i <- board.indices) {
      print(f"$i%2d|")
      for (j <- board(i).indices) {
        print(f"${board(i)(j)}%3s")
      }
      println()
    }
    println()
  }

  def manualMode(board: Array[Array[String]], difficultyLevel: Int): Unit = {
    while (true) {
      println("Ingrese la posición [fila, columna] que desea seleccionar (separadas por un espacio):")
      val input = readLine().split(" ").map(_.toInt)
      val row = input(0)
      val column = input(1)

      if (row >= 0 && row < board.length && column >= 0 && column < board(0).length) {
        val selectedBlock = board(row)(column)

        selectedBlock match {
          case "B" =>
            removeRowOrColumn(board, row, column)
            printBoard(board)
            updateBoard(board, difficultyLevel)
          case "T" =>
            removeTNTArea(board, row, column)
            printBoard(board)
            updateBoard(board, difficultyLevel)
          case block if block.startsWith("R") =>
            val colorToRemove = block.last.toString
            removeAllColorBlocks(board, colorToRemove)
            printBoard(board)
            updateBoard(board, difficultyLevel)
          case _ =>
            removeAdjacentBlocks(board, row, column, selectedBlock)

            val specialBlock = generateSpecialBlock(removedBlocksCount)
            if (specialBlock != "") {
              board(row)(column) = specialBlock
            }

            printBoard(board)
            removedBlocksCount = 0 // Reset removed blocks counter
            updateBoard(board, difficultyLevel)
        }

        printBoard(board)
      } else {
        println("Posición inválida. Por favor, ingrese una posición válida.")
      }
    }
  }

  def automaticMode(board: Array[Array[String]], difficultyLevel: Int): Unit = {
    while (true) {
      val row = Random.nextInt(board.length)
      val column = Random.nextInt(board(0).length)

      val selectedBlock = board(row)(column)

      selectedBlock match {
        case "B" =>
          removeRowOrColumn(board, row, column)
          printBoard(board)
          updateBoard(board, difficultyLevel)
        case "T" =>
          removeTNTArea(board, row, column)
          printBoard(board)
          updateBoard(board, difficultyLevel)
        case block if block.startsWith("R") =>
          val colorToRemove = block.last.toString
          removeAllColorBlocks(board, colorToRemove)
          printBoard(board)
          updateBoard(board, difficultyLevel)
        case _ =>
          removeAdjacentBlocks(board, row, column, selectedBlock)

          val specialBlock = generateSpecialBlock(removedBlocksCount)
          if (specialBlock != "") {
            board(row)(column) = specialBlock
          }

          printBoard(board)
          removedBlocksCount = 0 // Reset removed blocks counter
          updateBoard(board, difficultyLevel)
      }

      printBoard(board)
      Thread.sleep(1000)
    }
  }

  def removeAdjacentBlocks(board: Array[Array[String]], row: Int, column: Int, color: String): Unit = {
    if (row >= 0 && row < board.length && column >= 0 && column < board(0).length && board(row)(column) == color) {
      board(row)(column) = " "
      removedBlocksCount += 1

      removeAdjacentBlocks(board, row - 1, column, color)
      removeAdjacentBlocks(board, row + 1, column, color)
      removeAdjacentBlocks(board, row, column - 1, color)
      removeAdjacentBlocks(board, row, column + 1, color)
    }
  }

  def generateSpecialBlock(removedBlocksCount: Int): String = {
    removedBlocksCount match {
      case x if x >= 10 => "R" + (Random.nextInt(6) + 1).toString
      case x if x >= 7 => "T"
      case x if x >= 5 => "B"
      case _ => ""
    }
  }

  def updateBoard(board: Array[Array[String]], difficultyLevel: Int): Unit = {
    for (j <- 0 until board(0).length) {
      var emptySpaces = 0
      for (i <- board.length - 1 to 0 by -1) {
        if (board(i)(j) == " ") {
          emptySpaces += 1
        } else if (emptySpaces > 0) {
          board(i + emptySpaces)(j) = board(i)(j)
          board(i)(j) = " "
        }
      }

      // Rellenar posiciones vacías con bloques aleatorios
      for (i <- 0 until emptySpaces) {
        val color = (difficultyLevel match {
          case 1 => Random.nextInt(4) + 1
          case 2 => Random.nextInt(6) + 1
        }).toString
        board(i)(j) = color
      }
    }
  }

  def removeRowOrColumn(board: Array[Array[String]], row: Int, column: Int): Unit = {
    val removeRow = Random.nextBoolean()
    if (removeRow) {
      for (j <- 0 until board(0).length) {
        board(row)(j) = " "
      }
    } else {
      for (i <- 0 until board.length) {
        board(i)(column) = " "
      }
    }
  }

  def removeTNTArea(board: Array[Array[String]], row: Int, column: Int): Unit = {
    val radius = 4
    for (i <- row - radius to row + radius) {
      for (j <- column - radius to column + radius) {
        if (i >= 0 && i < board.length && j >= 0 && j < board(0).length) {
          board(i)(j) = " "
        }
      }
    }
  }

  def removeAllColorBlocks(board: Array[Array[String]], color: String): Unit = {
    for (i <- 0 until board.length) {
      for (j <- 0 until board(0).length) {
        if (board(i)(j) == color) {
          board(i)(j) = " "
        }
      }
    }
  }
}
