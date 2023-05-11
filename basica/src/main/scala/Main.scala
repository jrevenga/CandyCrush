import scala.io.StdIn.readLine

object Main {

  def main(args: Array[String]): Unit = {
    print("Elija el modo de ejecución (m: manual, a: automático): ")
    val mode = readLine().toLowerCase
    print("Elija el nivel de dificultad (1: fácil, 2: difícil): ")
    val difficultyLevel = readLine().toInt
    print("Ingrese el número de filas del tablero: ")
    val numRows = readLine().toInt
    print("Ingrese el número de columnas del tablero: ")
    val numColumns = readLine().toInt

    val game = new GameLogic()

    //Crear tablero
    val boardIni = game.inicializarTablero(numRows * numColumns)
    val board = game.rellena(boardIni, difficultyLevel)
    game.mostrarTablero(board, numColumns)

    println("EMPIEZA EL JUEGO")

    playGame(game, board, numRows, numColumns, difficultyLevel, mode, 5)
  }

  def playGame(game: GameLogic, board: List[Int], numRows: Int, numColumns: Int, difficultyLevel: Int, mode: String, lives: Int): Unit = {
    if (lives > 0) {
      mode match {
        case "m" => {
          print("Ingrese el número de fila:")
          val fila = readLine().toInt -1
          print("Ingrese el número de columna:")
          val columna = readLine().toInt -1

          val pos = fila * numColumns + columna
          val (newBoard, newLives) = game.gameManual(board, numRows, numColumns, difficultyLevel, lives, pos)
          playGame(game, newBoard, numRows, numColumns, difficultyLevel, mode, newLives)
        }
        case "a" => {
          print("Press Enter")
          scala.io.StdIn.readLine() // Espera hasta que el usuario presione Enter
          val (newBoard, newLives) = game.gameAuto(board, numRows, numColumns, difficultyLevel, lives)
          playGame(game, newBoard, numRows, numColumns, difficultyLevel, mode, newLives)
        }
      }
    } else {
      println("Game Over")
    }
  }

}
