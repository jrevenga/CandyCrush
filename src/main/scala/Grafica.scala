import juego.{Funciones, FuncionesAux}
import Amazon.Amazon
import Google.Google
import JSON_Google.JSON_Google

import java.awt.{Color, GridBagConstraints}
import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, Swing, _}
import java.awt.Color
import scala.swing._
import scala.swing.event._
import javax.swing._
import java.awt.Insets
import javax.swing.ImageIcon
import scala.swing.MenuBar.NoMenuBar.contents
import scala.swing._
import scala.swing.event.ButtonClicked
import scala.swing.ProgressBar //Progreso de las vidas


class VentanaDatos(ini: Long) extends MainFrame {
  title = "Configuración del juego"
  resizable = false
  background = new Color(0, 189, 161) // Color pastel

  peer.setLocationRelativeTo(null) // Centrar en la pantalla

  val nombreJuego = new TextField { columns = 15 }

  val modelCol = new SpinnerNumberModel(1, 1, Int.MaxValue, 1)
  val modelFila = new SpinnerNumberModel(1, 1, Int.MaxValue, 1)

  val col = Component.wrap(new JSpinner(modelCol))
  val fila = Component.wrap(new JSpinner(modelFila))

  val status1Ej = new RadioButton("Automática") {
    background = new Color(0, 189, 161) // Color pastel
  }
  val status2Ej = new RadioButton("Manual") {
    background = new Color(0, 189, 161) // Color pastel
  }
  status1Ej.selected = true
  val statusGroupEje = new scala.swing.ButtonGroup(status1Ej, status2Ej)

  val status1N = new RadioButton("1") {
    background = new Color(0, 189, 161) // Color pastel
  }
  val status2N = new RadioButton("2") {
    background = new Color(0, 189, 161) // Color pastel
  }
  status1N.selected = true
  val statusGroupNivel = new scala.swing.ButtonGroup(status1N, status2N)

  contents = new GridBagPanel {
    background = new Color(0, 189, 161) // Color pastel del fondo
    val c = new Constraints
    c.insets = new Insets(5, 5, 5, 5)

    c.gridx = 0
    c.gridy = 0
    layout(new Label("Nombre:")) = c

    c.gridx = 1
    c.gridy = 0
    layout(nombreJuego) = c

    c.gridx = 0
    c.gridy = 1
    layout(new Label("Ejecución:")) = c

    c.gridx = 1
    c.gridy = 1
    layout(new BoxPanel(Orientation.Horizontal) {
      contents += status1Ej
      contents += status2Ej
      background = new Color(230, 230, 230) // Color pastel
    }) = c

    c.gridx = 0
    c.gridy = 2
    layout(new Label("Nivel:")) = c

    c.gridx = 1
    c.gridy = 2
    layout(new BoxPanel(Orientation.Horizontal) {
      contents += status1N
      contents += status2N
      background = new Color(230, 230, 230) // Color pastel
    }) = c

    c.gridx = 0
    c.gridy = 3
    layout(new Label("Filas:")) = c

    c.gridx = 1
    c.gridy = 3
    layout(fila) = c

    c.gridx = 0
    c.gridy = 4
    layout(new Label("Columnas:")) = c

    c.gridx = 1
    c.gridy = 4
    layout(col) = c

    c.gridx = 0
    c.gridy = 5
    c.gridwidth = 2
    layout(Button("Aceptar") {
      if (nombreJuego.text != "") {
        dispose()

        val ejecucion = tipoEjecucion()
        val nivel = tipoNivel()

        val f = fila.peer.asInstanceOf[JSpinner].getValue.asInstanceOf[Int]
        val c = col.peer.asInstanceOf[JSpinner].getValue.asInstanceOf[Int]


        // Generar la matriz
        val matrizVacia = new Funciones().generarMatrizVacia(f * c)
        val matriz = new Funciones().rellenarMatriz(matrizVacia, nivel)

        // Generar la nueva ventana
        if (ejecucion == 1) {
          // Juego manual
          val juego = new VentanaJuegoManual(matriz, nivel, f, c, 5, 0, ini, nombreJuego.text)
          juego.visible = true
        } else {
          // Juego automatico
          //automaticoAux(matriz, nivel, f, c, 5)
          val juego = new VentanaJuegoAutomatico(matriz, nivel, f, c, 5, 0, ini, nombreJuego.text)
          juego.visible = true
        }
      }
    }) = c

    c.gridx = 0
    c.gridy = 6
    c.gridwidth = 2
    layout(Button("Cerrar") {
      sys.exit(0)
    }) = c
  }

  def tipoEjecucion(): Int = {
    if (statusGroupEje.selected.get.text == "Automática") {
      return 0
    } else {
      return 1
    }
  }

  def tipoNivel(): Int = {
    if (statusGroupNivel.selected.get.text == "1") {
      return 1
    } else {
      return 2
    }
  }

}


class VentanaJuegoManual(matriz: List[Int], nivel: Int, fila: Int, col: Int, vidas: Int, puntuacion: Int, ini: Long, nombre: String) extends MainFrame {
  title = "Juego manual"
  // Crear el panel de la cuadrícula
  val gridPanelPrincipal = generarGrid(matriz, fila, col)

  // Crear el botón de cerrar ventana
  val closeButton = new Button("Cerrar") {
    reactions += {
      case ButtonClicked(_) =>
        val datos = new Amazon
        val google = new Google
        val duracion = ((System.nanoTime() - ini) / 1000000000).toInt
        val currentDate = java.time.LocalDate.now.toString // This will give you the current date in the format 'yyyy-MM-dd'
        datos.main(nombre, puntuacion, currentDate, duracion);
        google.main(nombre, puntuacion, duracion)
        Thread.sleep(1000)


        val JSON_Google = new JSON_Google
        JSON_Google.main
        Thread.sleep(5000)
        import java.io.File
        import scala.sys.process.Process

        import scala.sys.process._

        val rutaFirebase = "C:\\Users\\PORTATIL\\AppData\\Roaming\\npm\\node_modules\\firebase-tools\\lib\\bin\\firebase.js"

        val cmd = s"cmd /c cd C:\\Users\\PORTATIL\\Desktop\\Pagina && node $rutaFirebase deploy --only hosting"

        cmd.!


        sys.exit(0) // Cierra la aplicación al hacer clic en el botón de cerrar
    }
  }

  import scala.swing._
  import scala.swing.event._








  val progressBarVidas = new ProgressBar {
    min = 0
    max = 5 // El valor máximo de vidas
    value = vidas // El valor actual de vidas
    labelPainted = true // Mostrar el valor numérico en la barra de progreso
    foreground = Color.RED
  }

  // Establecer el diseño de la ventana con el panel de la cuadrícula y el botón de cerrar
  contents = new BorderPanel {
    layout(gridPanelPrincipal) = BorderPanel.Position.Center
    layout(closeButton) = BorderPanel.Position.South
    layout(progressBarVidas) = BorderPanel.Position.North
  }


  import javax.imageio.ImageIO
  import java.awt.Toolkit
  import java.awt.Image

  def generarGrid(matriz: List[Int], fila: Int, col: Int): GridPanel = {
    val gridPanel = new GridPanel(fila, col) {
      for (f <- 0 until fila; c <- 0 until col) {
        val index = f * col + c
        val color = new FuncionesAux().getElem(index, matriz) match {
          case 1 => Color.RED
          case 2 => Color.GREEN
          case 3 => Color.BLUE
          case 4 => Color.WHITE
          case 5 => Color.YELLOW
          case 6 => Color.ORANGE
          case 7 => Color.PINK
          case 8 => Color.CYAN
          case _ => Color.BLACK // Si se proporciona un número de color no válido, se establece en negro
        }
        val button = new Button(" ")
        button.background = color

        // Agregar acción para obtener la posición de fila y columna al pulsar el botón
        button.action = scala.swing.Action(" ") {
          dispose()
          val pos = index
          val juegoManu = new Funciones().juegoManual(matriz, fila, col, nivel, vidas, pos, puntuacion)
          val matrizFinal = juegoManu._1
          val vidasFinal = juegoManu._2
          val puntuacionFinal = juegoManu._3
          if (vidasFinal == 0) {
            //Fin del juego
            val res = Dialog.showConfirmation(contents.head,
              "¡HAS PERDIDO!\n" +
                s"su puntación es: ${puntuacionFinal}",
              optionType = Dialog.Options.Default,
              title = title)
            // Enviar datos y multiplicar por el nivel
            if (res == Dialog.Result.Ok) {
              val datos = new Amazon
              val currentDate = java.time.LocalDate.now.toString // This will give you the current date in the format 'yyyy-MM-dd'

              datos.main(nombre, puntuacion, currentDate, 1000);
              Thread.sleep(10000)
              sys.exit(0)
            }
          } else {
            val otraVentana = new VentanaJuegoManual(matrizFinal, nivel, fila, col, vidasFinal, puntuacionFinal, ini, nombre)
            otraVentana.visible = true
          }
        }
        contents += button
      }
    }
    return gridPanel
  }



}

class VentanaJuegoAutomatico(matriz: List[Int], nivel: Int, fila: Int, col: Int, vidas: Int, puntuacion: Int, ini: Long, nombre: String) extends MainFrame {
  title = "Juego automatico"
  // Crear el panel de la cuadrícula
  val gridPanelPrincipal = generarGrid(matriz, fila, col)

  // Crear el botón de cerrar ventana
  val closeButton = new Button("Cerrar") {
    reactions += {
      case ButtonClicked(_) =>
        sys.exit(0) // Cierra la aplicación al hacer clic en el botón de cerrar
    }
  }

  val progressBarVidas = new ProgressBar {
    min = 0
    max = 5 // El valor máximo de vidas
    value = vidas // El valor actual de vidas
    labelPainted = true // Mostrar el valor numérico en la barra de progreso
    foreground = Color.RED
  }


  // Establecer el diseño de la ventana con el panel de la cuadrícula y el botón de cerrar
  contents = new BorderPanel {
    layout(gridPanelPrincipal) = BorderPanel.Position.Center
    layout(new BoxPanel(Orientation.Horizontal) {
      contents += closeButton
      val botonSig = new Button()
      botonSig.action = scala.swing.Action("Siguiente") {
        val juegoAuto = new Funciones().juegoAutomatico(matriz, fila, col, nivel, vidas, puntuacion)

        val matrizFinal = juegoAuto._1
        val vidasFinal = juegoAuto._2
        val puntuacionFinal = juegoAuto._3

        if (vidasFinal == 0) {
          //Fin del juego
          val res = Dialog.showConfirmation(new Label,
            "¡HAS PERDIDO!\n" +
              s"Su puntuación es: ${puntuacionFinal}",
            optionType = Dialog.Options.Default,
            title = "Fin del juego")
          // Enviar datos y multiplicar por el nivel
          if (res == Dialog.Result.Ok)
            sys.exit(0)
        } else {
          dispose()
          val ventana = new VentanaJuegoAutomatico(matrizFinal, nivel, fila, col, vidasFinal, puntuacionFinal, ini, nombre)
          ventana.visible = true
        }
      }
      contents += botonSig
    }) = BorderPanel.Position.South
    layout(progressBarVidas) = BorderPanel.Position.North
  }

  def generarGrid(matriz: List[Int], fila: Int, col: Int): GridPanel = {
    val gridPanel = new GridPanel(fila, col) {
      for (f <- 0 until fila; c <- 0 until col) {
        val index = f * col + c
        val color = new FuncionesAux().getElem(index, matriz) match {
          case 1 => Color.RED
          case 2 => Color.GREEN
          case 3 => Color.BLUE
          case 4 => Color.WHITE
          case 5 => Color.YELLOW
          case 6 => Color.ORANGE
          case 7 => Color.PINK
          case 8 => Color.CYAN
          case _ => Color.BLACK // Si se proporciona un número de color no válido, se establece en negro
        }
        val button = new Button(" ")
        button.background = color
        contents += button
      }
    }
    return gridPanel
  }
}






object GuiProgramFive {
  def main(args: Array[String]) {
    val ini = System.nanoTime()
    val juego = new VentanaDatos(ini)
    juego.visible = true
    println(s"el inicio es ini ${ini}")
  }
}







