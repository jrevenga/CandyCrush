import java.awt.{Color, Dimension}
import javax.imageio.ImageIO
import javax.swing.ImageIcon
import scala.swing._
import scala.swing.event.ButtonClicked

object GuiProgramFive {
  def main(args: Array[String]): Unit = {
    val window = new VentanaInicio
    window.visible = true
  }
}

class ImageLabel(path: String) extends Label {
  private val url = getClass.getResource(path)
  private val imageIcon = new ImageIcon(url).getImage.getScaledInstance(800, 800, java.awt.Image.SCALE_SMOOTH)
  icon = new ImageIcon(imageIcon)
  horizontalAlignment = Alignment.Center
}

class VentanaInicio extends MainFrame {
  preferredSize = new Dimension(800, 800)

  private val label = new ImageLabel("/portadaI.png")

  private val button = new Button("Inicio") {
    preferredSize = new Dimension(200, 100)
    reactions += {
      case ButtonClicked(_) =>
        val ini = System.nanoTime()
        val juego = new VentanaDatos(ini)
        juego.visible = true
        dispose()
    }
  }

  contents = new BorderPanel {
    layout(label) = BorderPanel.Position.Center
    layout(button) = BorderPanel.Position.South
  }

  pack()
  centerOnScreen()  // Centrar la ventana en la pantalla
}

class ImagePanel(imagePath: String) extends BoxPanel(Orientation.Vertical) {
  private val image = ImageIO.read(getClass.getResource(imagePath)).getScaledInstance(800, 800, java.awt.Image.SCALE_SMOOTH)

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    g.drawImage(image, 0, 0, null)
  }
}

class VentanaDatos(ini: Long) extends MainFrame {
  preferredSize = new Dimension(800, 800)
  resizable = false
  val col = new TextField("10")
  val fila = new TextField("10")
  private val labelCol = new Label("columnas")
  private val labelFila = new Label("filas")

  private val ejecucion = new Label("Sistema de ejecucion: ")
  private val status1Ej = new RadioButton("automatica")
  private val status2Ej = new RadioButton("manual")
  status2Ej.selected = true
  private val statusGroupEje = new ButtonGroup(status1Ej, status2Ej)
  val nivel = new Label("Seleccionar Nivel:")
  private val status1N = new RadioButton("1")
  private val status2N = new RadioButton("2")
  status1N.selected = true
  private val statusGroupNivel = new ButtonGroup(status1N, status2N)

  contents = new ImagePanel("/portada.png") {
    contents += Swing.VStrut(380)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += nivel
      contents += Swing.HStrut(10)
      contents += status1N
      contents += Swing.HStrut(10)
      contents += status2N
      contents += Swing.HStrut(300)
    }
    contents += Swing.VStrut(20)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += ejecucion
      contents += Swing.HStrut(5)
      contents += status2Ej
      contents += Swing.HStrut(10)
      contents += status1Ej
      contents += Swing.HStrut(230)
    }
    contents += Swing.VStrut(20)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HStrut(300)
      contents += labelFila
      contents += Swing.HStrut(10)
      contents += fila
      contents += Swing.HStrut(20)
      contents += labelCol
      contents += Swing.HStrut(10)
      contents += col
      contents += Swing.HStrut(290)
    }
    contents += Swing.VStrut(235)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += Button("Aceptar") {
        dispose()

        val ejecucion = tipoEjecucion()
        val nivel = tipoNivel()

        val f = fila.text.toInt
        val c = col.text.toInt

        // Generar la matriz
        val matrizVacia = new GameLogic().generarMatrizVacia(f * c)
        val matriz = new GameLogic().rellenarMatriz(matrizVacia, nivel)

        // Generar la nueva ventana
        if (ejecucion == 1) {
          // Juego manual
          val juego = new VentanaJuegoManual(matriz, nivel, f, c, 5, 0, ini)
          juego.visible = true
        } else {
          // Juego automatico
          val juego = new VentanaJuegoAutomatico(matriz, nivel, f, c, 5, 0, ini)
          juego.visible = true
        }
      }
      contents += Swing.HStrut(270)
      contents += Button("Cerrar") {
        sys.exit(0)
      }
    }

    for (e <- contents)
      e.xLayoutAlignment = 0.0
    border = Swing.EmptyBorder(10, 10, 10, 10)
  }

  private def tipoEjecucion(): Int = {
    if (statusGroupEje.selected.get.text == "automatica") {0} else {1}
  }

  private def tipoNivel(): Int = {
    if (statusGroupNivel.selected.get.text == "1") {1} else {2}
  }

  centerOnScreen() // Centrar la ventana en la pantalla
}

class VentanaJuegoManual(matriz: List[Int], nivel: Int, fila: Int, col: Int, vidas: Int, puntuacion: Int, ini: Long) extends MainFrame {
  preferredSize = new Dimension(800, 800)
  title = "Juego manual"
  // Crear el panel de la cuadrícula
  private val gridPanelPrincipal = generarGrid(matriz, fila, col)

  // Crear el botón de cerrar ventana
  private val closeButton = new Button("Cerrar") {
    reactions += {
      case ButtonClicked(_) =>
        sys.exit(0) // Cierra la aplicación al hacer clic en el botón de cerrar
    }
  }

  private val labelVidas = new Label(s"Vidas:  $vidas \n")

  // Establecer el diseño de la ventana con el panel de la cuadrícula y el botón de cerrar
  contents = new BorderPanel {
    layout(gridPanelPrincipal) = BorderPanel.Position.Center
    layout(closeButton) = BorderPanel.Position.South
    layout(labelVidas) = BorderPanel.Position.North
  }

  private def generarGrid(matriz: List[Int], fila: Int, col: Int): GridPanel ={
    val gridPanel = new GridPanel(fila, col) {
      for (f <- 0 until fila; c <- 0 until col) {
        val index = f * col + c
        val color = new GameLogic().getElem(index, matriz) match {
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
        button.action = Action(" ") {
          dispose()
          val pos = index
          val juegoManu = new GameLogic().juegoManual(matriz, fila, col, nivel, vidas, pos, puntuacion)
          val matrizFinal = juegoManu._1
          val vidasFinal = juegoManu._2
          val puntuacionFinal = juegoManu._3
          if (vidasFinal == 0) {
            //Fin del juego
            val res = Dialog.showConfirmation(contents.head,
              "GAME OVER\n" +
                s"Puntación: $puntuacionFinal",
              optionType = Dialog.Options.Default,
              title = title)
            // Enviar datos y multiplicar por el nivel
            if (res == Dialog.Result.Ok)
              sys.exit(0)
          } else {
            val otraVentana = new VentanaJuegoManual(matrizFinal, nivel, fila, col, vidasFinal, puntuacionFinal, ini)
            otraVentana.visible = true
          }
        }
        contents += button
      }
    }
    gridPanel
  }
  centerOnScreen() // Centrar la ventana en la pantalla
}

class VentanaJuegoAutomatico(matriz: List[Int], nivel: Int, fila: Int, col: Int, vidas: Int, puntuacion: Int, ini: Long) extends MainFrame {
  preferredSize = new Dimension(800, 800)
  title = "Juego automatico"
  // Crear el panel de la cuadrícula
  private val gridPanelPrincipal = generarGrid(matriz, fila, col)

  // Crear el botón de cerrar ventana
  private val closeButton = new Button("Cerrar") {
    reactions += {
      case ButtonClicked(_) =>
        sys.exit(0) // Cierra la aplicación al hacer clic en el botón de cerrar
    }
  }

  private val labelVidas = new Label(s"Vidas:  $vidas  \n")

  // Establecer el diseño de la ventana con el panel de la cuadrícula y el botón de cerrar
  contents = new BorderPanel {
    layout(gridPanelPrincipal) = BorderPanel.Position.Center
    layout(new BoxPanel (Orientation.Horizontal) {
      val botonSig = new Button()
      botonSig.action = Action("Siguiente") {
        val juegoAuto = new GameLogic().juegoAutomatico(matriz, fila, col, nivel, vidas, puntuacion)

        val matrizFinal = juegoAuto._1
        val vidasFinal = juegoAuto._2
        val puntuacionFinal = juegoAuto._3

        if (vidasFinal == 0) {
          //Fin del juego
          val res = Dialog.showConfirmation(new Label,
            "¡HAS PERDIDO!\n" +
              s"Su puntuación es: $puntuacionFinal",
            optionType = Dialog.Options.Default,
            title = "Fin del juego")
          // Enviar datos y multiplicar por el nivel
          if (res == Dialog.Result.Ok)
            sys.exit(0)
        } else {
          dispose()
          val ventana = new VentanaJuegoAutomatico(matrizFinal, nivel, fila, col, vidasFinal, puntuacionFinal, ini)
          ventana.visible = true
        }
      }
      contents += Swing.HGlue
      contents += botonSig
      contents += Swing.HStrut(280)
      contents += closeButton
    }) = BorderPanel.Position.South
    layout(labelVidas) = BorderPanel.Position.North
  }

  private def generarGrid(matriz: List[Int], fila: Int, col: Int): GridPanel = {
    val gridPanel = new GridPanel(fila, col) {
      for (f <- 0 until fila; c <- 0 until col) {
        val index = f * col + c
        val color = new GameLogic().getElem(index, matriz) match {
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
    gridPanel
  }
  centerOnScreen() // Centrar la ventana en la pantalla
}
