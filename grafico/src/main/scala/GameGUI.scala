import java.awt.{Color, Dimension}
import javax.imageio.ImageIO
import javax.swing.ImageIcon
import scala.swing._
import scala.swing.event.{ButtonClicked, MouseClicked}

object GuiProgramFive {
  val windowInicio = new VentanaInicio
  var windowConfig: VentanaConfig = null
  def main(args: Array[String]): Unit = {
    windowInicio.visible = true
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
        GuiProgramFive.windowConfig = new VentanaConfig(ini)
        GuiProgramFive.windowConfig.visible = true
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

class VentanaConfig(ini: Long) extends MainFrame {
  preferredSize = new Dimension(800, 800)
  resizable = false
  val filaTextField = new TextField("10")
  val columnaTextField = new TextField("10")
  private val labelFila = new Label("Filas")
  private val labelCol = new Label("Columnas")

  private val ejecucion = new Label("Sistema de ejecucion: ")
  private val ejAuto = new RadioButton("automatica")
  private val ejManual = new RadioButton("manual")
  ejManual.selected = true
  private val statusGroupEjecucion = new ButtonGroup(ejAuto, ejManual)
  val dificultad = new Label("Seleccionar Nivel:")
  private val status1Level = new RadioButton("1")
  private val status2Level = new RadioButton("2")
  status1Level.selected = true
  private val statusGroupNivel = new ButtonGroup(status1Level, status2Level)

  contents = new ImagePanel("/portada.png") {
    contents += Swing.VStrut(380)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += dificultad
      contents += Swing.HStrut(10)
      contents += status1Level
      contents += Swing.HStrut(10)
      contents += status2Level
      contents += Swing.HStrut(300)
    }
    contents += Swing.VStrut(20)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += ejecucion
      contents += Swing.HStrut(5)
      contents += ejManual
      contents += Swing.HStrut(10)
      contents += ejAuto
      contents += Swing.HStrut(230)
    }
    contents += Swing.VStrut(20)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HStrut(300)
      contents += labelFila
      contents += Swing.HStrut(10)
      contents += filaTextField
      contents += Swing.HStrut(20)
      contents += labelCol
      contents += Swing.HStrut(10)
      contents += columnaTextField
      contents += Swing.HStrut(290)
    }
    contents += Swing.VStrut(235)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += Swing.HGlue
      contents += Button("Aceptar") {
        dispose()
        val ejecucion = selecEjec()
        val dificultad = selecDificultad()
        val fila = filaTextField.text.toInt
        val columna = columnaTextField.text.toInt
        val matrizCeros = new GameLogic().inicializarTablero(fila * columna)
        val matriz = new GameLogic().rellena(matrizCeros, dificultad)
        new GameLogic().mostrarTablero(matriz,columna)
        val juego = new VentanaGame(matriz, dificultad, fila, columna, 5, ejecucion)
        juego.visible = true
      }
      contents += Swing.HStrut(270)
      contents += Button("Salir") {
        GuiProgramFive.windowInicio.visible = true
        dispose()
      }
    }
    for (e <- contents)
      e.xLayoutAlignment = 0.0
    border = Swing.EmptyBorder(10, 10, 10, 10)
  }
  private def selecEjec(): Int = {
    if (statusGroupEjecucion.selected.get.text == "automatica") {0} else {1}
  }
  private def selecDificultad(): Int = {
    if (statusGroupNivel.selected.get.text == "1") {1} else {2}
  }
  centerOnScreen() // Centrar la ventana en la pantalla
}

class VentanaGame(matriz: List[Int], dificultad: Int, filaTextField: Int, columnaTextField: Int, vidas: Int, ejecucion: Int) extends MainFrame {
  val logic = new GameLogic()
  var imageMap: Map[Int, javax.swing.ImageIcon] = Map()
  val blockSize = 600 / Math.max(filaTextField, columnaTextField)
  for (i <- 1 to 15) {
    val imagePath = s"/candy$i.png"
    val url = getClass.getResource(imagePath)
    if (url != null) {
      val originalImage = new javax.swing.ImageIcon(url).getImage
      val scaledImage = originalImage.getScaledInstance(blockSize, blockSize, java.awt.Image.SCALE_SMOOTH)
      imageMap += (i -> new javax.swing.ImageIcon(scaledImage))
    }
  }
  preferredSize = new Dimension(800, 800)
  private val gridPanelPrincipal = generarTablero(matriz, filaTextField, columnaTextField)
  private val closeButton = new Button("Salir") {
    reactions += {
      case ButtonClicked(_) =>
        GuiProgramFive.windowConfig.visible = true
        dispose()
    }
  }

  val labelVidas = new ProgressBar {
    min = 0
    max = 5 // El valor máximo de vidas
    value = vidas // El valor actual de vidas
    labelPainted = true // Mostrar el valor numérico en la barra de progreso
    foreground = Color.RED
  }


  contents = new BorderPanel {
    layout(gridPanelPrincipal) = BorderPanel.Position.Center
    if(ejecucion == 0){
      layout(new BoxPanel(Orientation.Horizontal) {
        val botonSig = new Button()
        botonSig.action = Action("Siguiente") {
          val (mejorPos, borrados) = logic.mejorPosicion(matriz, 0, -1, -1, filaTextField, columnaTextField)
          val juegoAuto = logic.game(matriz, filaTextField, columnaTextField, dificultad, vidas, mejorPos)

          val matrizFinal = juegoAuto._1
          val vidasFinal = juegoAuto._2

          if (vidasFinal == 0) {
            //Fin del juego
            val res = Dialog.showConfirmation(new Label,
              "¡HAS PERDIDO!\n",
              optionType = Dialog.Options.Default,
              title = "Fin del juego")
            // Enviar datos y multiplicar por el dificultad
            if (res == Dialog.Result.Ok)
              sys.exit(0)
          } else {
            dispose()
            val ventana = new VentanaGame(matrizFinal, dificultad, filaTextField, columnaTextField, vidasFinal, ejecucion)
            ventana.visible = true
          }
        }
        contents += Swing.HGlue
        contents += botonSig
        contents += Swing.HStrut(280)
        contents += closeButton
      }) = BorderPanel.Position.South
    };
    if (ejecucion == 1) {
      layout(closeButton) = BorderPanel.Position.South
    }
    layout(labelVidas) = BorderPanel.Position.North
  }

  private def generarTablero(matriz: List[Int], filaTextField: Int, columnaTextField: Int): GridBagPanel = {
    val tablero = new GridBagPanel {
      for (fila <- 0 until filaTextField; columna <- 0 until columnaTextField) {
        val indice = fila * columnaTextField + columna
        val imageIndex = new GameLogic().getElem(indice, matriz)
        val imageIcon = imageMap.get(imageIndex) match {
          case Some(image) => image
          case None =>
            imageMap.getOrElse(15, null) // Usando la imagen 15 como imagen por defecto
        }
        if (imageIcon != null) {
          val label = new Label {
            preferredSize = new Dimension(blockSize, blockSize)
            icon = imageIcon

            if(ejecucion == 1){
              listenTo(mouse.clicks)
              reactions += {
                case _: MouseClicked => {
                  dispose()
                  val pos = indice
                  val juegoManu = new GameLogic().game(matriz, filaTextField, columnaTextField, dificultad, vidas, pos)
                  val matrizFinal = juegoManu._1
                  val vidasFinal = juegoManu._2
                  if (vidasFinal == 0) {
                    //Fin del juego
                    val res = Dialog.showConfirmation(contents.head,
                      "            GAME OVER\n",
                      optionType = Dialog.Options.Default,
                      title = title)
                    // Enviar datos y multiplicar por el dificultad
                    if (res == Dialog.Result.Ok)
                      sys.exit(0)
                  } else {
                    val otraVentana = new VentanaGame(matrizFinal, dificultad, filaTextField, columnaTextField, vidasFinal, ejecucion)
                    otraVentana.visible = true
                  }
                }
              }
            }
          }
          val c = new Constraints
          c.gridx = columna
          c.gridy = fila
          layout(label) = c
        } else {
          println(s"No se pudo cargar la imagen para el índice $indice")
        }
      }
    }
    tablero
  }

  centerOnScreen() // Centrar la ventana en la pantalla
}