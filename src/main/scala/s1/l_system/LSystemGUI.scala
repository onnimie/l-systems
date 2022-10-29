package s1.l_system

import scala.swing.*
import scala.swing.Swing.*
import java.awt.Color
import scala.swing.event._

class LSystemWindow() extends MainFrame:
  title = "L-system"
  contents = new Panel:
    background    = Color.white
    preferredSize = (600, 600)

    // can be fun to play with inside PaintComponent - especially as extra input to drawFractal
    var mouseX = 0
    var mouseY = 0

    override def paintComponent(g: Graphics2D): Unit =
      super.paintComponent(g)

      // g.setColor(Color.GREEN)
      // g.drawLine(300, 550, 300, 100)

      iterationEnding = LSystem.drawFractalPieceByPiece(g, 20, charactersToDrawFromLatestIteration)
      //LSystem.drawFractal(g, 20)

    end paintComponent

    this.listenTo(mouse.moves)

    this.reactions += {
      case event.MouseMoved(_, point: java.awt.Point, _) =>
        mouseX = point.x
        mouseY = point.y
    }


  var iterationEnding = false
  var charactersToDrawFromLatestIteration = 1

  val listener = ActionListener( (e: java.awt.event.ActionEvent) => {
    // timer stuff here
    repaint()
    charactersToDrawFromLatestIteration += 1
    if iterationEnding then
      iterationEnding = false
      charactersToDrawFromLatestIteration = 1

  })

  val timer = javax.swing.Timer(1, listener)
  timer.start()

  centerOnScreen()

end LSystemWindow

object LSystemGUI extends SimpleSwingApplication:
  lazy val top = LSystemWindow()