package s1.l_system

import java.awt.{BasicStroke, Color}
import scala.collection.mutable.Buffer
import scala.swing.Graphics2D

val DEBUG = false

case class State(pos: Vector2D, base: Vector2D, width: Int)

object LSystem:

  val rules: Char => String = {
    case 'A' => "B[A]A"
    case 'B' => "BB"
    case 'F' => "F+F-F-F+F"
    case 'S' => "S+P"
    case 'P' => "S-P"
    case other => other.toString
  }

  val STARTER = "S"
  val lStrings = LazyList.iterate(STARTER)(_.flatMap(rules))

  def drawFractal(g: Graphics2D, depth: Int) =
    val strokes    = Buffer.tabulate(depth + 1)(BasicStroke(_))
    val states     = collection.mutable.Stack[State]()
    var position   = Vector2D(300, 550)
    var base       = Vector2D(0, -8)
    var width      = depth
    val LeafRadius = 6

    val tree = LSystem.lStrings(depth)

    if DEBUG then println(tree)

    drawPiecesWithRulesFromString(tree, depth, g)

  end drawFractal


  def drawFractalPieceByPiece(g: Graphics2D, depth: Int, chars: Int): Boolean = //return true if drawing the last char of an iteration


    val latestIteration: String = LSystem.lStrings(depth)

    if DEBUG then println(latestIteration)

    drawPiecesWithRulesFromString(latestIteration.take(chars), depth, g)

    if latestIteration.length - 1 == chars then true else false

  end drawFractalPieceByPiece

  def drawPiecesWithRulesFromString(s: String, depth: Int, g: Graphics2D) =

    val states     = collection.mutable.Stack[State]()
    var position   = Vector2D(300, 550)
    val strokes = Buffer.tabulate(depth + 1)(BasicStroke(_))
    var width = depth
    var base = Vector2D(0, -8)
    val LeafRadius = 6

    g.setColor(Color.GRAY)
    g.setStroke(strokes(width))

    for
      piece <- s
    do
      piece match
        case 'A' =>
          g.setColor(Color.GREEN)
          g.fillOval((position.x - LeafRadius).toInt, (position.y - LeafRadius).toInt, LeafRadius * 2, LeafRadius * 2)
          g.setColor(Color.GRAY)

        case 'B' =>
          val newPosition = position + base
          g.drawLine(position.x.toInt, position.y.toInt, newPosition.x.toInt, newPosition.y.toInt)
          position = newPosition

        case '[' =>
          states.push(State(position, base, width))
          width -= 1
          g.setStroke(strokes(width))
          base = base.rotate(-math.Pi / 4)

        case ']' =>
          val state: State = states.pop()
          base     = state.base.rotate(math.Pi / 4)
          position = state.pos
          width    = state.width - 1
          g.setStroke(strokes(width))

        case 'F' =>
          g.setColor(Color.RED)
          g.setStroke(strokes(1))
          val newPosition = position + base
          g.drawLine(position.x.toInt, position.y.toInt, newPosition.x.toInt, newPosition.y.toInt)
          position = newPosition
        case '+' =>
          base = base.rotate(-math.Pi / 2)
        case '-' =>
          base = base.rotate(math.Pi / 2)

        case 'S' =>
          g.setColor(Color.GREEN)
          g.setStroke(strokes(1))
          val newPosition = position + base
          g.drawLine(position.x.toInt, position.y.toInt, newPosition.x.toInt, newPosition.y.toInt)
          position = newPosition
        case 'P' =>
          g.setColor(Color.GREEN)
          g.setStroke(strokes(1))
          val newPosition = position + base
          g.drawLine(position.x.toInt, position.y.toInt, newPosition.x.toInt, newPosition.y.toInt)
          position = newPosition
    end for
  end drawPiecesWithRulesFromString


end LSystem
