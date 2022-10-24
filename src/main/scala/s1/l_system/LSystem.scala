package s1.l_system

import java.awt.{BasicStroke, Color}
import scala.collection.mutable.Buffer
import scala.swing.Graphics2D

val DEBUG = true

case class State(pos: Vector2D, base: Vector2D, width: Int)

object LSystem:

  val rules: Char => String = {
    case 'A' => "B[A]A"
    case 'B' => "BB"
    case other => other.toString
  }

  val lStrings = LazyList.iterate("A")(_.flatMap(rules))

  def drawFractal(g: Graphics2D, depth: Int) =
    val strokes    = Buffer.tabulate(depth + 1)(BasicStroke(_))
    val states     = collection.mutable.Stack[State]()
    var position   = Vector2D(300, 550)
    var base       = Vector2D(0, -8)
    var width      = depth
    val LeafRadius = 6

    val tree = LSystem.lStrings(depth)

    if DEBUG then println(tree)

    g.setColor(Color.GRAY)
    g.setStroke(strokes(width))

    for
      piece <- tree
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

end LSystem
