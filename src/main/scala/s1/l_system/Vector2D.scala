package s1.l_system

case class Vector2D(x: Double, y: Double):
  def +(other: Vector2D) = Vector2D(this.x + other.x, this.y + other.y)
  def scale(s: Double) = Vector2D(this.x * s, this.y * s)
  def rotate(alpha: Double) =
    Vector2D(
      this.x * math.cos(alpha) - this.y * math.sin(alpha),
      this.x * math.sin(alpha) + this.y * math.cos(alpha)
    )
end Vector2D
