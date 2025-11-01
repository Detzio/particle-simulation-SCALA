import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import scala.util.Random

object Particle {
  private def randomColor(): Color =
    Color(Random.nextDouble(), Random.nextDouble(), Random.nextDouble(), 1.0)

  def randomDirection(): Direction = {
    val vals = Direction.values
    vals(Random.nextInt(vals.length))
  }
}

case class Particle(x: Int, y: Int, dir: Direction = Particle.randomDirection(), color: Color = Particle.randomColor()) {

  def draw: Circle = new Circle {
    centerX = x
    centerY = y
    radius = 3
    fill = color
  }

  def nextPositionFor(d: Direction, maxWidth: Int, maxHeight: Int, step: Int = 4): (Int, Int) = {
    val (dx, dy) = d match {
      case Direction.NORTH      => (0, -step)
      case Direction.SOUTH      => (0, step)
      case Direction.EAST       => (step, 0)
      case Direction.WEST       => (-step, 0)
      case Direction.NORTHEAST  => (step, -step)
      case Direction.NORTHWEST  => (-step, -step)
      case Direction.SOUTHEAST  => (step, step)
      case Direction.SOUTHWEST  => (-step, step)
    }

    val nextXRaw = x + dx
    val nextYRaw = y + dy

    val wrappedX =
      if (nextXRaw < 0) maxWidth
      else if (nextXRaw > maxWidth) 0
      else nextXRaw

    val wrappedY =
      if (nextYRaw < 0) maxHeight
      else if (nextYRaw > maxHeight) 0
      else nextYRaw

    (wrappedX, wrappedY)
  }

  def withPositionAndDir(nx: Int, ny: Int, nd: Direction): Particle =
    copy(x = nx, y = ny, dir = nd)

}