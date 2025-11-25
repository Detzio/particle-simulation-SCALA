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

  def nextPositionFor(direction: Direction, screenWidth: Int, screenHeight: Int, step: Int = 4): (Int, Int) = {
    val (deltaX, deltaY) = direction match {
      case Direction.NORTH      => (0, -step)
      case Direction.SOUTH      => (0, step)
      case Direction.EAST       => (step, 0)
      case Direction.WEST       => (-step, 0)
      case Direction.NORTHEAST  => (step, -step)
      case Direction.NORTHWEST  => (-step, -step)
      case Direction.SOUTHEAST  => (step, step)
      case Direction.SOUTHWEST  => (-step, step)
    }

    val nextX = x + deltaX
    val nextY = y + deltaY

    val wrappedX =
      if (nextX < 0) screenWidth
      else if (nextX > screenWidth) 0
      else nextX

    val wrappedY =
      if (nextY < 0) screenHeight
      else if (nextY > screenHeight) 0
      else nextY

    (wrappedX, wrappedY)
  }

  def withPositionAndDir(newX: Int, newY: Int, newDirection: Direction): Particle =
    copy(x = newX, y = newY, dir = newDirection)

}