enum Direction {
  case NORTH, SOUTH, EAST, WEST, NORTHEAST, NORTHWEST, SOUTHEAST, SOUTHWEST

  def reverse: Direction = this match {
    case Direction.NORTH      => Direction.SOUTH
    case Direction.SOUTH      => Direction.NORTH
    case Direction.EAST       => Direction.WEST
    case Direction.WEST       => Direction.EAST
    case Direction.NORTHEAST  => Direction.SOUTHWEST
    case Direction.NORTHWEST  => Direction.SOUTHEAST
    case Direction.SOUTHEAST  => Direction.NORTHWEST
    case Direction.SOUTHWEST  => Direction.NORTHEAST
  }
}