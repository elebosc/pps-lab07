package ex2

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East  => Direction.South
    case Direction.South => Direction.West
    case Direction.West  => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West  => Direction.South
    case Direction.South => Direction.East
    case Direction.East  => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction)
    extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East  => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West  => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

/*
 * Robot with battery
 */
class RobotWithBattery(val robot: Robot)
    extends SimpleRobot(robot.position, robot.direction):

  val INITIAL_BATTERY_VALUE = 100
  val BATTERY_DECREASE_ON_ACTION = 5
  var battery: Int = INITIAL_BATTERY_VALUE

  private def decreaseBatteryLevel(): Unit =
    battery -= BATTERY_DECREASE_ON_ACTION

  override def turn(dir: Direction): Unit =
    if dir != direction then
      if (battery != 0) then
        super.turn(dir)
        decreaseBatteryLevel()
      else throw IllegalStateException("Battery is low")

  override def act(): Unit =
    if (battery != 0) then
      super.act()
      decreaseBatteryLevel()
    else throw IllegalStateException("Battery is low")

@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East
