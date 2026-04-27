package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotTest extends AnyFlatSpec with Matchers:

  val DEFAULT_INITIAL_POSITION: (Int, Int) = (0, 0)
  val DEFAULT_INITIAL_DIRECTION: Direction = Direction.North
  val DIRECTION_DIFFERENT_FROM_DEFAULT: Direction = Direction.East

  "A SimpleRobot" should "turn correctly" in:
    val robot =
      new SimpleRobot(DEFAULT_INITIAL_POSITION, DEFAULT_INITIAL_DIRECTION)

    robot.turn(Direction.East)
    robot.direction should be(Direction.East)

    robot.turn(Direction.South)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West)
    robot.direction should be(Direction.West)

    robot.turn(Direction.North)
    robot.direction should be(Direction.North)

  it should "act correctly" in:
    val robot =
      new SimpleRobot(DEFAULT_INITIAL_POSITION, DEFAULT_INITIAL_DIRECTION)

    robot.act()
    robot.position should be((0, 1))

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((1, 1))

    robot.turn(Direction.South)
    robot.act()
    robot.position should be((1, 0))

    robot.turn(Direction.West)
    robot.act()
    robot.position should be((0, 0))

  "A RobotWithBattery" should "have a correct initial battery level" in:
    val robot = RobotWithBattery(
      SimpleRobot(DEFAULT_INITIAL_POSITION, DEFAULT_INITIAL_DIRECTION)
    )
    robot.battery should be(robot.INITIAL_BATTERY_VALUE)

  "A RobotWithBattery battery level" should "decrease on turn to a different direction" in:
    val robot = RobotWithBattery(
      SimpleRobot(DEFAULT_INITIAL_POSITION, DEFAULT_INITIAL_DIRECTION)
    )
    robot.turn(DIRECTION_DIFFERENT_FROM_DEFAULT)
    robot.battery should be(
      robot.INITIAL_BATTERY_VALUE - robot.BATTERY_DECREASE_ON_ACTION
    )

  "A RobotWithBattery battery level" should "not decrease on turn to the same current direction" in:
    val robot = RobotWithBattery(
      SimpleRobot(DEFAULT_INITIAL_POSITION, DEFAULT_INITIAL_DIRECTION)
    )
    robot.turn(DEFAULT_INITIAL_DIRECTION)
    robot.battery should be(robot.INITIAL_BATTERY_VALUE)

  "A RobotWithBattery battery level" should "decrease on act" in:
    val robot = RobotWithBattery(
      SimpleRobot(DEFAULT_INITIAL_POSITION, DEFAULT_INITIAL_DIRECTION)
    )
    robot.turn(DIRECTION_DIFFERENT_FROM_DEFAULT)
    robot.act()
    robot.battery should be(
      robot.INITIAL_BATTERY_VALUE - 2 * robot.BATTERY_DECREASE_ON_ACTION
    )

  "A RobotWithBattery" should "be unable to turn to a different direction if the battery is low" in:
    val robot = RobotWithBattery(
      SimpleRobot(DEFAULT_INITIAL_POSITION, DEFAULT_INITIAL_DIRECTION)
    )
    while robot.battery != 0 do robot.act()
    a[IllegalStateException] should be thrownBy robot.turn(
      DIRECTION_DIFFERENT_FROM_DEFAULT
    )

  "A RobotWithBattery" should "be unable to act if the battery is low" in:
    val robot = RobotWithBattery(
      SimpleRobot(DEFAULT_INITIAL_POSITION, DEFAULT_INITIAL_DIRECTION)
    )
    while robot.battery != 0 do robot.act()
    a[IllegalStateException] should be thrownBy robot.act()
