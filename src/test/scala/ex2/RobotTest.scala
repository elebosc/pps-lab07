package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotTest extends AnyFlatSpec with Matchers:

  val DEFAULT_INITIAL_POSITION: (Int, Int) = (0, 0)
  val DEFAULT_INITIAL_DIRECTION: Direction = Direction.North
  val DIRECTION_DIFFERENT_FROM_DEFAULT: Direction = Direction.East
  val POSITION_AFTER_ONE_STEP_IN_DEFAULT_DIRECTION: (Int, Int) = (0, 1)
  val POSITION_AFTER_TWO_STEPS_IN_DEFAULT_DIRECTION: (Int, Int) = (0, 2)
  val SEED = 0
  val FAILURE_CERTAIN = 1
  val FAILURE_NONE = 0
  val REPETITIONS_NUMBER = 2

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
      DEFAULT_INITIAL_POSITION,
      DEFAULT_INITIAL_DIRECTION
    )
    robot.battery should be(robot.INITIAL_BATTERY_VALUE)

  "A RobotWithBattery battery level" should "decrease on turn to a different direction" in:
    val robot = RobotWithBattery(
      DEFAULT_INITIAL_POSITION,
      DEFAULT_INITIAL_DIRECTION
    )
    robot.turn(DIRECTION_DIFFERENT_FROM_DEFAULT)
    robot.battery should be(
      robot.INITIAL_BATTERY_VALUE - robot.BATTERY_DECREASE_ON_ACTION
    )

  "A RobotWithBattery battery level" should "not decrease on turn to the same current direction" in:
    val robot = RobotWithBattery(
      DEFAULT_INITIAL_POSITION,
      DEFAULT_INITIAL_DIRECTION
    )
    robot.turn(DEFAULT_INITIAL_DIRECTION)
    robot.battery should be(robot.INITIAL_BATTERY_VALUE)

  "A RobotWithBattery battery level" should "decrease on act" in:
    val robot = RobotWithBattery(
      DEFAULT_INITIAL_POSITION,
      DEFAULT_INITIAL_DIRECTION
    )
    robot.turn(DIRECTION_DIFFERENT_FROM_DEFAULT)
    robot.act()
    robot.battery should be(
      robot.INITIAL_BATTERY_VALUE - 2 * robot.BATTERY_DECREASE_ON_ACTION
    )

  "A RobotWithBattery" should "be unable to turn to a different direction if the battery is low" in:
    val robot = RobotWithBattery(
      DEFAULT_INITIAL_POSITION,
      DEFAULT_INITIAL_DIRECTION
    )
    while robot.battery != 0 do robot.act()
    val prevPosition = robot.position
    robot.turn(DIRECTION_DIFFERENT_FROM_DEFAULT)
    robot.position should be(prevPosition)

  "A RobotWithBattery" should "be unable to act if the battery is low" in:
    val robot = RobotWithBattery(
      DEFAULT_INITIAL_POSITION,
      DEFAULT_INITIAL_DIRECTION
    )
    while robot.battery != 0 do robot.act()
    val prevPosition = robot.position
    robot.act()
    robot.position should be(prevPosition)

  "A RobotCanFail" should "always fail to turn if failure is certain" in:
    val robot = RobotCanFail(
      DEFAULT_INITIAL_POSITION,
      DEFAULT_INITIAL_DIRECTION,
      FAILURE_CERTAIN,
      SEED
    )
    robot.turn(DIRECTION_DIFFERENT_FROM_DEFAULT)
    robot.direction should be(DEFAULT_INITIAL_DIRECTION)

  "A RobotCanFail" should "always fail to ACT if failure is certain" in:
    val robot = RobotCanFail(
      DEFAULT_INITIAL_POSITION,
      DEFAULT_INITIAL_DIRECTION,
      FAILURE_CERTAIN,
      SEED
    )
    robot.act()
    robot.position should be(DEFAULT_INITIAL_POSITION)

  "A RobotCanFail" should "never fail to turn if failure is impossible" in:
    val robot = RobotCanFail(
      DEFAULT_INITIAL_POSITION,
      DEFAULT_INITIAL_DIRECTION,
      FAILURE_NONE,
      SEED
    )
    robot.turn(DIRECTION_DIFFERENT_FROM_DEFAULT)
    robot.direction should be(DIRECTION_DIFFERENT_FROM_DEFAULT)

  "A RobotCanFail" should "never fail to act if failure is impossible" in:
    val robot = RobotCanFail(
      DEFAULT_INITIAL_POSITION,
      DEFAULT_INITIAL_DIRECTION,
      FAILURE_NONE,
      SEED
    )
    robot.act()
    robot.position should be(POSITION_AFTER_ONE_STEP_IN_DEFAULT_DIRECTION)

  "A RobotRepeated" should "perform acts a specified number of times" in:
    val robot = RobotRepeated(
      DEFAULT_INITIAL_POSITION,
      DEFAULT_INITIAL_DIRECTION,
      REPETITIONS_NUMBER
    )
    robot.act()
    robot.position should be(POSITION_AFTER_TWO_STEPS_IN_DEFAULT_DIRECTION)
