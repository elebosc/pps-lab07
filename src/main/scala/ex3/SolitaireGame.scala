package ex3

trait SolitaireADT:
  type Position
  type Move
  type Solution
  val allowedMoves: Set[Move]
  def placeMarks(): Seq[Solution]
  def render(solution: Solution): String

class Solitaire(val width: Int, val height: Int) extends SolitaireADT:
  private val HORIZONTAL_DELTA = 3
  private val VERTICAL_DELTA = 3
  private val DIAGONAL_DELTA = 2

  override type Position = (Int, Int)
  override type Move = Position => Position
  override type Solution = Seq[Position]
  override val allowedMoves: Set[Move] = Set(
    p => (p._1 - HORIZONTAL_DELTA, p._2), // Left
    p => (p._1, p._2 - VERTICAL_DELTA), // Up
    p => (p._1 + HORIZONTAL_DELTA, p._2), // Right
    p => (p._1, p._2 + VERTICAL_DELTA), // Down
    p => (p._1 - DIAGONAL_DELTA, p._2 - DIAGONAL_DELTA), // UpLeft
    p => (p._1 + DIAGONAL_DELTA, p._2 - DIAGONAL_DELTA), // UpRight
    p => (p._1 + DIAGONAL_DELTA, p._2 + DIAGONAL_DELTA), // BottomRight
    p => (p._1 - DIAGONAL_DELTA, p._2 + DIAGONAL_DELTA) // BottomLeft
  )

  def placeMarks(): Seq[Solution] = ???

  def render(solution: Solution): String =
    val reversed = solution.reverse
    val rows =
      for
        y <- 0 until height
        row = for
          x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
        yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

object SolitaireGame extends App:

  @main def main(): Unit =
    val width: Int = 5
    val height: Int = 5
    val solitaire = Solitaire(width, height)
    for solution <- solitaire.placeMarks() do solitaire.render(solution)
