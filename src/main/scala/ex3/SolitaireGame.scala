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

  private def isValidPosition(p: Position): Boolean =
    p._1 >= 0 && p._1 < width && p._2 >= 0 && p._2 < height

  override def placeMarks(): LazyList[Solution] =
    def _placeMarks(solution: List[Position]): LazyList[Solution] =
      if solution.length == width * height then LazyList(solution)
      else
        val currentPos = solution.head
        for
          move <- LazyList.from(allowedMoves)
          nextPos = move(currentPos)
          if isValidPosition(nextPos) && !solution.contains(nextPos)
          subSolution <- _placeMarks(nextPos :: solution)
        yield subSolution
    val startingPos = (width / 2, height / 2)
    _placeMarks(List(startingPos))

  override def render(solution: Solution): String =
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
    val width: Int = 7
    val height: Int = 5
    val solitaire = Solitaire(width, height)
    var count = 0
    solitaire.placeMarks().foreach { solution =>
      count += 1
      println(s"Solution $count")
      println(solitaire.render(solution))
      println()
    }
    if count == 0 then println("No solutions found.")
    else println(s"Total solutions found: $count")
