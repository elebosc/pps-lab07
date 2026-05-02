package ex3

trait SolitaireADT:
  type Position
  type Move
  type Solution
  val HORIZONTAL_DELTA = 3
  val VERTICAL_DELTA = 3
  val DIAGONAL_DELTA = 2
  val ALLOWED_MOVES: Set[Move]
  def placeMarks(): LazyList[Solution]
  def render(solution: Solution): String

class Solitaire(val width: Int, val height: Int) extends SolitaireADT:
  override type Position = (Int, Int)
  override type Move = Position => Position
  override type Solution = List[Position]
  override val ALLOWED_MOVES: Set[Move] = Set(
    p => (p._1 - HORIZONTAL_DELTA, p._2), // Left
    p => (p._1, p._2 - VERTICAL_DELTA), // Up
    p => (p._1 + HORIZONTAL_DELTA, p._2), // Right
    p => (p._1, p._2 + VERTICAL_DELTA), // Down
    p => (p._1 - DIAGONAL_DELTA, p._2 - DIAGONAL_DELTA), // Up-Left
    p => (p._1 + DIAGONAL_DELTA, p._2 - DIAGONAL_DELTA), // Up-Right
    p => (p._1 + DIAGONAL_DELTA, p._2 + DIAGONAL_DELTA), // Bottom-Right
    p => (p._1 - DIAGONAL_DELTA, p._2 + DIAGONAL_DELTA) // Bottom-Left
  )

  private def isPositionValid(p: Position): Boolean =
    p._1 >= 0 && p._1 < width && p._2 >= 0 && p._2 < height

  override def placeMarks(): LazyList[Solution] =
    def _placeMarks(currentSolution: List[Position]): LazyList[Solution] =
      if currentSolution.length == width * height then LazyList(currentSolution)
      else
        val currentPosition = currentSolution.head
        for
          move <- LazyList.from(ALLOWED_MOVES)
          nextPosition = move(currentPosition)
          if isPositionValid(nextPosition) && !currentSolution.contains(nextPosition)
          subSolution <- _placeMarks(nextPosition :: currentSolution)
        yield subSolution
    val startingPosition = (width / 2, height / 2)
    _placeMarks(List(startingPosition))

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
    val width: Int = 5
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
