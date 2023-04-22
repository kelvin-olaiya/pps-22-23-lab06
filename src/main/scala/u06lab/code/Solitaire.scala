package u06lab.code

import scala.annotation.targetName

object Solitaire extends App:
  type Position = (Int, Int)
  type Solution = Seq[Position]
  val HORIZONTAL_RIGHT = (3, 0)
  val VERTICAL_DOWN = (0, 3)
  val DIAGONAL_DOWN_RIGHT = (2, -2)
  val DIAGONAL_UP_RIGHT = (2, 2)
  val moves = List(HORIZONTAL_RIGHT, DIAGONAL_DOWN_RIGHT, DIAGONAL_UP_RIGHT, VERTICAL_DOWN)
    .flatMap((a, b) => List((a, b), (-a, -b)))
  type IterableFactory = Solution => Iterable[Solution]
  given IterableFactory = LazyList(_).view

  def render(solution: Seq[Position], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for i <- 0 until height
          row = for j <- 0 until width
                    number = reversed.indexOf((i, j)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def solutions(width: Int, height: Int)(n: Int = width * height)(using factory: IterableFactory): Iterable[Solution] =
    n match
      case 1 => factory(List((width/2, height/2)))
      case _ =>
        for
          solution <- solutions(width, height)(n - 1)
          moves = possibleMovesFrom(solution).filter(isInBoard(_, width, height))
          if moves.nonEmpty
          move <- moves
        yield move +: solution

  def possibleMovesFrom(solution: Solution): Iterable[Position] =
    moves.map(solution.head + _).filter(pos => !(solution contains pos))

  def isInBoard(position: Position, width: Int, height: Int): Boolean =
    ((0 until height) contains position._1) && ((0 until width) contains position._2)

  extension(position: Position)
    @targetName("plus")
    def +(other: Position): Position = (position._1 + other._1, position._2 + other._2)

  for
    solution <- solutions(5, 5)()
  do
    println(render(solution, 5, 5))
    println("---------------")
