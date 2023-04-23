package u06lab.code

object TicTacToe extends App:
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X
  import Player.*

  case class Placement(x: Int, y: Int, player: Player)

  type Board = Seq[Placement]
  type Game = Seq[Board]

  def find(board: Board, x: Int, y: Int): Option[Player] = board.find(disk => disk.x == x && disk.y == y).map(_.player)

  def availableRows(board: Board, x: Int): Seq[Int] = (0 until 3).diff(board.collect { case Placement(`x`, y, _) => y })

  def anyPlacement(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 until 3
      y <- availableRows(board, x)
    yield Placement(x, y, player) +: board

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList[Game](List(List()))
    case _ =>
      (for
        game <- computeAnyGame(player.other, moves - 1)
        previousBoard = game.head
        board <- anyPlacement(previousBoard, player)
      yield if previousBoard.isWinning then game else board +: game).distinct


  extension (board: Board)
    def isWinning: Boolean =
      board.exists(disk =>
        List((0, 1), (1, 0), (1, 1), (-1, 1)).map(offset => List(offset, (-offset._1, -offset._2)))
          .map(direction => direction.map(position => find(board, position._1 + disk.x, position._2 + disk.y)))
          .exists(direction => direction.forall(player => player.isDefined && player.get == disk.player))
      )

  def printBoards(game: Seq[Board]): Unit =
    for
      y <- 2 to 0 by -1
      board <- game.reverse
      x <- 0 to 2
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == 2 then
        print(" ")
        if board == game.head then println()

  computeAnyGame(O, 9).foreach { g =>
    printBoards(g)
    println()
  }