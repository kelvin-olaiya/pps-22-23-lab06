package u06lab.code

object GameCore:
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  import Player.*

  case class Placement(x: Int, y: Int, player: Player)
  type Board = Seq[Placement]
  type Game = Seq[Board]

import GameCore.*

trait TwoPlayerBoardGame:
  val BOARD_SIZE: Int

  def find(board: Board, x: Int, y: Int): Option[Player] =
    board.collectFirst { case Placement(`x`, `y`, player) => player }

  def anyPlacement(board: Board, player: Player): Seq[Board]

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList[Game](List(List()))
    case _ =>
      for
        game <- computeAnyGame(player.other, moves - 1)
        previousBoard = game.head
        if !previousBoard.isWinning
        board <- anyPlacement(previousBoard, player)
      yield board +: game

  def winningStrategy(board: Board): Boolean =
    board.exists(disk =>
      List((0, 1), (1, 0), (1, 1), (-1, 1)).map(offset => List(offset, (-offset._1, -offset._2)))
        .map(direction => direction.map(position => find(board, position._1 + disk.x, position._2 + disk.y)))
        .exists(direction => direction.forall(player => player.isDefined && player.get == disk.player))
    )

  extension (board: Board)
    def isWinning: Boolean = winningStrategy(board)

  def printBoards(game: Seq[Board]): Unit =
    for
      y <- (BOARD_SIZE - 1) to 0 by -1
      board <- game.reverse
      x <- 0 until BOARD_SIZE
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == (BOARD_SIZE - 1) then
        print(" ")
        if board == game.head then println()
