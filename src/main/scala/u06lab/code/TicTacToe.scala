package u06lab.code

object TicTacToe extends TwoPlayerBoardGame with App:
  import GameCore.{Board, Placement, Player}
  import GameCore.Player.*
  override val BOARD_SIZE = 3

  private def availableRows(board: Board, x: Int): Seq[Int] =
    (0 until BOARD_SIZE).diff(board collect { case Placement(`x`, y, _) => y })

  override def anyPlacement(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 until BOARD_SIZE
      y <- availableRows(board, x)
    yield Placement(x, y, player) +: board

  computeAnyGame(O, 9).foreach { g =>
    printBoards(g)
    println()
  }
