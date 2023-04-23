package u06lab.code

import java.util.OptionalInt

// Optional!
object ConnectThree extends TwoPlayerBoardGame with App:
  import GameCore.{Board, Placement, Player}
  import GameCore.Player.*
  override val BOARD_SIZE = 4
  
  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    board.collect { case Placement(`x`, y, _) => y }.maxOption.map(_ + 1).orElse(Some(0)).filter(_ <= BOARD_SIZE)
    
  override def anyPlacement(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 until BOARD_SIZE
      y = firstAvailableRow(board, x)
      if y.isDefined
    yield Placement(x, y.get, player) +: board

  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Placement(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Placement(0, 0, X), Placement(0, 1, O), Placement(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Placement(0, 0, X), Placement(0, 1, O), Placement(0, 2, X)), 1, 1)) // None

  // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Placement(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Placement(0, 0, X), Placement(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Placement(0, 0, X), Placement(0, 1, X), Placement(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Placement(0, 0, X), Placement(0, 1, X), Placement(0, 2, X), Placement(0, 3, X)), 0)) // None
  // Exercise 2: implement placeAnyDisk such that..
  printBoards(anyPlacement(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(anyPlacement(List(Placement(0, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O
  println("EX 3: ")
// Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 6).foreach { g =>
    printBoards(g)
    println()
  }
//  .... .... .... .... ...O
//  .... .... .... ...X ...X
//  .... .... ...O ...O ...O
//  .... ...X ...X ...X ...X
//
//
// .... .... .... .... O...
// .... .... .... X... X...
// .... .... O... O... O...
// .... X... X... X... X...

// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
