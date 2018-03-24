package info.daviot.tictactoe

import com.truelaurel.math.geometry.Pos
import org.scalatest.{FlatSpec, Matchers}

class UltimateBoardTest extends FlatSpec with Matchers {
  val emptyBoard = UltimateBoard()

  "board" should "produce 81 initial valid moves" in {
    val moves = emptyBoard.validMoves
    moves should have size 81
  }

  it should "produce 9 valid moves after first play" in {
    val moves = emptyBoard.play(Pos(1, 0)).validMoves
    moves shouldBe Set(
      Pos(3, 0), Pos(3, 1), Pos(3, 2),
      Pos(4, 0), Pos(4, 1), Pos(4, 2),
      Pos(5, 0), Pos(5, 1), Pos(5, 2)
    )
  }

  it should "produce 8 valid moves after first play at 0,0" in {
    val moves = emptyBoard.play(Pos(0, 0)).validMoves
    moves shouldBe Set(
      Pos(0, 1), Pos(0, 2),
      Pos(1, 0), Pos(1, 1), Pos(1, 2),
      Pos(2, 0), Pos(2, 1), Pos(2, 2)
    )
  }

}
