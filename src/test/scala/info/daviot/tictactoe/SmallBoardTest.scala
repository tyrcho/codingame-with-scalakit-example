package info.daviot.tictactoe

import com.github.writethemfirst.approvals.Approver
import com.truelaurel.math.geometry.Pos
import org.scalatest.{FlatSpec, Matchers}

class SmallBoardTest extends FlatSpec with Matchers {
    val emptyBoard = UltimateBoard()

    "small board" should "allow 2 moves for same player" in {
        val played = SmallBoard()
            .forcePlay(Pos(0, 1), true)
            .forcePlay(Pos(0, 2), true)
        new Approver().writeTo("smallBoard").verify(played.toText)
    }

    it should "give correct outcome" in {
        val moves = Seq(Pos(0, 0), Pos(1, 2), Pos(1, 1), Pos(2, 1), Pos(2, 2))
        moves.zipWithIndex.foldLeft(SmallBoard()){
            case (board, (move,i))=> val next=board.play(move)

            next
        }
    }
}
