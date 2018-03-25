package info.daviot.tictactoe

import com.truelaurel.math.geometry.Pos
import org.scalatest.{FlatSpec, Matchers}

class SmallBoardTest extends FlatSpec with Matchers with Approving {
    "small board" should "allow 2 moves for same player" in {
        val played = SmallBoard()
            .forcePlay(Pos(0, 1), player = true)
            .forcePlay(Pos(0, 2), player = true)

        verify(played.toText, "smallBoard")
    }

    it should "give correct outcome" in {
        val moves = Seq(Pos(0, 0), Pos(1, 2), Pos(1, 1), Pos(2, 1), Pos(2, 2))
        moves.zipWithIndex.foldLeft(SmallBoard()) {
            case (board, (move, i)) =>
                val next = board.play(move)
                verify(next.toText + next.outcome, "outcome" + i)
                next
        }
    }

    it should "have correct free" in {
        val moves = Seq(Pos(0, 0), Pos(1, 2), Pos(1, 1), Pos(2, 1), Pos(2, 2))
        val board = moves.foldLeft(SmallBoard()) {
            case (b, move) => b.play(move)
        }
        verifyAll("free", SmallBoard.allPos, board.isFree)
    }
}
