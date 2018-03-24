package info.daviot.tictactoe

import com.github.writethemfirst.approvals.Approver
import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.gomoku.GomokuBoard
import org.scalatest.{FlatSpec, Matchers}

class UltimateBoardTest extends FlatSpec with Matchers {
    val emptyBoard = UltimateBoard()

    "small board" should "allow 2 moves for same player" in {
        val played = GomokuBoard(3)
            .forcePlay(Pos(0, 1), true)
            .forcePlay(Pos(0, 2), true)
        new Approver().writeTo("smallBoard").verify(played.toText)
    }

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

    val movesToWinOnFirstCorner = Seq(Pos(3, 2), Pos(1, 7), Pos(5, 4), Pos(6, 4), Pos(2, 4),
        Pos(7, 4), Pos(4, 4), Pos(5, 3), Pos(6, 0), Pos(2, 2), Pos(7, 6),
        Pos(5, 2), Pos(6, 6), Pos(0, 2), Pos(0, 6), Pos(1, 2))


    it should "debug properly" in {
        movesToWinOnFirstCorner.zipWithIndex.foldLeft(emptyBoard) {
            case (board, (pos, i)) =>
                val next = board.play(pos)
                new Approver().writeTo("properDebug" + (i + 1)).verify(next.debugString)
                next
        }
    }

    it should "allow to play anywhere when sent on a completed small board" in {
        val boardWithFirstCornerDone = movesToWinOnFirstCorner.foldLeft(emptyBoard) {
            case (board, pos) => board.play(pos)
        }

        val moves = boardWithFirstCornerDone
            .play(Pos(4, 8))
            .play(Pos(3, 8))
            .play(Pos(1, 6))
            .play(Pos(5, 1))
            .play(Pos(6, 3))
            .validMoves

        new Approver().writeTo("validMovesAfterCornerCompleted").verify(
            moves.toList.sortBy(Pos.unapply).mkString("\n"))
    }

}
