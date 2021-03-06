package info.daviot.tictactoe

import com.github.writethemfirst.approvals.utils.functions.Function1
import com.github.writethemfirst.approvals.{Approver, CombinationApprover}
import com.truelaurel.algorithm.game.{Draw, Undecided}
import com.truelaurel.math.geometry.Pos
import org.scalatest.{FlatSpec, Matchers}

class UltimateBoardTest extends FlatSpec with Matchers with Approving {
    val emptyBoard = UltimateBoard()

    "board" should "produce 81 initial valid moves" in {
        val moves = emptyBoard.validMoves
        moves should have size 81
    }



    it should "produce valid moves after first plays" in {
        verifyAll(
            "initial valid moves",
            Seq(Pos(0, 0), Pos(1, 0), Pos(7, 1), Pos(4, 4)),
            (p: Pos) => emptyBoard.play(p).validMoves.toSeq.sortBy(Pos.unapply))
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

        verifyMoves(moves, "validMovesAfterCornerCompleted")
    }



    it should "read from strings" in {
        val boardString =
            """X.O|.OX|O.X
              |OXO|OOX|XO.
              |XOX|OO.|XXX
              |-----------
              |X..|XOO|.XO
              |OXO|XXO|X.O
              |..X|O.O|XXX
              |-----------
              |OXO|XXO|O.O
              |OXX|XXO|O.O
              |XOX|.OX|OX.""".stripMargin
        val board = UltimateBoard.fromString(boardString)
        new Approver().writeTo("read from string").verify(board.debugString)
    }

    it should "allow to play anywhere when sub board is full" in {
        val boardString =
            """.OX|XOX|.OO
              |XOO|O.X|XOO
              |.OX|OXO|.OX
              |-----------
              |XXO|X.X|OX.
              |OX.|OOX|XOX
              |OXO|XOX|..O
              |-----------
              |OOX|X.X|OOX
              |XXO|XOX|.XO
              |OXO|O.O|XX.""".stripMargin

        val board = UltimateBoard.fromString(boardString).copy(lastMove = Some(Pos(2, 3)))
        verifyMoves(board.validMoves, "subBoard full")
    }

    it should "initially be undecided" in {
        UltimateBoard().gameResult shouldBe Undecided
    }

    it should "detect draw" in {
        val boardString =
            """OOX|XXX|XOX
              |XOX|OO.|OXO
              |OXO|.X.|OXO
              |-----------
              |XXO|O.X|O.X
              |OXX|X.X|OOO
              |OXO|OOO|XOX
              |-----------
              |XX.|OXX|.OO
              |.X.|OXO|.OX
              |..X|O.O|XXX""".stripMargin

        val board = UltimateBoard.fromString(boardString)

        board.gameResult shouldBe Draw
    }


}
