package info.daviot.tictactoe

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.gomoku.GomokuBoard

case class UltimateBoard(smallBoards: Map[Pos, GomokuBoard] = Map.empty,
                         lastMove: Option[Pos] = None,
                         lastPlayer: Boolean = true) {


    def play(pos: Pos): UltimateBoard = {
        val key = pos / 3
        val oldSmallBoard = boardToPlay(pos)
        val smallBoard = oldSmallBoard.forcePlay(pos % 3, !lastPlayer)
        copy(
            lastMove = Some(pos),
            smallBoards = smallBoards.updated(key, smallBoard),
            lastPlayer = !lastPlayer)
    }

    def validMoves: Set[Pos] =
        (lastMove match {
            case None => for {
                row <- 0 to 8
                col <- 0 to 8
            } yield Pos(row, col)
            case Some(last) =>
                val smallBoard = nextSmallBoard.get
                for {
                    row <- last.x * 3 until (1 + last.x) * 3
                    col <- last.y * 3 until (1 + last.y) * 3
                    pos = Pos(row, col)
                    if smallBoard.isFree(pos % 3)
                } yield pos
        }).toSet


    private def boardToPlay(p: Pos): GomokuBoard =
        smallBoards.getOrElse(p / 3, emptySmallBoard)

    private def nextSmallBoard: Option[GomokuBoard] =
        lastMove.map { lastPos =>
            smallBoards.getOrElse(lastPos % 3, emptySmallBoard)
        }

    val emptySmallBoard = GomokuBoard(3)

    val separatorLine: String = "-" * 11

    def debugString: String =
        (((0 to 2).map(debugRow) :+ separatorLine) ++
            ((3 to 5).map(debugRow) :+ separatorLine) ++
            (6 to 8).map(debugRow)).mkString("\n")


    def debugRow(row: Int): String =
        (((0 to 2).map(debugPos(row)) :+ '|') ++
            ((3 to 5).map(debugPos(row)) :+ '|') ++
            (6 to 8).map(debugPos(row))).mkString

    def debugPos(row: Int)(col: Int): Char = {
        val pos = Pos(row, col)
        val board = smallBoards.getOrElse(pos / 3, emptySmallBoard)
        val smallPos = pos % 3
        if (board.isFree(smallPos)) '.'
        else if (board.playedFalse(smallPos)) firstPlayerChar
        else secondPlayerChar
    }

    val firstPlayerChar = 'X'
    val secondPlayerChar = 'O'
}
