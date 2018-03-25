package info.daviot.tictactoe

import com.truelaurel.algorithm.game._
import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.gomoku.{GomokuBoard, GomokuRules}
import info.daviot.tictactoe.UltimateBoard._

case class UltimateBoard(smallBoards: Map[Pos, GomokuBoard] = Map.empty,
                         lastMove: Option[Pos] = None,
                         lastPlayer: Boolean = true)
    extends GameState[Boolean] {


    def play(pos: Pos): UltimateBoard = {
        forcePlay(pos, !lastPlayer)
    }

    private def forcePlay(pos: Pos, player: Boolean): UltimateBoard = {
        val key = pos / 3
        val oldSmallBoard = boardToPlay(pos)
        val smallBoard = oldSmallBoard.forcePlay(pos % 3, player)
        copy(
            lastMove = Some(pos),
            smallBoards = smallBoards.updated(key, smallBoard),
            lastPlayer = player)
    }

    def validMoves: Set[Pos] = {
        lastMove.map { last =>
            val next = last % 3
            val smallBoard = smallBoards.getOrElse(next, emptySmallBoard)
            if (isFinished(smallBoard)) allFreePositions
            else validMoves(next, smallBoard)
        }.getOrElse(allValidMoves)
    }.toSet

    def gameResult: Outcome[Boolean] = {
        def wonBy(player: Boolean) = smallBoards.collect {
            case (pos, board) if hasWon(board, player) => (pos, player)
        }

        val allWonBoards = wonBy(true) ++ wonBy(false)
        val metaBoard = allWonBoards.foldLeft(emptySmallBoard) {
            case (board, (pos, winner)) => board.forcePlay(pos, winner)
        }
        if (hasWon(metaBoard, true)) Wins(true)
        else if (hasWon(metaBoard, false)) Wins(false)
        else if (smallBoards.values.count(isFinished) == 9) Draw
        else Undecided
    }

    private def validMoves(last: Pos, smallBoard: GomokuBoard): Seq[Pos] = {
        val startRow = last.x * 3
        val startCol = last.y * 3
        for {
            row <- startRow until startRow + 3
            col <- startCol until startCol + 3
            pos = Pos(row, col)
            if smallBoard.isFree(pos % 3)
        } yield pos
    }

    private def allFreePositions: Seq[Pos] =
        for {
            pos <- allValidMoves
            b = boardToPlay(pos)
            if b.isFree(pos % 3) && !isFinished(b)
        } yield pos


    private def boardToPlay(p: Pos): GomokuBoard =
        smallBoards.getOrElse(p / 3, emptySmallBoard)


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

    def nextPlayer: Boolean = !lastPlayer
}


object UltimateBoard {
    def fromString(boardString: String) = {
        val allLines = boardString.split("\n")
        val lines9 = allLines.take(3) ++ allLines.slice(4, 7) ++ allLines.slice(8, 11)
        val lines = lines9.map(line => line.take(3) ++ line.slice(4, 7) ++ line.slice(8, 11))
        allValidMoves.foldLeft(UltimateBoard()) {
            case (board, pos) =>
                lines(pos.x)(pos.y) match {
                    case '.' => board
                    case `firstPlayerChar` => board.forcePlay(pos, false)
                    case `secondPlayerChar` => board.forcePlay(pos, true)
                }
        }
    }

    val rules = GomokuRules(3, 3)

    val firstPlayerChar = 'X'
    val secondPlayerChar = 'O'

    val emptySmallBoard = GomokuBoard(3)

    val separatorLine: String = "-" * 11

    val allValidMoves: Seq[Pos] = for {
        row <- 0 to 8
        col <- 0 to 8
    } yield Pos(row, col)

    private def isFinished(smallBoard: GomokuBoard): Boolean =
        hasWon(smallBoard, true) || hasWon(smallBoard, false) || smallBoard.free.isEmpty

    private def hasWon(smallBoard: GomokuBoard, player: Boolean) =
        rules.hasWon(smallBoard, player)
}
