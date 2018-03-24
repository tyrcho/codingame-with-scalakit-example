package info.daviot.tictactoe

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.gomoku.GomokuBoard

case class UltimateBoard(smallBoards: Map[Pos, GomokuBoard] = Map.empty, lastMove: Option[Pos] = None) {

  def play(pos: Pos): UltimateBoard = {
    val key = pos / 3
    val smallBoard = boardToPlay(key).play(pos % 3)
    copy(
      lastMove = Some(pos),
      smallBoards = smallBoards.updated(key, smallBoard))
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

}
