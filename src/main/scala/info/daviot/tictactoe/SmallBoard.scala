package info.daviot.tictactoe

import com.truelaurel.algorithm.game._
import com.truelaurel.math.geometry.Pos
import com.truelaurel.math.geometry.grid.{BitGrid, GridData, Masks}
import info.daviot.tictactoe.SmallBoard._

case class SmallBoard(dataTrue: GridData = GridData(3),
                      dataFalse: GridData = GridData(3),
                      dataFree: GridData = GridData(3, rows = Array.fill(3)((1 << (3 + 1)) - 1)),
                      nextPlayer: Boolean = false)
    extends GameState[Boolean] {

    lazy val playedFalse: Set[Pos] = dataFalse.usedPos
    lazy val playedTrue: Set[Pos] = dataTrue.usedPos
    lazy val free: Set[Pos] = dataFree.usedPos


    def play(x: Int, y: Int): SmallBoard = play(Pos(x, y))

    def play(p: Pos): SmallBoard =
        forcePlay(p, nextPlayer)

    def outcome: Outcome[Boolean] =
        if (hasWon(true)) Wins(true)
        else if (hasWon(false)) Wins(false)
        else if (free.isEmpty) Draw
        else Undecided

    def hasWon(player: Boolean): Boolean = {
        val data = if (player) dataTrue else dataFalse
        BitGrid(data, masks).complete
    }

    def forcePlay(p: Pos, player: Boolean): SmallBoard = {
        if (player)
            copy(dataTrue = dataTrue + (p.x, p.y), dataFree = dataFree - (p.x, p.y), nextPlayer = !player)
        else
            copy(dataFalse = dataFalse + (p.x, p.y), dataFree = dataFree - (p.x, p.y), nextPlayer = !player)
    }

    def isFree(p: Pos): Boolean = dataFree.isUsed(p)

    override def toString: String = toText

    def toText: String = Seq.tabulate(3) { y =>
        Seq.tabulate(3) { x =>
            if (playedFalse(Pos(x, y))) 'F'
            else if (playedTrue(Pos(x, y))) 'T'
            else ' '
        }.mkString
    }.mkString("\n")


}


object SmallBoard {
    val masks = Masks(3, 3)
}
