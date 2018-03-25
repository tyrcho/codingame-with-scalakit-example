package info.daviot.tictactoe

import com.truelaurel.algorithm.game._
import com.truelaurel.math.geometry.Pos
import com.truelaurel.math.geometry.grid.{GridData, Masks}

case class SmallBoard(gridTrue: IntGrid = IntGrid(),
                      gridFalse: IntGrid = IntGrid(),
                      gridFree: IntGrid = IntGrid.full,
                      dataTrue: GridData = GridData(3),
                      dataFalse: GridData = GridData(3),
                      dataFree: GridData = GridData(3, rows = Array.fill(3)((1 << (3 + 1)) - 1)),
                      nextPlayer: Boolean = false)
    extends GameState[Boolean] {

    lazy val playedFalse: Seq[Pos] = gridFalse.usedPositions
    lazy val playedTrue: Seq[Pos] = gridTrue.usedPositions
    lazy val free: Seq[Pos] = gridFree.usedPositions


    def play(x: Int, y: Int): SmallBoard = play(Pos(x, y))

    def play(p: Pos): SmallBoard =
        forcePlay(p, nextPlayer)

    def outcome: Outcome[Boolean] =
        if (hasWon(true)) Wins(true)
        else if (hasWon(false)) Wins(false)
        else if (free.isEmpty) Draw
        else Undecided

    def hasWon(player: Boolean): Boolean = {
        val data = if (player) gridTrue else gridFalse
        data.isWon
    }

    def forcePlay(p: Pos, player: Boolean): SmallBoard = {
        if (player)
            copy(dataTrue = dataTrue + (p.x, p.y),
                gridTrue = gridTrue + p,
                dataFree = dataFree - (p.x, p.y),
                gridFree = gridFree - p,
                nextPlayer = !player)
        else
            copy(dataFalse = dataFalse + (p.x, p.y),
                gridFalse = gridFalse + p,
                dataFree = dataFree - (p.x, p.y),
                gridFree = gridFree - p,
                nextPlayer = !player)
    }

    def isFree(p: Pos): Boolean = dataFree.isUsed(p)

    override def toString: String = toText

    def toText: String = Seq.tabulate(3) { y =>
        Seq.tabulate(3) { x =>
            if (playedFalse.contains(Pos(x, y))) 'F'
            else if (playedTrue.contains(Pos(x, y))) 'T'
            else ' '
        }.mkString
    }.mkString("\n")


}


object SmallBoard {
    val allPos: Iterable[Pos] = for {
        row <- 0 to 2
        col <- 0 to 2
    } yield Pos(row, col)

    val masks = Masks(3, 3)
}

import info.daviot.tictactoe.IntGrid._

// 3x3 only
case class IntGrid(data: Int = 0) extends AnyVal {
    def freePositions: Seq[Pos] = allPositions.filter(v => (1 << v & data) == 0).map(posAtValue)

    def usedPositions: Seq[Pos] = allPositions.filter(v => (1 << v & data) != 0).map(posAtValue)


    def isWon: Boolean = winningMasks.exists(m =>
        (m.data & data) == m.data)

    def +(pos: Pos) =
        IntGrid(data | 1 << posValue(pos))

    def -(pos: Pos) =
        IntGrid(data & ~(1 << posValue(pos)))


}

object IntGrid {
    val full = IntGrid((1 << 10) - 1)
    val allPositions: Seq[Int] = 0 to 8

    val winningMasks: Seq[IntGrid] = horizMasks ++ verticalMasks ++ diagonalMasks

    def horizMasks: Seq[IntGrid] = Seq.tabulate(3)(row => IntGrid() + Pos(row, 0) + Pos(row, 1) + Pos(row, 2))

    def verticalMasks: Seq[IntGrid] = Seq.tabulate(3)(col => IntGrid() + Pos(0, col) + Pos(1, col) + Pos(2, col))

    def diagonalMasks: Seq[IntGrid] = Seq(
        IntGrid() + Pos(0, 0) + Pos(1, 1) + Pos(2, 2),
        IntGrid() + Pos(0, 2) + Pos(1, 1) + Pos(2, 0))

    private def posValue(pos: Pos): Int = pos.x + 3 * pos.y

    private def posAtValue(value: Int): Pos = Pos(value % 3, value / 3)
}
