import com.truelaurel.codingame.challenge.GameLoop
import info.daviot.tictactoe.{Accumulator, TicIO, Bot}

// https://www.codingame.com/ide/puzzle/tic-tac-toe

// ~runMain com.truelaurel.codingame.tool.bundle.BundlerMain TicTacToe.scala
object Player {
  def main(args: Array[String]): Unit = {
    new GameLoop(TicIO, Bot, Accumulator).run()
  }
}
