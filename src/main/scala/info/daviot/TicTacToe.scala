import com.truelaurel.codingame.challenge.GameLoop
import info.daviot.tictactoe.{Accumulator, TicIO, Bot}

object Player {
  def main(args: Array[String]): Unit = {
    new GameLoop(TicIO, Bot, Accumulator).run()
  }
}
