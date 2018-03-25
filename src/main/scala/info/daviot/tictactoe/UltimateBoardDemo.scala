package info.daviot.tictactoe

import com.truelaurel.algorithm.mcts.MctsAi
import com.truelaurel.math.geometry.Pos
import com.truelaurel.time.Chronometer

import scala.concurrent.duration.DurationInt

object UltimateBoardDemo {

    def main(args: Array[String]): Unit = {
        val outcome = UltimateRules.judge(
            truePl = mctsMove,
            falsePl = mctsMove,
            s => println(s.debugString))
        println(outcome)
    }


    def mctsMove(s: UltimateBoard): Pos = {
        val chronometer = new Chronometer(100.millis)
        chronometer.start()
        MctsAi(UltimateRules)(_ => chronometer.willOutOfTime).chooseMove(s)
    }

}
