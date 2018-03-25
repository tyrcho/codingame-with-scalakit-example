package info.daviot.tictactoe

import com.truelaurel.algorithm.game._
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

    object UltimateRules extends RulesFor2p[UltimateBoard, Pos] {
        def initial: UltimateBoard = UltimateBoard()

        def validMoves(state: UltimateBoard): Seq[Pos] = state.validMoves.toSeq

        def applyMove(state: UltimateBoard, move: Pos): UltimateBoard =
            state.play(move)

        def outcome(state: UltimateBoard): Outcome[Boolean] =
            state.gameResult
    }


    def mctsMove(s: UltimateBoard): Pos = {
        val chronometer = new Chronometer(1.seconds)
        chronometer.start()
        MctsAi(UltimateRules)(_ => chronometer.willOutOfTime).chooseMove(s)
    }

}
