package info.daviot.tictactoe

import com.truelaurel.algorithm.alphabeta.AlphaBetaAi
import com.truelaurel.algorithm.mcts.MctsAi
import com.truelaurel.codingame.challenge.GameBot
import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.gomoku.{GomokuBoard, GomokuRules}
import com.truelaurel.time.Chronometer

import scala.concurrent.duration.DurationInt

object Bot extends GameBot[State, Action] {
    val rules = GomokuRules(3, 3)

    /**
      * Reacts to the given game state by playing one or more actions
      *
      * @param state current state of the game
      * @return one or more actions to play
      */
    override def react(state: State): Action =
        Action(mctsMove(state.board))

    def mctsMove(s: UltimateBoard): Pos = {
        val chronometer = new Chronometer(90.millis)
        chronometer.start()
        MctsAi(UltimateRules)(_ => chronometer.willOutOfTime).chooseMove(s)
    }

    def alphaBetaMove(s: GomokuBoard): Pos =
        AlphaBetaAi(rules, rules.centerHeuristic).chooseMove(s, 3)
}
