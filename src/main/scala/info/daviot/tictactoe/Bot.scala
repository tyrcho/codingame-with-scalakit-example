package info.daviot.tictactoe

import com.truelaurel.algorithm.mcts.MctsAi
import com.truelaurel.codingame.challenge.GameBot
import com.truelaurel.math.geometry.Pos
import com.truelaurel.time.Chronometer

import scala.concurrent.duration.DurationInt

object Bot extends GameBot[State, Action] {
    /**
      * Reacts to the given game state by playing one or more actions
      *
      * @param state current state of the game
      * @return one or more actions to play
      */
    override def react(state: State): Action =
        Action(mctsMove(state.board))

    def mctsMove(s: UltimateBoard): Pos = {
        val time = if (s.lastMove.isDefined) 90.millis else 900.millis
        val chronometer = new Chronometer(time)
        chronometer.start()
        MctsAi(UltimateRules)(_ => chronometer.willOutOfTime).chooseMove(s)
    }
}
