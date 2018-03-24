package info.daviot.tictactoe

import com.truelaurel.algorithm.alphabeta.AlphaBetaAi
import com.truelaurel.codingame.challenge.GameBot
import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.gomoku.{GomokuBoard, GomokuRules}

object Bot extends GameBot[State, Action] {
  val rules = GomokuRules(3, 3)

  /**
    * Reacts to the given game state by playing one or more actions
    *
    * @param state current state of the game
    * @return one or more actions to play
    */
  override def react(state: State): Action =
    Action(state.validActions.head)


  def alphaBetaMove(s: GomokuBoard): Pos =
    AlphaBetaAi(rules, rules.centerHeuristic).chooseMove(s, 3)
}
