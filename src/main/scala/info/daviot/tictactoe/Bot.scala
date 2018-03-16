package info.daviot.tictactoe

import com.truelaurel.codingame.challenge.GameBot

object Bot extends GameBot[State, Action] {
  /**
    * Reacts to the given game state by playing one or more actions
    *
    * @param state current state of the game
    * @return one or more actions to play
    */
  override def react(state: State): Action = Action()
}
