package info.daviot.tictactoe

import com.truelaurel.codingame.challenge.{GameAccumulator, GameIO}
import com.truelaurel.math.geometry.Pos

object TicIO extends GameIO[TicContext, State, Action] {
  /**
    * Reads game context from the referee system. A context stores game's global information
    */
  override def readContext: TicContext = TicContext()

  /**
    * Reads current state from the referee system. A state provides information for the current turn
    */
  override def readState(turn: Int, context: TicContext): State = {
    val Array(opponentrow, opponentcol) = readLine.split(" ").map(_.toInt)
    val validPos = Seq.fill(readInt) {
      val Array(row, col) = readLine.split(" ").map(_.toInt)
      Pos(row, col)
    }

    State(validPos)
  }

  /**
    * Writes action to the referee system
    */
  override def writeAction(state: State, action: Action): Unit = {
    println(s"${action.pos.x} ${action.pos.y}")
  }
}

object Accumulator extends GameAccumulator[TicContext, State, Action] {
  /**
    * Accumulates information derived from the current state and selected actions into a new game context that will be
    * used in the next round.
    *
    * In certain cases, the input state doesn't include all known information. These information must be calculated from
    * historical actions and states. For example, it could action cool down, previously observed positions in fog of war.
    *
    * @param context the current context which may contain historical events.
    * @param state   the current state
    * @param action  actions performed for the current round
    * @return a new context accumulated with historical events including those generated from the current round
    */
  override def accumulate(context: TicContext, state: State, action: Action): TicContext = context
}

case class TicContext()

case class State(validActions: Seq[Pos])

case class Action(pos:Pos)