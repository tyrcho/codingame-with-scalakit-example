package info.daviot.tictactoe

import com.truelaurel.algorithm.game.{Outcome, RulesFor2p}
import com.truelaurel.math.geometry.Pos

object UltimateRules extends RulesFor2p[UltimateBoard, Pos] {
    def initial: UltimateBoard = UltimateBoard()

    def validMoves(state: UltimateBoard): Seq[Pos] = state.validMoves.toSeq

    def applyMove(state: UltimateBoard, move: Pos): UltimateBoard =
        state.play(move)

    def outcome(state: UltimateBoard): Outcome[Boolean] =
        state.gameResult
}
