package info.daviot.tictactoe

import com.github.writethemfirst.approvals.utils.functions.Function1
import com.github.writethemfirst.approvals.{Approver, CombinationApprover}
import com.truelaurel.math.geometry.Pos

import scala.collection.JavaConverters._

trait Approving {
    def verifyAll[I, O](name: String, inputs: Iterable[I], f: I => O): Unit = {
        val list = asJavaIterable(inputs)
        val fJ: Function1[I, O] = i => f(i)
        new CombinationApprover().writeTo(name).testing(this.getClass)
            .verifyAllCombinations(list, fJ)
    }

    def verifyMoves(moves: Set[Pos], name: String): Unit =
        verify(moves.toList.sortBy(Pos.unapply).mkString("\n"), name)

    def verify(output: Any, name: String): Unit =
        new Approver().writeTo(name).testing(this.getClass)
            .verify(output)
}
