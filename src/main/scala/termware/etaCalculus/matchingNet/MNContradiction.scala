package termware.etaCalculus.matchingNet

import termware.etaCalculus.{Contradiction, ITerm}
import termware.util.SimplePrint


case class MNContradiction(
    index:ITerm,
    target: MatchingNetElement,
    msg:String) extends Contradiction {

  override def message: String = msg

  override def longMessage: String =
    s"""
       |contradictions in MNNEN: ${msg}
       |index = ${SimplePrint(index)}
       |target = ${target}
     """.stripMargin

  override def operation: String = "addPair"

  override def args: Seq[ITerm] = Seq(index,new MNArrows(target))
}

