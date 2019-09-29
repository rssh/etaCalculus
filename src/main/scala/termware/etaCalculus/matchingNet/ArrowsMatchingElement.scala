package termware.etaCalculus.matchingNet

import termware.etaCalculus.{ArrowPatternCheckSuccess, ArrowsMergingPolicy, IArrows, ITerm, VarSubstitution}


class ArrowsMatchingElement(arrows: IArrows, otherwise: MatchingNetElement = NotFoundElement ) extends MatchingNetElement {

  override def checkPattern(s: VarSubstitution, pattern: ITerm): MatchingNetPatternCheckResult = {
    arrows.checkPattern(s,pattern) match {
      case ArrowPatternCheckSuccess(s,selectedRight, origin) =>
        MatchingNetPatternCheckResult(s,FoundElement(selectedRight),true)
      case _ =>
        MatchingNetPatternCheckResult(s,otherwise,false)
    }
  }

  override def add(index: ITerm, value: ITerm, mergingPolicy: ArrowsMergingPolicy): Either[MNContradiction, MatchingNetElement]= ???

  override def isFinal(): Boolean = false
}

