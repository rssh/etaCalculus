package termware.etaCalculus.matchingNet

import termware.etaCalculus.{ArrowsMergingPolicy, ITerm, VarSubstitution}

case class CheckTermMatchingElement(check: ITerm, onSuccess: MatchingNetElement, onFail: MatchingNetElement) extends MatchingNetElement {
  override def checkPattern(s: VarSubstitution, pattern: ITerm): MatchingNetPatternCheckResult = ???

  override def add(index: ITerm, value: ITerm, mergingPolicy: ArrowsMergingPolicy): Either[MNContradiction, MatchingNetElement] = ???

  override def isFinal(): Boolean = ???
}
