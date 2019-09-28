package termware.etaCalculus.matchingNet

import termware.etaCalculus.{ITerm, VarSubstitution}

case class CheckTermMatchingElement(check: ITerm, onSuccess: MatchingNetElement, onFail: MatchingNetElement) extends MatchingNetElement {
  override def checkPattern(s: VarSubstitution, pattern: ITerm): MatchingNetPatternCheckResult = ???

  override def add(index: ITerm, value: ITerm): Either[MNContradiction, MatchingNetElement] = ???

  override def isFinal(): Boolean = ???
}
