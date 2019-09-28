package termware.etaCalculus.matchingNet

import termware.etaCalculus.{IName, ITerm, VarSubstitution}

class NameSameArityElement(
    byName: Map[IName,MatchingNetElement],
    onFail: MatchingNetElement = NotFoundElement) extends MatchingNetElement {

  override def checkPattern(s: VarSubstitution, pattern: ITerm): MatchingNetPatternCheckResult =
    pattern match {
      case IName(n) => byName.get(n) match {
        case None => MatchingNetPatternCheckResult.failure(s,onFail)
        case Some(givenName) => givenName.checkPattern(s,pattern)
      }
    }

  override def add(index: ITerm, value: ITerm): Either[MNContradiction,MatchingNetElement] = ???

  def addSameArity(index: ITerm, value: ITerm): Either[MNContradiction,NameSameArityElement] = ???

  override def isFinal(): Boolean = ???

}

object NameSameArityElement {
  def apply(
      byName: Map[IName, MatchingNetElement],
      onFail: MatchingNetElement = NotFoundElement): NameSameArityElement =
    new NameSameArityElement(byName, onFail)
}
