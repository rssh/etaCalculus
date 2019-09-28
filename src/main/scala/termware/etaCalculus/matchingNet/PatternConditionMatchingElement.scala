package termware.etaCalculus.matchingNet

import termware.etaCalculus.{IPatternCondition, ITerm, VarSubstitution}

class PatternConditionMatchingElement(values: Seq[(IPatternCondition,MatchingNetElement)], onFail: MatchingNetElement = NotFoundElement) extends MatchingNetElement {
  override def checkPattern(s: VarSubstitution, pattern: ITerm): MatchingNetPatternCheckResult = ???

  override def add(index: ITerm, value: ITerm): Either[MNContradiction, MatchingNetElement] = ???

  override def isFinal(): Boolean = ???

  def addPatternCondition(index:IPatternCondition, value: ITerm): Either[MNContradiction, PatternConditionMatchingElement] = ???


}

