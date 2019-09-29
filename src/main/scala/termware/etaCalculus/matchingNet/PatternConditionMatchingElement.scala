package termware.etaCalculus.matchingNet

import termware.etaCalculus.{ArrowsMergingPolicy, IPatternCondition, ITerm, VarSubstitution}

class PatternConditionMatchingElement(values: Seq[(IPatternCondition,MatchingNetElement)], onFail: MatchingNetElement = NotFoundElement) extends MatchingNetElement {
  override def checkPattern(s: VarSubstitution, pattern: ITerm): MatchingNetPatternCheckResult = ???

  override def add(index: ITerm, value: ITerm, mergingPolicy: ArrowsMergingPolicy): Either[MNContradiction, MatchingNetElement] = ???

  override def isFinal(): Boolean = ???

  def addPatternCondition(index:IPatternCondition, value: ITerm, mergingPolicy: ArrowsMergingPolicy): Either[MNContradiction, PatternConditionMatchingElement] = ???


}

