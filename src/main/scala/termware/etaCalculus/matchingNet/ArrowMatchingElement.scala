package termware.etaCalculus.matchingNet

import termware.etaCalculus.{Arrow, ArrowsMergingPolicy, ITerm, UnificationFailure, UnificationSuccess, VarSubstitution}

class ArrowMatchingElement(arrow: Arrow, onFail: MatchingNetElement = NotFoundElement) extends MatchingNetElement {

  override def checkPattern(s: VarSubstitution, pattern: ITerm): MatchingNetPatternCheckResult = {
    arrow.left.leftUnifyInSubst(s,pattern) match {
      case UnificationSuccess(s1) =>
        MatchingNetPatternCheckResult(s1,FoundElement(arrow.right),true)
      case UnificationFailure(msg, left, right, prevFailure, substitution) =>
        val next = if (arrow.otherwise.isEmpty()) {
          onFail
        } else {
          new ArrowsMatchingElement(arrow.otherwise, onFail)
        }
        MatchingNetPatternCheckResult.failure(s,next)
    }
  }

  override def add(index: ITerm, value: ITerm, mergingPolicy: ArrowsMergingPolicy): Either[MNContradiction,MatchingNetElement] = ???

  override def isFinal(): Boolean = ???
}

