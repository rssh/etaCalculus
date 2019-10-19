package termware.etaCalculus.matchingNet

import termware.etaCalculus.{Arrow, ArrowPatternCheckFailure, ArrowPatternCheckResult, ArrowPatternCheckSuccess, ArrowsMergingPolicy, Contradiction, EmptyArrows, IArrows, IEtaTerm, ITerm, IVarTerm, TCArrows, UnificationFailure, UnificationResult, UnificationSuccess, VarSubstitution}

class MNArrows(element: MatchingNetElement) extends IArrows {

  override type Carrier = MNArrows
  override def tcArrows: TCArrows[Carrier] = TCMNArrows

  override def carrier: MNArrows = this

  override def checkPattern(s: VarSubstitution, arg: ITerm): ArrowPatternCheckResult = {
    var c = element.checkPattern(s,arg)
    while(!c.nextElement.isFinal()) {
      // TODO: c.arg
      c = element.checkPattern(c.substitution,arg)
    }
    c.nextElement match {
      case FoundElement(value) => ArrowPatternCheckSuccess(c.substitution,value,arg)
      case NotFoundElement => ArrowPatternCheckFailure(arg,"not found")
      case ErrorMatchingNetElement(msg,pattern) => ArrowPatternCheckFailure(arg,msg)
    }
  }

  override def termApplyChecked(arg: ITerm, u: ArrowPatternCheckSuccess): ITerm = {
    u.selectedRight.substVars(u.substitution,Map.empty)
  }

  override def leftUnifyInSubst(s: VarSubstitution, o: ITerm): UnificationResult = {
    ???
  }


  override def isEmpty(): Boolean = { element == NotFoundElement }

  override def linear(): Seq[(ITerm, ITerm)] = ???

  override def addPair(left: ITerm, right: ITerm, mergingPolicy: ArrowsMergingPolicy): Either[Contradiction, IArrows] = ???

}



object TCMNArrows extends TCArrows[MNArrows] {

  override def iarrows(t: MNArrows): IArrows = t

  override def checkPattern(t: MNArrows, s: VarSubstitution, arg: ITerm): ArrowPatternCheckResult = t.checkPattern(s,arg)

  override def linear(t: MNArrows): Seq[(ITerm, ITerm)] = t.linear()

  override def addPair(t: MNArrows, left: ITerm, right: ITerm, mergingPolicy: ArrowsMergingPolicy): Either[Contradiction, IArrows] =
    t.addPair(left, right, mergingPolicy)

  override def isEmpty(t: MNArrows): Boolean = t.isEmpty()

  override def leftUnifyInSubst(t: MNArrows, s: VarSubstitution, o: ITerm): UnificationResult = t.leftUnifyInSubst(s,o)

  override def mapVars(t: MNArrows, f: IVarTerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = ???

  /**
    * recursive version of hasPatterns, which track circular dependencies in trace.
    * Public as
    * True, when term have patternCondition inside.
    *
    * @param t
    * @param trace
    * @return
    */
  override def hasPatternsRec(t: MNArrows, trace: Map[IVarTerm, Boolean]): Boolean = ???
}