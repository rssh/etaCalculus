package termware.etaCalculus.matchingNet

import termware.etaCalculus._

class MNArrows(element: MatchingNetElement,
                indetermincy: IVarTerm = MNArrows.indeterminacy) extends IArrows {

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

  override def leftUnifyInSubst(s0: VarSubstitution, o: ITerm): UnificationResult = {
    o match {
      case IEtaTerm(eta) => eta.baseTerm().leftUnifyInSubst(s0,o)
      case IArrows(arrows) =>
        var s = s0
        var indeterminacy: IArrows = EmptyArrows
        var result: UnificationResult = UnificationSuccess(s)
        var pairs = arrows.linear()
        var wasFailure = false
        while(!wasFailure && pairs.isEmpty) {
           val (l,r) = pairs.head
           pairs = pairs.tail
           val check = this.checkPattern(s,l)
           check match {
             case ArrowPatternCheckFailure(_,_) =>
               indeterminacy.addPair(l,r,ArrowsMergingPolicy.Contradiction) match {
                 case Left(contradiction) =>
                   result = UnificationFailure("contradiction when merging uncerainty",this,arrows,s)
                   wasFailure = true
                 case Right(newIndetermenism) =>
                   indeterminacy = newIndetermenism
               }
             case ArrowPatternCheckSuccess(s1,sr,_) =>
               s = s1
               sr.leftUnifyInSubst(s,r) match {
                 case UnificationSuccess(s2) => s = s2
                 case f@UnificationFailure(msg,_,_,_,_) =>
                   result = UnificationFailure(msg,this,o,s,Some(f))
                   wasFailure = true
               }
           }
        }
        if (!wasFailure) {
          result = UnificationSuccess(s.updated(indeterminacyVar(),indeterminacy))
        }
        result
      case IPatternCondition(pc) =>
        PredefLogicInterpretations.instance.check(pc.expression, s0)
    }
  }


  override def isEmpty(): Boolean = { element == NotFoundElement }

  override def linear(): Seq[(ITerm, ITerm)] = ???

  override def addPair(left: ITerm, right: ITerm, mergingPolicy: ArrowsMergingPolicy): Either[Contradiction, IArrows] = ???

  override def indeterminacyVar(): IVarTerm = indetermincy

}

object MNArrows {

  val indeterminacyContext: IEtaTerm = IEtaTerm(PredefinedNames.indterminacyName -> IPatternCondition.all)(PredefinedNames.indterminacyName)

  val indeterminacy = indeterminacyContext.baseTerm().asVar().!()

}

object TCMNArrows extends TCArrows[MNArrows] {

  override def iarrows(t: MNArrows): IArrows = t

  override def checkPattern(t: MNArrows, s: VarSubstitution, arg: ITerm): ArrowPatternCheckResult = t.checkPattern(s,arg)

  override def linear(t: MNArrows): Seq[(ITerm, ITerm)] = t.linear()

  override def indeterminacyVar(t: MNArrows): IVarTerm = t.indeterminacyVar()

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