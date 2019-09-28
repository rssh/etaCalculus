package termware.etaCalculus

import termware.etaCalculus.matchingNet.MNContradiction
import termware.util.FastRefOption


trait TCArrows[T] extends TCTerm[T]
{

  def iarrows(t:T): IArrows

  def checkPattern(t:T, s: VarSubstitution, arg: ITerm): ArrowPatternCheckResult

  def termApplyChecked(t:T, arg: ITerm, u: ArrowPatternCheckSuccess): ITerm =
    u.selectedRight.substVars(u.substitution,Map.empty)

  def linear(t:T): Seq[(ITerm,ITerm)]

  def addPair(t:T, left: ITerm, right:ITerm): Either[Contradiction,IArrows]

  def isEmpty(t:T): Boolean

  override def leftUnifyInSubst(t: T, s: VarSubstitution, o: ITerm): UnificationResult

  override def tcError(t: T): FastRefOption[TCErrorTerm[T]] = FastRefOption.empty
  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption.empty
  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption.empty
  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption.empty
  override def tcStructured(t: T): FastRefOption[TCStructured[T]] = FastRefOption.empty
  override def tcEta(t: T): FastRefOption[TCEtaTerm[T]] = FastRefOption.empty
  override def tcPatternCondition(t: T): FastRefOption[TCPatternCondition[T]] = FastRefOption.empty
  override def tcArrows(t:T): FastRefOption[TCArrows[T]] = FastRefOption(this)

  override def termEqNoRef(t: T, otherTerm: ITerm): Boolean = t.equals(otherTerm.carrier)

}

trait IArrows extends ITerm
{
  type Carrier

  def tcArrows: TCArrows[Carrier]

  def carrier: Carrier

  def checkPattern(s: VarSubstitution, arg: ITerm): ArrowPatternCheckResult

  def termApplyChecked(arg: ITerm, u: ArrowPatternCheckSuccess): ITerm

  def termApply(s: VarSubstitution, arg: ITerm): FastRefOption[ITerm] =
    checkPattern(s,arg) match {
      case ArrowPatternCheckSuccess(s,r,p) => FastRefOption(r.substVars(s,Map.empty))
      case ArrowPatternCheckFailure(x,msg) => FastRefOption.empty
    }

  def isEmpty(): Boolean

  def linear(): Seq[(ITerm,ITerm)]

  def addPair(left:ITerm, right: ITerm): Either[Contradiction,IArrows]


  override def kindFold[S](s0: S)(folder: TermKindFolder[S]): S = {
    folder.onArrows(this,s0)
  }

  //def orElse(other: IArrows) = OrElseRulest(this,other)

  override def kindTransform[B](matcher: TermKindTransformer[B], vo: Map[IEtaTerm, IEtaTerm]): B = {
    matcher.onArrows(this, vo)
  }

}

object IArrows {

  def unapply(arg: ITerm): FastRefOption[IArrows] = {
     arg.asArrows()
  }

  def fromPairs(pairs:(ITerm,ITerm)*): Either[Contradiction,IArrows] = {
    val s0: Either[Contradiction,IArrows] = Right(EmptyArrows)
    pairs.foldLeft(s0){ (s,e) =>
      s.flatMap(sx => sx.addPair(e._1,e._2))
    }
  }

}

case class CArrows[T](t:T, tc: TCArrows[T]) extends IArrows {

  override type Carrier = T

  override def tcArrows: TCArrows[T] = tc

  override def carrier: T = t

  override def checkPattern(s: VarSubstitution, arg: ITerm): ArrowPatternCheckResult = {
    tc.checkPattern(t,s,arg)
  }

  override def isEmpty(): Boolean = {
    tc.isEmpty(t)
  }

  override def linear(): Seq[(ITerm, ITerm)] = tc.linear(t)

  override def addPair(left: ITerm, right: ITerm): Either[Contradiction, IArrows] =
    tc.addPair(t,left,right)

  override def termApplyChecked(arg: ITerm, u: ArrowPatternCheckSuccess): ITerm = {
    tc.termApplyChecked(t,arg,u)
  }

  override def tcTerm: TCTerm[T] = tc

  override def kindTransform[B](matcher: TermKindTransformer[B], vo: Map[IEtaTerm, IEtaTerm]): B = matcher.onArrows(this,vo)
}

object TCArrow extends TCArrows[Arrow] {

  override def iarrows(t: Arrow): IArrows = t

  override def mapVars(t: Arrow, f: IVarTerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    t.mapVars(f,vo)
  }

  override def checkPattern(t: Arrow, s: VarSubstitution, arg: ITerm): ArrowPatternCheckResult = {
    t.checkPattern(s,arg)
  }

  override def termApplyChecked(t: Arrow, arg: ITerm, s:  ArrowPatternCheckSuccess): ITerm = {
    t.termApplyChecked(arg, s)
  }

  override def linear(t: Arrow): Seq[(ITerm, ITerm)] = ???

  override def addPair(t: Arrow, left: ITerm, right: ITerm): Either[Contradiction, IArrows] = ???

  override def isEmpty(t: Arrow): Boolean = ???

  override def leftUnifyInSubst(t: Arrow, s: VarSubstitution, o: ITerm): UnificationResult = ???

  override def hasPatternsRec(t: Arrow, trace: Map[IVarTerm, Boolean]): Boolean = ???
}


case class Arrow(left:ITerm, right:ITerm, otherwise: IArrows) extends IArrows {

  thisArrow =>

  override type Carrier = Arrow

  override def tcTerm: TCTerm[Arrow] = TCArrow

  type LeftCarrier = left.Carrier

  type RightCarrier = right.Carrier

  override def carrier: Carrier = this

  override def tcArrows: TCArrows[Arrow] = TCArrow

  override def checkPattern(s: VarSubstitution, arg: ITerm) : ArrowPatternCheckResult = {
    left.leftUnifyInSubst(s,arg) match {
      case UnificationSuccess(substitution) =>
             ArrowPatternCheckSuccess(substitution,right,left)
      case UnificationFailure(msg, left, right, prevFailure, substitution) =>
             ArrowPatternCheckFailure(left,msg)
    }
  }

  override def termApplyChecked(arg: ITerm, u: ArrowPatternCheckSuccess): ITerm = {
    right.substVars(u.substitution,Map.empty)
  }

  override def leftUnifyInSubst(s: VarSubstitution, o: ITerm): UnificationResult = {
     o match {
       case IArrows(otherArrows) =>
         otherArrows.checkPattern(s,left) match {
           case sc@ArrowPatternCheckSuccess(sLeft, selectedRight, originPattern) =>
             val r = otherArrows.termApplyChecked(left, sc)
             right.leftUnifyInSubst(sLeft,r)
           case ArrowPatternCheckFailure(originPattern, msg) =>
             otherwise.leftUnifyInSubst(s,o)
         }
     }
  }


  override def mapVars(f: IVarTerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    val l = left.mapVars(f,vo)
    if (l.isError()) {
      l
    } else {
      val r = right.mapVars(f,vo)
      if (r.isError()) r else {
        val o = otherwise.mapVars(f,vo)
        o match {
          case IArrows(oa) => Arrow(l,r,oa)
          case IErrorTerm(oe) => oe
          case _ => IErrorTerm("mapvars (arrows) is not arrow")
        }
      }
    }
  }

  override def isEmpty(): Boolean = false

  override def linear(): Seq[(ITerm, ITerm)] = {
    (left,right) +: otherwise.linear()
  }

  override def addPair(left: ITerm, right: ITerm): Either[Contradiction, IArrows] = ???

  override def hasPatternsRec(trace: Map[IVarTerm, Boolean]): Boolean = {
    (    left.hasPatternsRec(trace)
      || right.hasPatternsRec(trace)
      || otherwise.hasPatternsRec(trace)
    )
  }

}

object TCEmptyArrows extends TCArrows[EmptyArrows.type] {

  override def iarrows(t: EmptyArrows.type): IArrows = t

  override def checkPattern(t: EmptyArrows.type, s: VarSubstitution, arg: ITerm): ArrowPatternCheckResult =
    ArrowPatternCheckFailure(arg,"empty")

  override def isEmpty(t: EmptyArrows.type): Boolean = true

  override def linear(t: EmptyArrows.type): Seq[(ITerm, ITerm)] = Seq()

  override def addPair(t: EmptyArrows.type, left: ITerm, right: ITerm): Either[Contradiction, IArrows] = Right(Arrow(left,right,t))

  override def leftUnifyInSubst(t: EmptyArrows.type, s: VarSubstitution, o: ITerm): UnificationResult = {
    o match {
      case IArrows(oa) => if (oa.isEmpty()) {
                             UnificationSuccess(s)
                          } else {
                             UnificationFailure("!empty",EmptyArrows,o,None,s)
                          }
      case _ => UnificationFailure("!arrow",EmptyArrows,o,None,s)
    }
  }

  override def mapVars(t: EmptyArrows.type, f: IVarTerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = t

  override def hasPatternsRec(t: EmptyArrows.type, trace: Map[IVarTerm, Boolean]): Boolean = false

}

object EmptyArrows extends IArrows {
  override type Carrier = this.type

  override def tcArrows: TCArrows[EmptyArrows.type] = TCEmptyArrows

  override def carrier: EmptyArrows.type = EmptyArrows

  override def checkPattern(s: VarSubstitution, arg: ITerm): ArrowPatternCheckResult =
    ArrowPatternCheckFailure(arg,"emplty left")

  override def termApplyChecked(arg: ITerm, u: ArrowPatternCheckSuccess): ITerm = {
    // will
    this
  }

  override def tcTerm: TCTerm[EmptyArrows.type] = TCEmptyArrows

  override def kindTransform[B](matcher: TermKindTransformer[B], vo: Map[IEtaTerm, IEtaTerm]): B = {
    matcher.onArrows(this,vo)
  }

  override def isEmpty(): Boolean = true

  override def linear(): Seq[(ITerm, ITerm)] = Seq.empty

  override def addPair(left: ITerm, right: ITerm): Either[Contradiction, IArrows] =
    Right(Arrow(left,right,this))

}

