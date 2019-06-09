package termware.etaCalculus

import java.beans.Expression

import termware.util.FastRefOption

import scala.reflect.ClassTag

trait TCPatternCondition[T] extends TCTerm[T] {

  def iPatternCondition(t:T): IPatternCondition = CPatternCondition(t,this)

  def expression(t:T): ITerm

  def substExpression(t:T, nExpr: ITerm): IPatternCondition

  def interpret(t:T, s:Substitution[IVarTerm,ITerm], arg: ITerm): Boolean

  override def hasPatternsRec(t: T,trace: Map[IVarTerm, Boolean]): Boolean = true

  override def tcPatternCondition(t: T): FastRefOption[TCPatternCondition[T]] = FastRefOption(this)

  override def tcEta(t: T): FastRefOption[TCEtaTerm[T]] = FastRefOption.empty
  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption.empty
  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption.empty
  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption.empty
  override def tcError(t: T): FastRefOption[TCErrorTerm[T]] = FastRefOption.empty


  override def termEqNoRef(t: T, otherTerm: ITerm): Boolean = {
    otherTerm match {
      case IPatternCondition(otherExpression) => expression(t).termEqNoRef(otherExpression)
      case _ => false
    }
  }

}


trait IPatternCondition extends ITerm {

  override final def tcTerm = tcGuarded

  def tcGuarded: TCPatternCondition[Carrier]

  def expression: ITerm =
    tcGuarded.expression(carrier)

  def substExpression(nExpr: ITerm): IPatternCondition =
    tcGuarded.substExpression(carrier, nExpr)

  def interpret(s:Substitution[IVarTerm,ITerm], arg: ITerm): Boolean =
    tcGuarded.interpret(carrier,s,arg)

  override def transform[B](matcher: TermKindTransformer[B], vo: Map[IEtaTerm, IEtaTerm]): B = {
     matcher.onPatternCondition(this,vo)
  }

}

object IPatternCondition
{

  def unapply(arg: ITerm): FastRefOption[IPatternCondition] = {
    if (arg.isInstanceOf[IPatternCondition]) {
      FastRefOption(arg.asInstanceOf[IPatternCondition])
    } else {
      FastRefOption.empty
    }
  }


}

case class CPatternCondition[T](override val carrier:T, override val tcGuarded:TCPatternCondition[T]) extends IPatternCondition {

  type Carrier = T

}


case class PlainPatternCondition(override val expression: ITerm) extends IPatternCondition {

  override type Carrier = PlainPatternCondition

  override def carrier: PlainPatternCondition = this

  override def tcGuarded: TCPatternCondition[Carrier] = TCPlainPatternCondition
}

object TCPlainPatternCondition extends TCPatternCondition[PlainPatternCondition] {

  override def expression(t: PlainPatternCondition): ITerm = t.expression

  override def substExpression(t: PlainPatternCondition, nExpr: ITerm): IPatternCondition =
    t.copy(expression = nExpr)

  override def interpret(t: PlainPatternCondition, s: Substitution[IVarTerm, ITerm], arg: ITerm): Boolean = {
    t.expression match {
      case IEtaTerm(eta) => eta.context().get(PredefinedNames.THIS) match {
          case Some(v) => val s1 = s.update(IVarTerm(eta,PredefinedNames.THIS),arg)
                    v.leftUnifyInSubst(s1,arg) match {
                      case UnificationSuccess(s2) =>
                        PredefLogicInterpretations.instance.check(t.expression,s2).isSuccess()
                      case failure: UnificationFailure => false
                    }
          case None => PredefLogicInterpretations.instance.check(t.expression,s).isSuccess()
        }
      case _ => PredefLogicInterpretations.instance.check(t.expression,s).isSuccess()
    }
  }

  override def mapVars(t: PlainPatternCondition, f: IVarTerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = ???

  override def subst[N <: ITerm, V <: ITerm](t: PlainPatternCondition, s: Substitution[N, V], vo: Map[IEtaTerm, IEtaTerm])(implicit nTag: ClassTag[N]): ITerm = ???

  override def map(t: PlainPatternCondition, f: ITerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = ???

  override def tcName(t: PlainPatternCondition): FastRefOption[TCName[PlainPatternCondition]] = ???

  override def tcVar(t: PlainPatternCondition): FastRefOption[TCVarTerm[PlainPatternCondition]] = ???

  override def tcPrimitive(t: PlainPatternCondition): FastRefOption[TCPrimitive[PlainPatternCondition]] = ???

  override def tcStructured(t: PlainPatternCondition): FastRefOption[TCStructured[PlainPatternCondition]] = ???

  override def tcEta(t: PlainPatternCondition): FastRefOption[TCEtaTerm[PlainPatternCondition]] = ???

  override def tcError(t: PlainPatternCondition): FastRefOption[TCErrorTerm[PlainPatternCondition]] = ???

  override def leftUnifyInSubst(t: PlainPatternCondition, s: Substitution[IVarTerm, ITerm], o: ITerm): UnificationResult = ???
}