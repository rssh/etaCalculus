package termware.etaCalculus

import java.beans.Expression

import termware.util.FastRefOption

import scala.reflect.{ClassManifestFactory, ClassTag}

trait TCPatternCondition[T] extends TCTerm[T] {

  def iPatternCondition(t:T): IPatternCondition = CPatternCondition(t,this)

  def expression(t:T): ITerm

  def substExpression(t:T, nExpr: ITerm): IPatternCondition


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

  private final def change(modify: ITerm => ITerm): IPatternCondition = {
    val ne = modify(expression)
    if (ne eq expression) {
      this
    } else {
      PlainPatternCondition(ne)
    }
  }

  override def mapVars(f: IVarTerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    change(_.mapVars(f,vo))
  }

  override def substVars(s: Substitution[IVarTerm, ITerm], vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    change(_.substVars(s,vo))
  }

  override def subst[N <: ITerm, V <: ITerm](s: Substitution[N, V], vo: Map[IEtaTerm, IEtaTerm])(implicit nTag: ClassTag[N]): ITerm = {
     if (nTag <:< (ClassManifestFactory.classType(classOf[IPatternCondition]))) {
       val sn = s.asInstanceOf[Substitution[IPatternCondition,V]]
       sn.get(this) match {
         case Some(x) => x
         case None => change(_.subst(s,vo))
       }
     } else {
       change(_.subst(s,vo))
     }
  }

  override def map(f: ITerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm =
    change(_.map(f,vo))

  override def leftUnifyInSubst(s: Substitution[IVarTerm, ITerm], o: ITerm): UnificationResult = {
    expression match {
      case IEtaTerm(etaExpression) =>
        etaExpression.context().get(PredefinedNames.THIS) match {
          case Some(thisValue) => thisValue.leftUnifyInSubst(s,o) match {
              case UnificationSuccess(s1) =>
                PredefLogicInterpretations.instance.check(etaExpression.baseTerm(),s1)
              case failure => failure
            }
          case None => PlainPatternCondition(etaExpression.baseTerm()).leftUnifyInSubst(s,o)
        }
      case other =>
        PredefLogicInterpretations.instance.check(other,s)
    }
  }


}

object TCPlainPatternCondition extends TCPatternCondition[PlainPatternCondition] {

  type Carrier = PlainPatternCondition

  override def expression(t: PlainPatternCondition): ITerm = t.expression

  override def substExpression(t: PlainPatternCondition, nExpr: ITerm): IPatternCondition =
    t.copy(expression = nExpr)

  override def mapVars(t: PlainPatternCondition, f: IVarTerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = t.mapVars(f,vo)

  override def subst[N <: ITerm, V <: ITerm](t: PlainPatternCondition, s: Substitution[N, V], vo: Map[IEtaTerm, IEtaTerm])(implicit nTag: ClassTag[N]): ITerm = {
    t.subst(s, vo)
  }

  override def map(t: PlainPatternCondition, f: ITerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm =
    t.map(f,vo)

  override def tcName(t: Carrier): FastRefOption[TCName[Carrier]] = FastRefOption.empty
  override def tcVar(t: Carrier): FastRefOption[TCVarTerm[Carrier]] = FastRefOption.empty
  override def tcPrimitive(t: Carrier): FastRefOption[TCPrimitive[Carrier]] = FastRefOption.empty
  override def tcStructured(t: Carrier): FastRefOption[TCStructured[Carrier]] = FastRefOption.empty
  override def tcEta(t: Carrier): FastRefOption[TCEtaTerm[PlainPatternCondition]] = FastRefOption.empty
  override def tcError(t: PlainPatternCondition): FastRefOption[TCErrorTerm[PlainPatternCondition]] = ???

  override def leftUnifyInSubst(t: PlainPatternCondition, s: Substitution[IVarTerm, ITerm], o: ITerm): UnificationResult = t.leftUnifyInSubst(s,o)

}