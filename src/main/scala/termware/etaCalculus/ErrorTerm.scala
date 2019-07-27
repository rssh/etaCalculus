package termware.etaCalculus
import termware.util.FastRefOption

import scala.reflect.ClassTag

trait TCErrorTerm[T] extends TCTerm[T] {

  def ierror(t:T): IErrorTerm = CErrorTerm(t,this)

  def shortMessage(t: T): String

  def detailedMessage(t: T): String

  def traceData(t:T): Any

  override def hasPatternsRec(t: T, trace: Map[IVarTerm,Boolean]): Boolean = false

  override def leftUnifyInSubst(t: T, s: VarSubstitution, o: ITerm): UnificationResult = {
    UnificationFailure("attempt to unify error",iterm(t),o,None,s)
  }


  override def substVars(t: T, s: VarSubstitution, vo:Map[IEtaTerm,IEtaTerm]): ITerm = ierror(t)
  override def mapVars(t:T, f: IVarTerm => ITerm, vo:Map[IEtaTerm,IEtaTerm]): ITerm = ierror(t)

  override def subst[N <: ITerm, V <: ITerm](t: T, s: Substitution[N, V], vo: Map[IEtaTerm, IEtaTerm])(implicit nTag: ClassTag[N]): ITerm = ierror(t)

  //override def map(t: T, f: ITerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = ierror(t)

  override def tcError(t: T): FastRefOption[TCErrorTerm[T]] = FastRefOption(this)
  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption.empty
  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption.empty
  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption.empty
  override def tcStructured(t: T): FastRefOption[TCStructured[T]] = FastRefOption.empty
  override def tcEta(t: T): FastRefOption[TCEtaTerm[T]] = FastRefOption.empty
  override def tcPatternCondition(t: T): FastRefOption[TCPatternCondition[T]] = FastRefOption.empty

  override def termEqNoRef(t: T, otherTerm: ITerm): Boolean = t.equals(otherTerm.carrier)

}


trait IErrorTerm extends ITerm {

  override def tcTerm: TCTerm[Carrier] = tcError

  def tcError: TCErrorTerm[Carrier]

  override def kindTransform[B](matcher: TermKindTransformer[B], vo:Map[IEtaTerm,IEtaTerm]): B = {
    matcher.onError(this,vo)
  }

  override def kindFold[S](s0: S)(folder: TermKindFolder[S]): S = folder.onError(this,s0)

  def shortMessage(): String = {
    tcError.shortMessage(carrier)
  }

  def detailedMessage(): String = {
    tcError.detailedMessage(carrier)
  }

}

object IErrorTerm {

  def unapply(arg: ITerm): FastRefOption[IErrorTerm] = arg.asError()

  def apply(msg: String): IErrorTerm = new CErrorTerm[String](msg, TCStringErrorTerm)
}

case class CErrorTerm[T](carrier:T, tcError: TCErrorTerm[T]) extends IErrorTerm {
  type Carrier = T
}


object TCStringErrorTerm extends TCErrorTerm[String] {
  override def shortMessage(t: String): String = t

  override def detailedMessage(t: String): String = t

  override def traceData(t: String): Any = t
}