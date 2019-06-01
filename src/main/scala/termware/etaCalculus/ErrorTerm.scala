package termware.etaCalculus
import termware.util.FastRefOption

trait TCErrorTerm[T] extends TCTerm[T] {

  def ierror(t:T): IErrorTerm = CErrorTerm(t,this)

  def shortMessage(t: T): String

  def detailedMessage(t: T): String

  def traceData(t:T): Any

  override def leftUnifyInSubst(t: T, s: Substitution[IVarTerm,ITerm], o: ITerm): UnificationResult = {
    UnificationFailure("attempt to unify error",iterm(t),o,None,s)
  }

  override def substVars(t: T, s: Substitution[IVarTerm,ITerm], vo:Map[IEtaTerm,IEtaTerm]): ITerm = ierror(t)
  override def mapVars(t:T, f: IVarTerm => ITerm, vo:Map[IEtaTerm,IEtaTerm]): ITerm = ierror(t)

  override def tcError(t: T): FastRefOption[TCErrorTerm[T]] = FastRefOption(this)
  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption.empty
  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption.empty
  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption.empty
  override def tcStructured(t: T): FastRefOption[TCStructured[T]] = FastRefOption.empty
  override def tcEta(t: T): FastRefOption[TCEtaTerm[T]] = FastRefOption.empty

}


trait IErrorTerm extends ITerm {

  override def tcTerm: TCTerm[Carrier] = tcError

  def tcError: TCErrorTerm[Carrier]

  override def transform[B](matcher: TermKindTransformer[B], vo:Map[IEtaTerm,IEtaTerm]): B = {
    matcher.onError(this,vo)
  }

}

object IErrorTerm {

  def unapply(arg: ITerm): FastRefOption[IErrorTerm] = arg.asError()

}

case class CErrorTerm[T](carrier:T, tcError: TCErrorTerm[T]) extends IErrorTerm {
  type Carrier = T
}

