package termware.etaCalculus

import termware.util.FastRefOption


trait TCTerm[T] extends TCLeftUnificable[T] {

   def iterm(t:T):ITerm = CTerm(t,this)

   def substVars(t:T, s: Substitution[IVarTerm,ITerm]): ITerm = {
     mapVars(t, v => s.getOrElse(v,v))
   }

   def mapVars(t:T, f: IVarTerm => ITerm): ITerm

   //def subst[N<:ITerm, V<:ITerm](t:T, s: ISubstitution[N,V]): ITerm

   //def mapTerms(t:T, f: ITerm => ITerm): ITerm

   def tcName(t: T): FastRefOption[TCName[T]]

   def isName(t: T): Boolean = tcName(t).isDefined

   def tcVar(t:T): FastRefOption[TCVarTerm[T]]

   def isVar(t:T): Boolean = tcVar(t).isDefined

   def tcPrimitive(t:T): FastRefOption[TCPrimitive[T]]

   def isPrimitive(t:T): Boolean = tcPrimitive(t).isDefined

   def tcStructured(t:T): FastRefOption[TCStructured[T]]

   def isStructured(t:T): Boolean = tcStructured(t).isDefined

   def tcEta(t:T): FastRefOption[TCEtaTerm[T]]

   def isEta(t:T): Boolean = tcEta(t).isDefined

   def tcError(t:T): FastRefOption[TCErrorTerm[T]]

   def isError(t:T): Boolean = tcError(t).isDefined

}

trait ITerm extends ILeftUnificable
{

  type Carrier

  def tcTerm: TCTerm[Carrier]

  def carrier: Carrier

  def isName(): Boolean = tcTerm.isName(carrier)

  def asName(): FastRefOption[IName] = tcTerm.tcName(carrier).map(_.iname(carrier))

  def isPrimitive(): Boolean = tcTerm.isPrimitive(carrier)

  def asPrimitive(): FastRefOption[IPrimitive] = tcTerm.tcPrimitive(carrier).map(_.iprimitive(carrier))

  def isVar(): Boolean = tcTerm.isVar(carrier)

  def asVar(): FastRefOption[IVarTerm] = tcTerm.tcVar(carrier).map(_.ivar(carrier))

  def isStructured(): Boolean = tcTerm.isStructured(carrier)

  def asStructured(): FastRefOption[IStructured] = tcTerm.tcStructured(carrier).map(_.istructured(carrier))

  def isEta(): Boolean = tcTerm.isEta(carrier)

  def asEta(): FastRefOption[IEtaTerm] = tcTerm.tcEta(carrier).map(_.ieta(carrier))

  def isError(): Boolean = tcTerm.isError(carrier)

  def asError(): FastRefOption[IErrorTerm] = tcTerm.tcError(carrier).map(_.ierror(carrier))

  def transform[B](matcher: TermKindMatcher[B]):B

  def leftUnifyInSubst(s: Substitution[IVarTerm,ITerm], o: ITerm): UnificationResult = {
    tcTerm.leftUnifyInSubst(carrier,s,o)
  }

  def substVars(s: Substitution[IVarTerm,ITerm]): ITerm = {
    tcTerm.substVars(carrier,s)
  }

  def mapVars(f: IVarTerm => ITerm): ITerm = {
    tcTerm.mapVars(carrier,f)
  }

  //def subst[N<:ITerm,V<:ITerm](s:ISubstitution[N,V]): ITerm = {
  //  tcTerm.subst(carrier,s)
  //}

}

object ITerm extends ITermConversions {

  type Aux[T] = ITerm{ type Carrier = T }

}

case class CTerm[T](t:T,tc:TCTerm[T]) extends ITerm
{
   type Carrier = T

   def tcTerm: TCTerm[Carrier] = tc

   def carrier: Carrier = t

  override def transform[B](matcher: TermKindMatcher[B]): B = {
    // need to be reviewed after adding of each term type
    this match {
      case IName(x) => matcher.onName(x)
      case IPrimitive(x) => matcher.onPrimitive(x)
      case IVarTerm(x) => matcher.onVar(x)
      case IEtaTerm(x) => matcher.onEta(x)
      case IStructured(x) => matcher.onStructured(x)
      case IErrorTerm(x) => matcher.onError(x)
    }

  }

}

trait TCTermLowPriorityImplicits {



}
