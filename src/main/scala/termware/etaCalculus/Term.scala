package termware.etaCalculus

import termware.util.FastRefOption


trait TCTerm[T]  {

   def iterm(t:T):ITerm = CTerm(t,this)

   def leftUnifyInSubst(t: T, s:IVarSubstitution, o: ITerm): IVarSubstitution

   def leftUnifyInSubst[S,O](t:T, s:S, o:O)(implicit stc: TCVarSubstitution[S], otc: TCTerm[O]): IVarSubstitution =
     leftUnifyInSubst(t,stc.isubstitution(s),otc.iterm(o))

   def substVars(t:T, s: IVarSubstitution): ITerm

   def tcName(t: T): FastRefOption[TCName[T]]

   def isName(t: T): Boolean = tcName(t).isDefined

   def tcVar(t:T): FastRefOption[TCVarTerm[T]]

   def isVar(t:T): Boolean = tcVar(t).isDefined

   def tcPrimitive(t:T): FastRefOption[TCPrimitive[T]]

   def isPrimitive(t:T): Boolean = tcPrimitive(t).isDefined

}

trait ITerm
{

  type Carrier

  def tcTerm: TCTerm[Carrier]

  def carrier: Carrier

  def isName(): Boolean = tcTerm.isName(carrier)

  def asName(): FastRefOption[IName] = tcTerm.tcName(carrier).map(_.iname(carrier))

  def isPrimitive(): Boolean = tcTerm.isPrimitive(carrier)

  def asPrimitive(): FastRefOption[IPrimitive] = tcTerm.tcPrimitive(carrier).map(_.iprimitive(carrier))

  def leftUnifyInSubst(s: IVarSubstitution, o: ITerm): IVarSubstitution = {
    tcTerm.leftUnifyInSubst(carrier,s,o)
  }

  def substVars(s: IVarSubstitution): ITerm = {
    tcTerm.substVars(carrier,s)
  }

}

object ITerm {

  type Aux[T] = ITerm{ type Carrier = T }

}

case class CTerm[T](t:T,tc:TCTerm[T]) extends ITerm
{
   type Carrier = T

   def tcTerm: TCTerm[Carrier] = tc

   def carrier: Carrier = t

}

trait TCTermLowPriorityImplicits {



}
