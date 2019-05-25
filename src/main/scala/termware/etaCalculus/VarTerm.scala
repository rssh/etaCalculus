package termware.etaCalculus

import termware.util.{FastRefOption, IdentityRef}


trait TCVarTerm[T] extends TCTerm[T]
{

  def owner(t:T): IEtaTerm

  def name(t:T): IName

  def ivar(t:T): IVarTerm = CVarTerm(t,this)

  override def leftUnifyInSubst(t: T, s: Substitution[IVarTerm, ITerm], o: ITerm): UnificationResult = {
     o.asVar() match {
       case FastRefOption.Some(otherVar) =>
         if (otherVar.ownerRef == ownerRef(t)
             &&
             otherVar.name == name(t)
            )
              UnificationSuccess(s)
         else
           ILeftUnificable.putOrMerge(s,ivar(t),o)
       case _ =>
         ILeftUnificable.putOrMerge(s,ivar(t),o)
     }
  }


  override def substVars(t: T, s: Substitution[IVarTerm,ITerm]): ITerm = {
    val v = ivar(t)
    s.getOrElse(v,v)
  }

  override def mapVars(t: T, f: IVarTerm => ITerm): ITerm = {
    f(ivar(t))
  }

  def ownerRef(t:T): IdentityRef[IEtaTerm] =
    new IdentityRef[IEtaTerm](owner(t))


  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption.empty
  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption(this)
  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption.empty
  override def tcStructured(t: T): FastRefOption[TCStructured[T]] = FastRefOption.empty
  override def tcEta(t: T): FastRefOption[TCEtaTerm[T]] = FastRefOption.empty
  override def tcError(t: T): FastRefOption[TCErrorTerm[T]] = FastRefOption.empty


}



trait IVarTerm extends ITerm {

  type Carrier

  def carrier: Carrier

  def tcVarTerm: TCVarTerm[Carrier]

  override def tcTerm: TCTerm[Carrier] = tcVarTerm

  def name: IName = tcVarTerm.name(carrier)

  def owner: IEtaTerm = tcVarTerm.owner(carrier)

  def ownerRef: IdentityRef[IEtaTerm] = tcVarTerm.ownerRef(carrier)

  override def transform[B](matcher: TermKindMatcher[B]): B = matcher.onVar(this)



  override def equals(obj: scala.Any): Boolean = {
      if (obj.isInstanceOf[IVarTerm]) {
        val other = obj.asInstanceOf[IVarTerm]
        if (owner eq other) {
           name == other.name
        } else false
      } else false
  }

  override def hashCode(): Int = {
    name.hashCode() ^ System.identityHashCode(owner)
  }

}

object IVarTerm
{

  def unapply(arg: ITerm): FastRefOption[IVarTerm] = arg.asVar()

}

case class CVarTerm[T](carrier: T, tcVarTerm: TCVarTerm[T]) extends IVarTerm
{
  override type Carrier = T


}

class PlainVarTerm(override val owner: IEtaTerm, override val name: IName) extends IVarTerm {

  override type Carrier = PlainVarTerm

  override def carrier: PlainVarTerm = this

  override def tcVarTerm: TCVarTerm[PlainVarTerm] = TCPlainVarTerm

  override def substVars(s: Substitution[IVarTerm,ITerm]): ITerm = {
    s.getOrElse(this,this)
  }


}

object PlainVarTerm {

  def apply(owner: IEtaTerm, name:IName): PlainVarTerm =
    new PlainVarTerm(owner, name)

}

object TCPlainVarTerm extends TCVarTerm[PlainVarTerm] {

  override def owner(t: PlainVarTerm): IEtaTerm = t.owner

  override def name(t: PlainVarTerm): IName = t.name


}
