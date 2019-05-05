package termware.etaCalculus

import termware.util.{FastRefOption, IdentityRef}


trait TCVarTerm[T] extends TCTerm[T]
{

  def owner(t:T): IEtaTerm

  def name(t:T): IName

  def ivar(t:T): IVarTerm = CVarTerm(t,this)

  def leftUnifyInSubst[S](s:IVarSubstitution, t:T, o:S)(implicit stc: TCEtaTerm[S]): IVarSubstitution = {
      implicit val tc:TCVarTerm[T] = this
      s.put(t,o)
  }

  override def substVars(t: T, s: IVarSubstitution): ITerm = {
    s.get(t)(this) match {
      case Some(x) => x
      case None => ivar(t)
    }
  }

  def ownerRef(t:T): IdentityRef[IEtaTerm] =
    new IdentityRef[IEtaTerm](owner(t))


  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption.empty

  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption(this)

  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption.empty


}



trait IVarTerm extends ITerm
{

  type Carrier

  def carrier: Carrier

  def tcVarTerm: TCVarTerm[Carrier]

  override def tcTerm: TCTerm[Carrier] = tcVarTerm

  def name: IName = tcVarTerm.name(carrier)

  def owner: IEtaTerm = tcVarTerm.owner(carrier)

  def onwerRef: IdentityRef[IEtaTerm] = tcVarTerm.ownerRef(carrier)



}

case class CVarTerm[T](carrier: T, tcVarTerm: TCVarTerm[T]) extends IVarTerm
{
  override type Carrier = T

}

case class PlainVarTerm(override val owner: IEtaTerm, override val name: IName) extends IVarTerm {

  override type Carrier = PlainVarTerm

  override def carrier: PlainVarTerm = this

  override def tcVarTerm: TCVarTerm[PlainVarTerm] = TCPlainVarTerm



  override def substVars(s: IVarSubstitution): ITerm = {
    s.getOrElse(this,this)
  }



}

object TCPlainVarTerm extends TCVarTerm[PlainVarTerm] {

  override def owner(t: PlainVarTerm): IEtaTerm = t.owner

  override def name(t: PlainVarTerm): IName = t.name

  override def leftUnifyInSubst(t: PlainVarTerm, s: IVarSubstitution, o: ITerm): IVarSubstitution = {
    s.put(t,o)
  }

}
