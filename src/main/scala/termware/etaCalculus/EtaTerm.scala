package termware.etaCalculus

trait TCEtaTerm[T] extends TCTerm[T] {

  def context(t:T): IVarSubstitution

  def etaModify[T](t:T, f: IVarSubstitution => IVarSubstitution): T

  def resolve(t:T, n:IName): Option[ITerm]

  def baseTerm(t:T): ITerm

}


trait IEtaTerm extends ITerm
{

  type Carrier

  def tcEtaTerm: TCEtaTerm[Carrier]

  final override def tcTerm: TCTerm[Carrier] = tcEtaTerm

  def carrier: Carrier

  def context(): IVarSubstitution = tcEtaTerm.context(carrier)

}


case class CEtaTerm[T](carrier :T, tcEtaTerm:TCEtaTerm[T]) extends IEtaTerm
{

  type Carrier = T

}
