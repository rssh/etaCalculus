package termware.etaCalculus

import termware.util.{FastRefOption, IdentityRef}

trait TCEtaTerm[T] extends TCTerm[T] {

  def ieta(t:T): IEtaTerm

  def context(t:T): Substitution[IName,ITerm]

  def etaModify(t:T, f: Substitution[IName,ITerm] => Substitution[IName,ITerm]): T

  def resolve(t:T, n:IName): Option[ITerm]
    = context(t).get(n)

  def baseTerm(t:T): ITerm

}


trait IEtaTerm extends ITerm
{

  type Carrier

  def tcEtaTerm: TCEtaTerm[Carrier]

  final override def tcTerm: TCTerm[Carrier] = tcEtaTerm

  def carrier: Carrier

  def context(): Substitution[IName,ITerm] = tcEtaTerm.context(carrier)

  def baseTerm(): ITerm = tcEtaTerm.baseTerm(carrier)

  override def transform[B](matcher: TermKindMatcher[B]): B = matcher.onEta(this)

  def etaModify(f: Substitution[IName,ITerm] => Substitution[IName,ITerm]): IEtaTerm =
    tcEtaTerm.ieta(tcEtaTerm.etaModify(carrier,f))

  def resolve(n:IName) = tcEtaTerm.resolve(carrier,n)


  override final def equals(obj: scala.Any): Boolean = {
    if (obj.isInstanceOf[ITerm]) {
      val otherTerm = obj.asInstanceOf[ITerm]
      if (otherTerm.isEta()) {
        // each eta=term unique
        this eq otherTerm
      } else false
    } else false
  }

  override final def hashCode(): Int = {
    System.identityHashCode(this)
  }

}

object IEtaTerm {

  def unapply(arg: ITerm): FastRefOption[IEtaTerm] = arg.asEta()

  def create(prevOwner: IEtaTerm, nextOwner: IEtaTerm, context: Substitution[IName,ITerm], baseTerm: ITerm): IEtaTerm = {
    ???
  }

}


case class CEtaTerm[T](carrier :T, tcEtaTerm:TCEtaTerm[T]) extends IEtaTerm
{

  type Carrier = T

}

sealed trait VarOwner {
  def fromSelf(self:IEtaTerm): IEtaTerm
}

class OwnerRef(iEtaTerm: IEtaTerm) extends VarOwner {
  override def fromSelf(self: IEtaTerm): IEtaTerm = iEtaTerm
}

case object SelfRef extends VarOwner {
  override def fromSelf(self: IEtaTerm): IEtaTerm = self
}

class VarOwnerChangeTransformer(owners: Map[IEtaTerm,IEtaTerm]) extends TermKindMatcher[ITerm] {

  thisVarOwnerChangeTransformer =>

  override def onName(name: IName): ITerm = name

  override def onVar(varTerm: IVarTerm): ITerm =
    owners.get(varTerm.owner) match {
      case Some(changed) => PlainVarTerm(changed,varTerm.name)
      case None => varTerm
    }

  override def onPrimitive(primitive: IPrimitive): ITerm = primitive

  override def onStructured(structured: IStructured): ITerm = {
    structured.mapSubterms(_.transform(this))
  }

  override def onEta(eta: IEtaTerm): ITerm = {
    new PlainEtaTerm(
      new PlainEtaInitTransformers {
        override def contextTransformation(oldContext: Substitution[IName, ITerm]): Substitution[IName, ITerm] =
          oldContext.mapValues(_.transform(thisVarOwnerChangeTransformer))

        override def bodyTransformer(self: PlainEtaTerm, oldBody: ITerm): ITerm = {
          val nOwners = owners.updated(eta, self)
          oldBody.transform(new VarOwnerChangeTransformer(nOwners))
        }

      },
      eta.context(),
      eta.baseTerm()
    )
  }

  override def onError(error: IErrorTerm): ITerm = ???

}

trait PlainEtaInitTransformers {

  def contextTransformation(oldContext: Substitution[IName,ITerm]): Substitution[IName,ITerm]

  def bodyTransformer(self: PlainEtaTerm, oldBody: ITerm): ITerm

}

object TCPlainEtaTerm extends TCEtaTerm[PlainEtaTerm] {

  override def ieta(t: PlainEtaTerm): IEtaTerm = t

  override def context(t: PlainEtaTerm): Substitution[IName, ITerm] =
    t.context

  override def etaModify(t: PlainEtaTerm, f: Substitution[IName, ITerm] => Substitution[IName, ITerm]): PlainEtaTerm
   = t.etaModify(f)

  override def resolve(t: PlainEtaTerm, n: IName): Option[ITerm] =
    context(t).get(n)

  override def baseTerm(t: PlainEtaTerm): ITerm = ???

  override def mapVars(t: PlainEtaTerm, f: IVarTerm => ITerm): ITerm = ???

  //override def subst[N <: ITerm, V <: ITerm](t: PlainEtaTerm, s: ISubstitution[N, V]): ITerm = ???

  override def tcName(t: PlainEtaTerm): FastRefOption[TCName[PlainEtaTerm]] = ???

  override def tcVar(t: PlainEtaTerm): FastRefOption[TCVarTerm[PlainEtaTerm]] = ???

  override def tcPrimitive(t: PlainEtaTerm): FastRefOption[TCPrimitive[PlainEtaTerm]] = ???

  override def tcStructured(t: PlainEtaTerm): FastRefOption[TCStructured[PlainEtaTerm]] = ???

  override def tcEta(t: PlainEtaTerm): FastRefOption[TCEtaTerm[PlainEtaTerm]] = ???

  override def tcError(t: PlainEtaTerm): FastRefOption[TCErrorTerm[PlainEtaTerm]] = ???

  override def leftUnifyInSubst(t: PlainEtaTerm, s: Substitution[IVarTerm, ITerm], o: ITerm): UnificationResult = ???
}


class PlainEtaTerm(
    initTransformers: PlainEtaInitTransformers,
    iniContext: Substitution[IName,ITerm],
    iniBaseTerm: ITerm
   ) extends IEtaTerm {

  thisEtaTerm =>

  override val context = initTransformers.contextTransformation(iniContext)

  // TODO: do reduction of internal eta term
  override val baseTerm = initTransformers.bodyTransformer(this,iniBaseTerm)

  override type Carrier = PlainEtaTerm

  override def carrier: PlainEtaTerm = this

  override def tcEtaTerm: TCEtaTerm[PlainEtaTerm] = TCPlainEtaTerm

  override def etaModify(f: Substitution[IName, ITerm] => Substitution[IName, ITerm]): PlainEtaTerm =
    new PlainEtaTerm(
      new PlainEtaInitTransformers {
        override def contextTransformation(oldContext: Substitution[IName, ITerm]): Substitution[IName, ITerm] =
          f(oldContext)

        override def bodyTransformer(self: PlainEtaTerm, oldBody: ITerm): ITerm = ???
      },
      context,
      baseTerm
    )

}