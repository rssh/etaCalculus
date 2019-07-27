package termware.etaCalculus

import termware.util.{FastRefOption, IdentityRef}

import scala.reflect.ClassTag


trait TCVarTerm[T] extends TCTerm[T]
{

  def owner(t:T): IEtaTerm

  def name(t:T): IName

  def ivar(t:T): IVarTerm = CVarTerm(t,this)

  override def leftUnifyInSubst(t: T, s: VarSubstitution, o: ITerm): UnificationResult = {
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

  def fixOwner(v:IVarTerm,vo:Map[IEtaTerm,IEtaTerm]):IVarTerm = {
    vo.get(v.owner) match {
      case Some(x) => v.changeOwner(x)
      case None => v
    }
  }

  override def substVars(t: T, s: VarSubstitution, vo: Map[IEtaTerm,IEtaTerm]): ITerm = {
    val v = ivar(t)
    s.get(v) match {
      case FastRefOption.Some(t) => t.kindTransform(VarOwnerChangeTransformer,vo)
      case FastRefOption.Empty() => fixOwner(v,vo)
    }
  }

  override def mapVars(t: T, f: IVarTerm => ITerm, vo:Map[IEtaTerm,IEtaTerm]): ITerm = {
    f(ivar(t)).kindTransform(VarOwnerChangeTransformer,vo)
  }

  override def subst[N <: ITerm, V <: ITerm](t: T, s: Substitution[N, V], vo: Map[IEtaTerm, IEtaTerm])(implicit nTag: ClassTag[N]): ITerm = {
    val v = ivar(t)
    v match {
      case nTag(vn) => s.get(vn) match {
          case FastRefOption.Some(t) => t.kindTransform(VarOwnerChangeTransformer, vo)
          case FastRefOption.Empty() => fixOwner(v,vo)
        }
      case other => fixOwner(v,vo)
    }
  }

  //override def map(t: T, f: ITerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
  //  f(ivar(t)).kindTransform(VarOwnerChangeTransformer,vo)
  //}

  override def termEqNoRef(t: T, otherTerm: ITerm): Boolean = {
    otherTerm match {
      case IVarTerm(otherVar) =>
        (owner(t) eq otherVar.owner) && name(t).termEqNoRef(otherVar.name)
      case _ => false
    }
  }

  def ownerRef(t:T): IdentityRef[IEtaTerm] =
    new IdentityRef[IEtaTerm](owner(t))

  def changeOwner(t:T, newOwner: IEtaTerm): IVarTerm

  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption.empty
  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption(this)
  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption.empty
  override def tcStructured(t: T): FastRefOption[TCStructured[T]] = FastRefOption.empty
  override def tcEta(t: T): FastRefOption[TCEtaTerm[T]] = FastRefOption.empty
  override def tcError(t: T): FastRefOption[TCErrorTerm[T]] = FastRefOption.empty
  override def tcPatternCondition(t: T): FastRefOption[TCPatternCondition[T]] = FastRefOption.empty

}



trait IVarTerm extends ITerm {

  type Carrier

  def carrier: Carrier

  def tcVarTerm: TCVarTerm[Carrier]

  override def tcTerm: TCTerm[Carrier] = tcVarTerm

  def name: IName = tcVarTerm.name(carrier)

  def owner: IEtaTerm = tcVarTerm.owner(carrier)

  def ownerRef: IdentityRef[IEtaTerm] = tcVarTerm.ownerRef(carrier)

  def changeOwner(newOwner: IEtaTerm): IVarTerm = tcVarTerm.changeOwner(carrier,newOwner)

  override def hasPatternsRec(trace: Map[IVarTerm, Boolean]): Boolean = {
    trace.get(this) match {
      case Some(v) => v
      case None => value() match {
        case FastRefOption.Some(v) => v.hasPatternsRec(trace.updated(this,false))
        case _ => false
      }
    }
  }

  override def kindTransform[B](matcher: TermKindTransformer[B], vo:Map[IEtaTerm,IEtaTerm]): B =
      matcher.onVar(this,vo)

  override def kindFold[S](s0: S)(folder: TermKindFolder[S]): S = folder.onVar(this,s0)

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

  def hasValue(): Boolean = value().isDefined

  def value(): FastRefOption[ITerm] =
    owner.context().get(name)

  def value_(): ITerm =
    owner.context().get(name).get


}

object IVarTerm
{

  def apply(owner: IEtaTerm, name: IName): IVarTerm =
    PlainVarTerm(owner,name)

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

  override def substVars(s: VarSubstitution, vo: Map[IEtaTerm,IEtaTerm]): ITerm = {
    s.get(this) match {
      case FastRefOption.Some(t) => if (vo.isEmpty) t else t.kindTransform(VarOwnerChangeTransformer,vo)
      case FastRefOption.Empty() => vo.get(this.owner).map(this.changeOwner).getOrElse(this)
    }
  }

  override def changeOwner(newOwner: IEtaTerm): IVarTerm = {
    if (newOwner eq owner) {
      this
    } else
      new PlainVarTerm(newOwner, name)
  }

  override def toString = s"PlainVarTerm(${owner.refString},name)"

}

object PlainVarTerm {

  def apply(owner: IEtaTerm, name:IName): PlainVarTerm =
    new PlainVarTerm(owner, name)

}

object TCPlainVarTerm extends TCVarTerm[PlainVarTerm] {

  override def owner(t: PlainVarTerm): IEtaTerm = t.owner

  override def name(t: PlainVarTerm): IName = t.name

  override def changeOwner(t: PlainVarTerm, newOwner: IEtaTerm): IVarTerm = t.changeOwner(newOwner)

  override def hasPatternsRec(t: PlainVarTerm, trace: Map[IVarTerm, Boolean]): Boolean = {
    t.hasPatternsRec(trace)
  }


}
