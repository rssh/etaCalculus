package termware.etaCalculus

import termware.etaCalculus.PlainEtaTerm.TraveseTransformers
import termware.util.{FastRefOption, IdentityRef}

import scala.reflect.ClassTag

trait TCEtaTerm[T] extends TCTerm[T] {

  def ieta(t:T): IEtaTerm

  def context(t:T): Substitution[IName,ITerm]

  def etaModify(t:T,
      f: Substitution[IName,ITerm] => Substitution[IName,ITerm],
      vo:Map[IEtaTerm,IEtaTerm]): T

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

  override def transform[B](matcher: TermKindTransformer[B], vo: Map[IEtaTerm,IEtaTerm]): B =
    matcher.onEta(this,vo)

  def etaModify(f: Substitution[IName,ITerm] => Substitution[IName,ITerm], vo: Map[IEtaTerm,IEtaTerm]): IEtaTerm =
    tcEtaTerm.ieta(tcEtaTerm.etaModify(carrier,f,vo))

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

  /**
    * Create terms and change names to variables.
    * @param context
    * @param baseTerm
    * @return
    */
  def create(context: Substitution[IName,ITerm], baseTerm: ITerm): IEtaTerm = {
     val names = context.keys()
     new PlainEtaTerm(
       new PlainEtaInitTransformers {

         override def contextTransformation(self: PlainEtaTerm, oldContext: Substitution[IName, ITerm]): Substitution[IName, ITerm] = {
            oldContext.mapValues(_.map(substName(_,self),Map.empty))
         }

         override def bodyTransformer(self: PlainEtaTerm, oldBody: ITerm): ITerm = {
            oldBody.map(substName(_,self),Map.empty)
         }

         def substName(t:ITerm, self: PlainEtaTerm): ITerm = {
           t match {
             case IName(n) => if (context.containsKey(n)) {
                  PlainVarTerm(self,n)
                } else n
             case other => other
           }
         }

       },
       context,
       baseTerm
     )
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

object VarOwnerChangeTransformer extends TermKindTransformer[ITerm] {

  override def onName(name: IName, vo: Map[IEtaTerm,IEtaTerm]): ITerm = name

  override def onVar(varTerm: IVarTerm, vo:Map[IEtaTerm,IEtaTerm]): ITerm =
    vo.get(varTerm.owner) match {
      case Some(changed) => PlainVarTerm(changed,varTerm.name)
      case None => varTerm
    }

  override def onPrimitive(primitive: IPrimitive, vo:Map[IEtaTerm,IEtaTerm]): ITerm = primitive

  override def onStructured(structured: IStructured, vo: Map[IEtaTerm,IEtaTerm] ): ITerm = {
    structured.mapSubterms(_.transform(this,vo),vo,true)
  }

  override def onEta(eta: IEtaTerm, vo: Map[IEtaTerm,IEtaTerm]): ITerm = {
    new PlainEtaTerm(
      new PlainEtaInitTransformers {
        override def contextTransformation(self: PlainEtaTerm, oldContext: Substitution[IName, ITerm]): Substitution[IName, ITerm] = {
          val nOwners = vo.updated(eta, self)
          oldContext.mapValues(_.transform(VarOwnerChangeTransformer,nOwners))
        }

        override def bodyTransformer(self: PlainEtaTerm, oldBody: ITerm): ITerm = {
          val nOwners = vo.updated(eta, self)
          oldBody.transform(VarOwnerChangeTransformer,nOwners)
        }

      },
      eta.context(),
      eta.baseTerm()
    )
  }

  override def onError(error: IErrorTerm, vo:Map[IEtaTerm,IEtaTerm]): ITerm = error

  override def onPatternCondition(patternCondition: IPatternCondition, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
     val expr = patternCondition.expression
     val nExpr = expr.transform(this,vo)
     if (nExpr eq expr) {
       patternCondition
     } else {
       patternCondition.substExpression(nExpr)
     }
  }

}

trait PlainEtaInitTransformers {

  def contextTransformation(self: PlainEtaTerm, oldContext: Substitution[IName,ITerm]): Substitution[IName,ITerm]

  def bodyTransformer(self: PlainEtaTerm, oldBody: ITerm): ITerm

}


object TCPlainEtaTerm extends TCEtaTerm[PlainEtaTerm] {

  override def ieta(t: PlainEtaTerm): IEtaTerm = t

  override def context(t: PlainEtaTerm): Substitution[IName, ITerm] =
    t.context

  override def etaModify(t: PlainEtaTerm,
      f: Substitution[IName, ITerm] => Substitution[IName, ITerm],
      vo: Map[IEtaTerm,IEtaTerm]): PlainEtaTerm
   = t.etaModify(f,vo)

  override def resolve(t: PlainEtaTerm, n: IName): Option[ITerm] =
    context(t).get(n)

  override def baseTerm(t: PlainEtaTerm): ITerm = t.baseTerm

  override def mapVars(t: PlainEtaTerm, f: IVarTerm => ITerm, vo:Map[IEtaTerm,IEtaTerm]): ITerm = {
    t.mapVars(f,vo)
  }

  override def subst[N <: ITerm, V <: ITerm](t: PlainEtaTerm, s: Substitution[N, V], vo: Map[IEtaTerm, IEtaTerm])(implicit nTag:ClassTag[N]): ITerm = {
    t.subst(s,vo)
  }

  override def map(t: PlainEtaTerm, f: ITerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    t.map(f,vo)
  }

  override def hasPatternsRec(t: PlainEtaTerm, trace: Map[IVarTerm,Boolean]): Boolean = t.hasPatternsRec(trace)

  override def tcName(t: PlainEtaTerm): FastRefOption[TCName[PlainEtaTerm]] = FastRefOption.empty
  override def tcVar(t: PlainEtaTerm): FastRefOption[TCVarTerm[PlainEtaTerm]] = FastRefOption.empty
  override def tcPrimitive(t: PlainEtaTerm): FastRefOption[TCPrimitive[PlainEtaTerm]] = FastRefOption.empty
  override def tcStructured(t: PlainEtaTerm): FastRefOption[TCStructured[PlainEtaTerm]] = FastRefOption.empty
  override def tcEta(t: PlainEtaTerm): FastRefOption[TCEtaTerm[PlainEtaTerm]] = FastRefOption(this)
  override def tcError(t: PlainEtaTerm): FastRefOption[TCErrorTerm[PlainEtaTerm]] = FastRefOption.empty
  override def tcPatternCondition(t: PlainEtaTerm): FastRefOption[TCPatternCondition[PlainEtaTerm]] = FastRefOption.empty

  override def termEqNoRef(t: PlainEtaTerm, otherTerm: ITerm): Boolean = t.termEqNoRef(otherTerm)

  override def leftUnifyInSubst(t: PlainEtaTerm, s: Substitution[IVarTerm, ITerm], o: ITerm): UnificationResult = {
    // TODO:  recheck and add tests
    t.baseTerm.leftUnifyInSubst(s,o)
  }


}


class PlainEtaTerm(
    initTransformers: PlainEtaInitTransformers,
    iniContext: Substitution[IName,ITerm],
    iniBaseTerm: ITerm
   ) extends IEtaTerm {

  thisEtaTerm =>

  override val context = initTransformers.contextTransformation(thisEtaTerm,iniContext)

  // TODO: do reduction of internal eta term
  override val baseTerm = initTransformers.bodyTransformer(thisEtaTerm,iniBaseTerm)

  override type Carrier = PlainEtaTerm

  override def carrier: PlainEtaTerm = this

  override def tcEtaTerm: TCEtaTerm[PlainEtaTerm] = TCPlainEtaTerm

  override def etaModify(f: Substitution[IName, ITerm] => Substitution[IName, ITerm],
      vo: Map[IEtaTerm,IEtaTerm]): PlainEtaTerm =
    new PlainEtaTerm(
      new PlainEtaInitTransformers {

        override def contextTransformation(self: PlainEtaTerm,
            oldContext: Substitution[IName, ITerm]): Substitution[IName, ITerm] =
          f(oldContext).mapValues(
            _.transform(VarOwnerChangeTransformer, vo.updated(thisEtaTerm,self))
          )

        override def bodyTransformer(self: PlainEtaTerm, oldBody: ITerm): ITerm =
          oldBody.transform(VarOwnerChangeTransformer,vo.updated(thisEtaTerm,self))

      },
      context,
      baseTerm
    )

  override def mapVars(f: IVarTerm => ITerm, vo:Map[IEtaTerm,IEtaTerm]): ITerm = {
    new PlainEtaTerm(
      TraveseTransformers(thisEtaTerm,(t,vo)=>t.mapVars(f,vo),vo),
      context,
      baseTerm
    )
  }

  override def subst[N <: ITerm, V <: ITerm](s: Substitution[N, V], vo: Map[IEtaTerm, IEtaTerm])(implicit nTag: ClassTag[N]): ITerm = {
    new PlainEtaTerm(
      TraveseTransformers(thisEtaTerm,(t,vo)=>t.subst(s,vo),vo),
      context,
      baseTerm
    )
  }

  override def map(f: ITerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    new PlainEtaTerm(
      TraveseTransformers(thisEtaTerm,(t,vo)=>t.map(f,vo),vo),
      context,
      baseTerm
    )
  }

  override def hasPatternsRec(trace: Map[IVarTerm,Boolean]): Boolean = baseTerm.hasPatternsRec(trace)

  override def termEqNoRef(other: ITerm): Boolean = baseTerm.termEqNoRef(other)


}

object PlainEtaTerm {

  case class TraveseTransformers(
      thisEtaTerm: IEtaTerm,
      f: (ITerm,Map[IEtaTerm,IEtaTerm]) => ITerm,
      vo:Map[IEtaTerm,IEtaTerm]) extends PlainEtaInitTransformers {
    override def contextTransformation(self: PlainEtaTerm, oldContext: Substitution[IName, ITerm]): Substitution[IName, ITerm] = {
      oldContext.mapValues(f(_,vo.updated(thisEtaTerm,self)))
    }

    override def bodyTransformer(self: PlainEtaTerm, oldBody: ITerm): ITerm = {
      f(oldBody,vo.updated(thisEtaTerm,self))
    }
  }

}