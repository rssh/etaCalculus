package termware.etaCalculus

import termware.NameSubstitution
import termware.etaCalculus.PlainEtaTerm.TraveseTransformers
import termware.util.{FastRefOption, IdentityRef, SimplePrint}

import scala.reflect.ClassTag

trait TCEtaTerm[T] extends TCTerm[T] {

  def ieta(t:T): IEtaTerm

  def context(t:T): NameSubstitution

  def etaModify(t:T,
      f: NameSubstitution => NameSubstitution,
      vo:Map[IEtaTerm,IEtaTerm]): T

  def resolve(t:T, n:IName): FastRefOption[ITerm]
    = context(t).get(n)

  def baseTerm(t:T): ITerm


  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption.empty
  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption.empty
  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption.empty
  override def tcStructured(t: T): FastRefOption[TCStructured[T]] = FastRefOption.empty
  override def tcEta(t: T): FastRefOption[TCEtaTerm[T]] = FastRefOption(this)
  override def tcError(t: T): FastRefOption[TCErrorTerm[T]] = FastRefOption.empty
  override def tcPatternCondition(t: T): FastRefOption[TCPatternCondition[T]] = FastRefOption.empty
  override def tcArrows(t: T): FastRefOption[TCArrows[T]] = FastRefOption.empty

}


trait IEtaTerm extends ITerm
{

  type Carrier

  def tcEtaTerm: TCEtaTerm[Carrier]

  final override def tcTerm: TCTerm[Carrier] = tcEtaTerm

  def carrier: Carrier

  def context(): NameSubstitution = tcEtaTerm.context(carrier)

  def baseTerm(): ITerm = tcEtaTerm.baseTerm(carrier)

  override def kindTransform[B](matcher: TermKindTransformer[B], vo: Map[IEtaTerm,IEtaTerm]): B =
    matcher.onEta(this,vo)

  override def kindFold[S](s0: S)(folder: TermKindFolder[S]): S = {
    folder.onEta(this,s0)
  }

  def etaModify(f: NameSubstitution => NameSubstitution, vo: Map[IEtaTerm,IEtaTerm]): IEtaTerm =
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

  def refString(): String = {
    super.toString()
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
  def create(context: NameSubstitution, baseTerm: ITerm): IEtaTerm = {

     new PlainEtaTerm(
       new PlainEtaInitTransformers {

         override def contextTransformation(self: PlainEtaTerm, oldContext: NameSubstitution): NameSubstitution = {
            val n2v = new NameToVarTransformer(self,context)
            oldContext.mapValues(_.kindTransform(n2v,Map.empty))
         }

         override def bodyTransformer(self: PlainEtaTerm, oldBody: ITerm): ITerm = {
            val n2v = new NameToVarTransformer(self,context)
            oldBody.kindTransform(n2v,Map.empty)
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


class NameToVarTransformer(owner: IEtaTerm, context: NameSubstitution)
                                  extends VarOwnerChangeTransformer {

  override def onName(name: IName, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    if (context.contains(name)) {
      PlainVarTerm(owner,name)
    } else {
       name
    }
  }

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

trait VarOwnerChangeTransformer extends TermKindTransformer[ITerm] {

  thisVarOwnerChangeTransformer =>

  override def onName(name: IName, vo: Map[IEtaTerm,IEtaTerm]): ITerm = name

  override def onVar(varTerm: IVarTerm, vo:Map[IEtaTerm,IEtaTerm]): ITerm =
    vo.get(varTerm.owner) match {
      case Some(changed) => PlainVarTerm(changed,varTerm.name)
      case None => varTerm
    }

  override def onPrimitive(primitive: IPrimitive, vo:Map[IEtaTerm,IEtaTerm]): ITerm = primitive

  override def onStructured(structured: IStructured, vo: Map[IEtaTerm,IEtaTerm] ): ITerm = {
    structured.mapSubterms(_.kindTransform(this,vo),vo,true)
  }

  override def onEta(eta: IEtaTerm, vo: Map[IEtaTerm,IEtaTerm]): ITerm = {
    new PlainEtaTerm(
      new PlainEtaInitTransformers {
        override def contextTransformation(self: PlainEtaTerm, oldContext: NameSubstitution): NameSubstitution = {
          val nOwners = vo.updated(eta, self)
          oldContext.mapValues(_.kindTransform[ITerm](thisVarOwnerChangeTransformer,nOwners))
        }

        override def bodyTransformer(self: PlainEtaTerm, oldBody: ITerm): ITerm = {
          val nOwners = vo.updated(eta, self)
          oldBody.kindTransform(thisVarOwnerChangeTransformer,nOwners)
        }

      },
      eta.context(),
      eta.baseTerm()
    )
  }

  override def onError(error: IErrorTerm, vo:Map[IEtaTerm,IEtaTerm]): ITerm = error

  override def onPatternCondition(patternCondition: IPatternCondition, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
     val expr = patternCondition.expression
     val nExpr = expr.kindTransform(this,vo)
     if (nExpr eq expr) {
       patternCondition
     } else {
       patternCondition.substExpression(nExpr)
     }
  }

  override def onArrows(arrow: IArrows, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    val pairs = arrow.linear()
    val s0: IArrows = EmptyArrows
    pairs.foldLeft(s0){ (s, e) =>
      val l = e._1.kindTransform(this,vo)
      val r = e._2.kindTransform(this, vo)
      s.addPair(l,r,ArrowsMergingPolicy.OldFirst) match {
        case Right(v) => v
        case Left(msg) =>
          // TODO: flag to add ?
          throw new IllegalStateException("Can't transform arrow:"+msg)
      }
    }
  }


}

object VarOwnerChangeTransformer extends VarOwnerChangeTransformer


trait PlainEtaInitTransformers {

  def contextTransformation(self: PlainEtaTerm, oldContext: NameSubstitution): NameSubstitution

  def bodyTransformer(self: PlainEtaTerm, oldBody: ITerm): ITerm

}


object TCPlainEtaTerm extends TCEtaTerm[PlainEtaTerm] {

  override def ieta(t: PlainEtaTerm): IEtaTerm = t

  override def context(t: PlainEtaTerm): NameSubstitution =
    t.context

  override def etaModify(t: PlainEtaTerm,
      f: NameSubstitution => NameSubstitution,
      vo: Map[IEtaTerm,IEtaTerm]): PlainEtaTerm
   = t.etaModify(f,vo)

  override def resolve(t: PlainEtaTerm, n: IName): FastRefOption[ITerm] =
    context(t).get(n)

  override def baseTerm(t: PlainEtaTerm): ITerm = t.baseTerm

  override def mapVars(t: PlainEtaTerm, f: IVarTerm => ITerm, vo:Map[IEtaTerm,IEtaTerm]): ITerm = {
    t.mapVars(f,vo)
  }

 // override def subst[N <: ITerm, V <: ITerm](t: PlainEtaTerm, s: Substitution[N, V], vo: Map[IEtaTerm, IEtaTerm])(implicit nTag:ClassTag[N]): ITerm = {
  //   t.subst(s,vo)
 // }

  override def hasPatternsRec(t: PlainEtaTerm, trace: Map[IVarTerm,Boolean]): Boolean = t.hasPatternsRec(trace)

  override def termEqNoRef(t: PlainEtaTerm, otherTerm: ITerm): Boolean = t.termEqNoRef(otherTerm)

  override def leftUnifyInSubst(t: PlainEtaTerm, s: VarSubstitution, o: ITerm): UnificationResult = {
    // TODO:  recheck and add tests
    t.baseTerm.leftUnifyInSubst(s,o)
  }


}


class PlainEtaTerm(
    initTransformers: PlainEtaInitTransformers,
    iniContext: NameSubstitution,
    iniBaseTerm: ITerm
   ) extends IEtaTerm {

  thisEtaTerm =>

  override val context = initTransformers.contextTransformation(thisEtaTerm,iniContext)

  // TODO: do reduction of internal eta term
  override val baseTerm = initTransformers.bodyTransformer(thisEtaTerm,iniBaseTerm)

  override type Carrier = PlainEtaTerm

  override def carrier: PlainEtaTerm = this

  override def tcEtaTerm: TCEtaTerm[PlainEtaTerm] = TCPlainEtaTerm

  override def etaModify(f: NameSubstitution => NameSubstitution,
      vo: Map[IEtaTerm,IEtaTerm]): PlainEtaTerm = {
    maybeSame( new PlainEtaTerm(
      new PlainEtaInitTransformers {

        override def contextTransformation(self: PlainEtaTerm,
            oldContext: NameSubstitution): NameSubstitution =
          f(oldContext).mapValues(
            _.kindTransform(VarOwnerChangeTransformer, vo.updated(thisEtaTerm, self))
          )

        override def bodyTransformer(self: PlainEtaTerm, oldBody: ITerm): ITerm =
          oldBody.kindTransform(VarOwnerChangeTransformer, vo.updated(thisEtaTerm, self))

      },
      context,
      baseTerm
    ))
  }

  override def mapVars(f: IVarTerm => ITerm, vo:Map[IEtaTerm,IEtaTerm]): ITerm = {
    maybeSame(new PlainEtaTerm(
      TraveseTransformers(thisEtaTerm,(t,vo)=>t.mapVars(f,vo),vo),
      context,
      baseTerm
    ))
  }

 // override def subst[N <: ITerm, V <: ITerm](s: Substitution[N, V], vo: Map[IEtaTerm, IEtaTerm])(implicit nTag: ClassTag[N]): ITerm = {
 //   maybeSame(new PlainEtaTerm(
 //     TraveseTransformers(thisEtaTerm,(t,vo)=>t.subst(s,vo),vo),
 //     context,
 //     baseTerm
 //   ))
 // }

  override def hasPatternsRec(trace: Map[IVarTerm,Boolean]): Boolean = baseTerm.hasPatternsRec(trace)

  override def termEqNoRef(other: ITerm): Boolean = baseTerm.termEqNoRef(other)

  private def maybeSame(other: PlainEtaTerm): PlainEtaTerm = {
    if ((context eq other.context) && (baseTerm eq other.baseTerm)) {
      this
    } else other
  }

  override def leftUnifyInSubst(s: VarSubstitution, o: ITerm): UnificationResult = {
    Console.print("!!!leftUnifyInSubst, left="+SimplePrint(s))
    Console.print("!!!right="+SimplePrint(o))
    val r = baseTerm.leftUnifyInSubst(s,o)
    Console.print("!!!result="+r)
    r
  }


}

object PlainEtaTerm {

  case class TraveseTransformers(
      thisEtaTerm: IEtaTerm,
      f: (ITerm,Map[IEtaTerm,IEtaTerm]) => ITerm,
      vo:Map[IEtaTerm,IEtaTerm]) extends PlainEtaInitTransformers {

    override def contextTransformation(self: PlainEtaTerm, oldContext: NameSubstitution): NameSubstitution = {
      oldContext.mapValues(f(_,vo.updated(thisEtaTerm,self)))
    }

    override def bodyTransformer(self: PlainEtaTerm, oldBody: ITerm): ITerm = {
      f(oldBody,vo.updated(thisEtaTerm,self))
    }

  }

}