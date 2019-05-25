package termware.etaCalculus

import termware.util.FastRefOption

import scala.annotation.tailrec

case class StructuredComponent(
    name:IName,
    index:Int = -1,
    defValue: FastRefOption[ITerm] = FastRefOption.empty,
    constraint: ILeftUnificable = ILeftUnificable.STAR)



trait TCStructured[T] extends TCTerm[T] {

  def istructured(t:T): IStructured

  def name(t:T): IName

  def arity(t:T):Int

  def subterm(t:T, i:Int): FastRefOption[ITerm]

  def subterm(t:T, n:IName): FastRefOption[ITerm]

  def subtermMeta(t:T, i:Int): FastRefOption[StructuredComponent]

  def subtermMeta(t:T, n:IName): FastRefOption[StructuredComponent]

  def foldSubtermsWhile[S](t:T,s0:S)(f: (S,ITerm) => S)(p: S => Boolean): S

  def mapSubterms(t:T, f: ITerm => ITerm): ITerm

  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption.empty
  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption.empty
  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption.empty
  override def tcStructured(t: T): FastRefOption[TCStructured[T]] = FastRefOption(this)
  override def tcEta(t: T): FastRefOption[TCEtaTerm[T]] = FastRefOption.empty
  override def tcError(t: T): FastRefOption[TCErrorTerm[T]] = FastRefOption.empty

}


trait IStructured extends ITerm
{

  def tcStructured: TCStructured[Carrier]

  override final def tcTerm: TCTerm[Carrier] = tcStructured

  def name(): IName = {
    tcStructured.name(carrier)
  }

  def arity():Int = {
    tcStructured.arity(carrier)
  }

  def subterm(i:Int): FastRefOption[ITerm] = {
    tcStructured.subterm(carrier,i)
  }

  def subterm(n:IName): FastRefOption[ITerm] = {
    tcStructured.subterm(carrier,n)
  }

  def mapSubterms(f:ITerm => ITerm): ITerm = {
    tcStructured.mapSubterms(carrier,f)
  }

  def foldSubtermsWhile[S](s0:S)(f: (S,ITerm) => S)(p: S => Boolean): S = {
    tcStructured.foldSubtermsWhile(carrier,s0)(f)(p)
  }

  override def transform[B](matcher: TermKindMatcher[B]): B = {
    matcher.onStructured(this)
  }

}

object IStructured {

  def unapply(arg: ITerm): FastRefOption[IStructured] = arg.asStructured()



}


object TCPlainStructured extends TCStructured[PlainStructured]
{
  type Carrier = PlainStructured

  override def istructured(t: Carrier): IStructured = t

  override def name(t: Carrier): IName = {
    t.metainfo.name
  }

  override def arity(t: Carrier): Int = {
    t.metainfo.components.size
  }

  override def subterm(t: Carrier, i: Int): FastRefOption[ITerm] = {
    FastRefOption(t.subterms.applyOrElse(i,null))
  }

  override def subterm(t: Carrier, n: IName): FastRefOption[ITerm] = {
    t.metainfo.nameIndexes.get(n) match {
      case Some(i) => subterm(t,i)
      case None => FastRefOption.empty
    }
  }

  override def subtermMeta(t:Carrier, i:Int): FastRefOption[StructuredComponent] = {
    FastRefOption(t.metainfo.components.applyOrElse(i,null))
  }

  override def subtermMeta(t: Carrier, n: IName): FastRefOption[StructuredComponent] = {
    t.metainfo.nameIndexes.get(n) match {
      case Some(i) => subtermMeta(t,i)
      case None => FastRefOption.empty
    }
  }

  override def foldSubtermsWhile[S](t: Carrier, s0: S)(f: (S, ITerm) => S)(p: S => Boolean): S = {
    var s = s0
    var i = 0
    val n = arity(t)
    while(p(s) && i<n) {
      s = f(s,t.subterms(i))
      i += 1
    }
    s
  }


  override def mapVars(t: Carrier, f: IVarTerm => ITerm): ITerm = {
    var i=0
    var wasError = false
    var wasChanged = false
    var r: ITerm = t
    var newSubterms: Array[ITerm] = null
    var cs: Substitution[IVarTerm,ITerm] = MapBasedVarSubstitution.empty
    while(i < arity(t) && !wasError) {
      val prev = t.subterms(i)
      val nSubterm = prev.mapVars(f)
      if (!(prev eq nSubterm)) {
        val meta = t.metainfo.components(i)
        val check = meta.constraint.leftUnifyInSubst(cs,nSubterm)
        check match {
          case UnificationSuccess(s1) => cs = s1
            if (!wasChanged) {
              newSubterms = new Array[ITerm](t.arity())
              t.subterms.copyToArray(newSubterms,0,i)
              wasChanged = true
            }
            newSubterms(i) = nSubterm
          case failure:UnificationFailure =>
            wasError = true
            r = failure.toIErrorTerm
        }
      }
    }
    if (wasError) {
      r
    } else if (wasChanged) {
      new PlainStructured(t.metainfo,newSubterms.toIndexedSeq)
    } else {
      istructured(t)
    }
  }

  override def substVars(t: Carrier, s: Substitution[IVarTerm,ITerm]): ITerm = {
    var i=0
    var wasError = false
    var wasChanged = false
    var r: ITerm = t
    var newSubterms: Array[ITerm] = null
    var cs = s
    while(i < arity(t) && !wasError) {
       val prev = t.subterms(i)
       val nSubterm = prev.substVars(s)
       if (!(prev eq nSubterm)) {
         val meta = t.metainfo.components(i)
         val check = meta.constraint.leftUnifyInSubst(cs,nSubterm)
         check match {
           case UnificationSuccess(s1) => cs = s1
             if (!wasChanged) {
               newSubterms = new Array[ITerm](t.arity())
               t.subterms.copyToArray(newSubterms,0,i)
               wasChanged = true
             }
             newSubterms(i) = nSubterm
           case failure:UnificationFailure =>
             wasError = true
             r = failure.toIErrorTerm
         }
       } else {
         //
         if (wasChanged) {
           newSubterms(i) = nSubterm
         }
       }
       i = i+1
     }
    if (wasError) {
      r
    } else if (wasChanged) {
      new PlainStructured(t.metainfo,newSubterms.toIndexedSeq)
    } else istructured(t)
  }


  override def leftUnifyInSubst(t: Carrier, s: Substitution[IVarTerm,ITerm], o: ITerm): UnificationResult = {
    o match {
      case IStructured(otherStructured) =>
        name(t).leftUnifyInSubst(s,otherStructured.name()) match {
          case UnificationSuccess(s) =>
            unifySubterms(t,s,otherStructured,0)
          case f: UnificationFailure =>
            UnificationFailure("name mismath",iterm(t),o,Some(f),s)
        }
      case IEtaTerm(oEta) =>
        // TODO: think, how to move context between, (structure context representation)
        oEta.baseTerm() match {
          case IStructured(oEtaStructured) =>
            UnificationFailure("matching structured and eta is not supported yet",iterm(t),o,None,s)
        }
        //leftUnifyInSubst(t,s,EtaEliminate(oEta.baseTerm()))
        UnificationFailure("term kind mismatch",iterm(t),o,None,s)
      case _ =>
        UnificationFailure("term kind mismatch",iterm(t),o,None,s)
    }
  }


  @tailrec
  private def unifySubterms(carrier: Carrier, s: Substitution[IVarTerm, ITerm], structured: IStructured, i: Int): UnificationResult = {
    if (i==arity(carrier)) {
      UnificationSuccess(s)
    } else {
      subtermMeta(carrier,i) match {
        case FastRefOption.Some(leftMeta) =>
          val left = subterm(carrier,i).get()
          structured.subterm(leftMeta.name).orElse(leftMeta.defValue) match {
            case FastRefOption.Some(right) =>
              left.leftUnifyInSubst(s,right) match {
                case UnificationSuccess(s1) => unifySubterms(carrier,s1,structured,i+1)
                case failure: UnificationFailure =>
                  UnificationFailure(s"mismatched subterm ${leftMeta.name}",istructured(carrier),structured,Some(failure),s)
              }
            case FastRefOption.Empty(e) =>
              // No name and no default value
              UnificationFailure(s"Can't find component with name ${leftMeta.name}",istructured(carrier),structured,None,s)
          }
      }
    }
  }

  override def mapSubterms(t: Carrier, f: ITerm => ITerm): ITerm = {
    t.mapSubterms(f)
  }

}

case class StructuredMetainfo(name: IName, components: IndexedSeq[StructuredComponent], nameIndexes:Map[IName,Int])

class PlainStructured(val metainfo: StructuredMetainfo,
    val subterms: IndexedSeq[ITerm]) extends IStructured
{

  override type Carrier = PlainStructured

  override def tcStructured: TCStructured[Carrier] = TCPlainStructured

  override def carrier: PlainStructured = this

  override def name(): IName = {
    metainfo.name
  }

  override def arity():Int = {
    subterms.size
  }

  override def subterm(i:Int): FastRefOption[ITerm] = {
    FastRefOption(subterms.applyOrElse(i,null))
  }

  override def subterm(n:IName): FastRefOption[ITerm] = {
    metainfo.nameIndexes.get(n) match {
      case Some(i) => subterm(i)
      case None =>FastRefOption.empty
    }
  }

  override def mapSubterms(f: ITerm => ITerm): ITerm = {
     var i=0
     var errorTerm: FastRefOption[IErrorTerm] = FastRefOption.empty
     var nSubterms = new Array[ITerm](subterms.size)
     while(i < subterms.size && errorTerm.isEmpty) {
       val e = subterms(i)
       val ne = f(e)
       val m = metainfo.components(i)
       if (!(ne eq e)) {
         m.constraint.leftUnifyInSubst(MapBasedVarSubstitution.empty, ne) match {
           case UnificationSuccess(_) => nSubterms(i) = ne
           case f: UnificationFailure => errorTerm = f.toIErrorTerm.asError()
         }
       } else {
         nSubterms(i) = ne
       }
       i = i+1
     }
     errorTerm.getOrElse(new PlainStructured(metainfo,nSubterms.toIndexedSeq))
  }


}

object PlainStructured {

  /**
    *
    * @param sname
    * @param subtermsMetas meta from structures components.  Note, that index in components is ignored, instead
    *                      used index in subtermsMetas
    * @return
    */
  def createMetainfo(sname:String, subtermsMetas:Seq[StructuredComponent]):StructuredMetainfo = {
    val s0 = StructuredMetainfo(StringName(sname),IndexedSeq.empty,Map.empty)
    subtermsMetas.foldLeft(s0){ (s,e) =>
      val l = s.components.length
      val e1 = e.copy(index = l)
      s.copy(
        components = s.components :+ e1,
        nameIndexes = s.nameIndexes.updated(e.name,l)
      )
    }
  }

  def freeMetainfo(name:String, subtermNames: String*) = ???



}

object TCStructuredEtaRepresentation extends TCStructured[StructuredEtaRepresentation]
{
  override def istructured(t: StructuredEtaRepresentation): IStructured = t

  override def name(t: StructuredEtaRepresentation): IName = t.name()

  override def arity(t: StructuredEtaRepresentation): Int = t.arity()

  override def subterm(t: StructuredEtaRepresentation, i: Int): FastRefOption[ITerm] = ???

  override def subterm(t: StructuredEtaRepresentation, n: IName): FastRefOption[ITerm] = ???

  override def subtermMeta(t: StructuredEtaRepresentation, i: Int): FastRefOption[StructuredComponent] = ???

  override def subtermMeta(t: StructuredEtaRepresentation, n: IName): FastRefOption[StructuredComponent] = ???

  override def foldSubtermsWhile[S](t: StructuredEtaRepresentation, s0: S)(f: (S, ITerm) => S)(p: S => Boolean): S = ???

  override def mapVars(t: StructuredEtaRepresentation, f: IVarTerm => ITerm): ITerm = ???

  override def leftUnifyInSubst(t: StructuredEtaRepresentation, s: Substitution[IVarTerm,ITerm], o: ITerm): UnificationResult = ???

  override def mapSubterms(t: StructuredEtaRepresentation, f: ITerm => ITerm): ITerm = ???
}

class StructuredEtaRepresentation(iEtaTerm: IEtaTerm, structuredBase: IStructured) extends IStructured
{
  override def tcStructured: TCStructured[StructuredEtaRepresentation] = TCStructuredEtaRepresentation

  override type Carrier = StructuredEtaRepresentation

  override def carrier: StructuredEtaRepresentation = this

  override def name(): IName = structuredBase.name()

  override def arity(): Int = structuredBase.arity()

  override def subterm(i: Int): FastRefOption[ITerm] = ???


}