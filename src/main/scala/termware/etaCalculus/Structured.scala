package termware.etaCalculus

import termware.etaCalculus.PlainStructured.{createMetainfo, freeMetainfo}
import termware.util.FastRefOption

import scala.annotation.tailrec
import scala.reflect.ClassTag

case class StructuredComponent(
    name:IName,
    index:Int = -1,
    defValue: Option[ITerm] = None,  //can't be valuetype: see https://github.com/scala/bug/issues/7396
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

  def mapSubterms(t:T, f: ITerm => ITerm, vo:Map[IEtaTerm,IEtaTerm], fProcessVO: Boolean): ITerm

  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption.empty
  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption.empty
  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption.empty
  override def tcStructured(t: T): FastRefOption[TCStructured[T]] = FastRefOption(this)
  override def tcEta(t: T): FastRefOption[TCEtaTerm[T]] = FastRefOption.empty
  override def tcError(t: T): FastRefOption[TCErrorTerm[T]] = FastRefOption.empty
  override def tcPatternCondition(t: T): FastRefOption[TCPatternCondition[T]] = FastRefOption.empty

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

  def mapSubterms(f:ITerm => ITerm, vo:Map[IEtaTerm,IEtaTerm], fProcessVO: Boolean): ITerm = {
    tcStructured.mapSubterms(carrier,f,vo, fProcessVO)
  }

  def foldSubtermsWhile[S](s0:S)(f: (S,ITerm) => S)(p: S => Boolean): S = {
    tcStructured.foldSubtermsWhile(carrier,s0)(f)(p)
  }

  override def kindTransform[B](matcher: TermKindTransformer[B], vo: Map[IEtaTerm,IEtaTerm]): B = {
    matcher.onStructured(this,vo)
  }

  override def kindFold[S](s0: S)(folder: TermKindFolder[S]): S = {
    folder.onStructured(this,s0)
  }

}


object IStructured {

  def unapply(arg: ITerm): FastRefOption[IStructured] = arg.asStructured()

  @inline
  def freeNamed(name:String,byName:(String,ITerm)*): ITerm = {
    PlainStructured.freeNamed(name,byName: _*)
  }

  @inline
  def freeIndexed(name:String, byIndex: ITerm*): ITerm = {
    PlainStructured.freeIndexed(name,byIndex: _*)
  }

  def nameFreeIndexed(name: IName, byIndex: ITerm*): ITerm = {
    PlainStructured.nameFreeIndexed(name,byIndex: _*)
  }


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


  override def mapVars(t: Carrier, f: IVarTerm => ITerm, vo: Map[IEtaTerm,IEtaTerm]): ITerm = {
    mapSubterms(t,{ prev =>
      prev.mapVars(f,vo)
    },vo,true)
  }

  override def substVars(t: Carrier, s: VarSubstitution, vo: Map[IEtaTerm,IEtaTerm]): ITerm = {
    mapSubterms(t,{ _.substVars(s,vo) },vo,true)
  }

  override def subst[N<: ITerm, V <: ITerm](t: Carrier, s: Substitution[N, V], vo: Map[IEtaTerm, IEtaTerm])(implicit nTag:ClassTag[N]): ITerm = {
     t.subst(s,vo)
  }

  override def map(t: Carrier, f: ITerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
     t.map(f,vo)
  }

  override def leftUnifyInSubst(t: Carrier, s: VarSubstitution, o: ITerm): UnificationResult = {
    o match {
      case IStructured(otherStructured) =>
        name(t).leftUnifyInSubst(s,otherStructured.name()) match {
          case UnificationSuccess(s) =>
            unifySubterms(t,s,otherStructured,0)
          case f: UnificationFailure =>
            UnificationFailure("name mismath",iterm(t),o,Some(f),s)
        }
      case IEtaTerm(oEta) =>
        leftUnifyInSubst(t,s,oEta.baseTerm())
        // TODO: think, how to move context between, (structure context representation)
        //oEta.baseTerm() match {
        //  case IStructured(oEtaStructured) =>
            //UnificationFailure("matching structured and eta is not supported yet",iterm(t),o,None,s)
        //}
        //leftUnifyInSubst(t,s,EtaEliminate(oEta.baseTerm()))
        //UnificationFailure("term kind mismatch",iterm(t),o,None,s)
      case _ =>
        UnificationFailure("term kind mismatch",iterm(t),o,None,s)
    }
  }


  @tailrec
  private def unifySubterms(carrier: Carrier, s: VarSubstitution, structured: IStructured, i: Int): UnificationResult = {
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
            case FastRefOption.Empty() =>
              // No name and no default value
              UnificationFailure(s"Can't find component with name ${leftMeta.name}",istructured(carrier),structured,None,s)
          }
      }
    }
  }

  override def mapSubterms(t: Carrier, f: ITerm => ITerm, vo: Map[IEtaTerm,IEtaTerm], fProcessVO: Boolean): ITerm = {
    t.mapSubterms(f,vo, fProcessVO)
  }

  override def hasPatternsRec(t: Carrier, trace:Map[IVarTerm,Boolean]): Boolean = {
    t.hasPatternsRec(trace)
  }

  override def termEqNoRef(t: Carrier, otherTerm: ITerm): Boolean = {
    t.termEqNoRef(otherTerm)
  }

}

case class StructuredMetainfo(name: IName, components: IndexedSeq[StructuredComponent], nameIndexes:Map[IName,Int])

case class PlainStructured(val metainfo: StructuredMetainfo,
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

  override def mapSubterms(f: ITerm => ITerm, vo:Map[IEtaTerm,IEtaTerm], fProcessVo: Boolean): ITerm = {

     def changeOwnerIfVar(t:ITerm):ITerm = {
       if (fProcessVo) {
         t
       } else {
         t match {
           case IVarTerm(v) => vo.get(v.owner).map(v.changeOwner(_)).getOrElse(v)
           case _ => t.kindTransform(VarOwnerChangeTransformer, vo)
         }
       }
     }

     var i=0
     var errorTerm: FastRefOption[IErrorTerm] = FastRefOption.empty
     var nSubterms = new Array[ITerm](subterms.size)
     while(i < subterms.size && errorTerm.isEmpty) {
       val e = subterms(i)
       val ne = f(e)
       val m = metainfo.components(i)
       if (!(ne eq e)) {
         m.constraint.leftUnifyInSubst(MapBasedVarSubstitution.empty, ne) match {
           case UnificationSuccess(_) => nSubterms(i) = changeOwnerIfVar(ne)
           case f: UnificationFailure => errorTerm = f.toIErrorTerm.asError()
         }
       } else {
         nSubterms(i) = changeOwnerIfVar(ne)
       }
       i = i+1
     }
     errorTerm.getOrElse(new PlainStructured(metainfo,nSubterms.toIndexedSeq))
  }

  def fixVars(t:ITerm, vo:Map[IEtaTerm,IEtaTerm]): ITerm = {
    if (vo.isEmpty) t
    else t.kindTransform(VarOwnerChangeTransformer,vo)
  }

  override def subst[N <: ITerm, V <: ITerm](s: Substitution[N, V], vo: Map[IEtaTerm, IEtaTerm])(implicit nTag: ClassTag[N]): ITerm = {
    this match {
      case nTag(thisN) => s.get(thisN).map(fixVars(_,vo)).getOrElse(substInternal(s,vo))
      case _ => substInternal(s,vo)
    }

  }

  def substInternal[N <: ITerm,V <:ITerm](s:Substitution[N,V], vo:Map[IEtaTerm,IEtaTerm])(implicit nTag: ClassTag[N]): ITerm = {
    mapSubterms({
      case nTag(x) => val tc = s.get(x) match {
          case FastRefOption.Some(v) => v
          case FastRefOption.Empty() => x
        }
        if (vo.isEmpty)
          tc
        else
          tc.kindTransform(VarOwnerChangeTransformer,vo)
      case t => t.subst(s,vo)
    }, vo, true)
  }

  override def map(f: ITerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = mapSubterms(f,vo, true)

  override def termEqNoRef(o: ITerm): Boolean = {
    o match {
      case IStructured(otherStructured) =>
        if (arity() != otherStructured.arity() || name() != otherStructured.name()) {
          false
        } else {
          var i=0
          var mismatchDetected = false
          while(i < subterms.size && !mismatchDetected) {
            val l = subterms(i)
            val r = otherStructured.subterm(i).get()
            if (!l.termEqNoRef(r)) {
              mismatchDetected = true
            }
            i = i+1
          }
          !mismatchDetected
        }
      case IEtaTerm(eta) => termEqNoRef(eta.baseTerm())
      case IVarTerm(v) => false //  value substitution is nto stored, to prevent endless loops
      case _ => false
    }
  }


  override def hasPatterns(): Boolean = {
    _hasPatterns
  }

  override def hasPatternsRec(trace: Map[IVarTerm, Boolean]): Boolean = {
    subterms.exists(_.hasPatternsRec(trace))
  }

  private[this] lazy val _hasPatterns = hasPatternsRec(Map.empty)

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
    createMetainfo(StringName(sname),subtermsMetas)
  }

  def createMetainfo(name:IName, subtermsMetas:Seq[StructuredComponent]):StructuredMetainfo = {
    val s0 = StructuredMetainfo(name,IndexedSeq.empty,Map.empty)
    subtermsMetas.foldLeft(s0){ (s,e) =>
      val l = s.components.length
      val e1 = e.copy(index = l)
      s.copy(
        components = s.components :+ e1,
        nameIndexes = s.nameIndexes.updated(e.name,l)
      )
    }
  }


  def freeMetainfo(name:String, subtermNames: String*): StructuredMetainfo = {
    val metas = subtermNames.zipWithIndex.map{ case (name,index) =>StructuredComponent(StringName(name),index) }
    createMetainfo(name,metas)
  }

  def freeNamed(name:String,byName:(String,ITerm)*): ITerm = {
    val metaInfo = freeMetainfo(name,byName.map(_._1): _*)
    new PlainStructured(metaInfo, byName.map(_._2).toIndexedSeq)
  }

  def freeIndexed(name:String, byIndex: ITerm*): ITerm = {
    val metas = (0 until byIndex.length).map(i => StructuredComponent(IntName(i),i))
    val metaInfo = createMetainfo(name,metas)
    new PlainStructured(metaInfo, byIndex.toIndexedSeq)
  }

  def nameFreeIndexed(name:IName, byIndex: ITerm*): ITerm = {
    val metas = (0 until byIndex.length).map(i => StructuredComponent(IntName(i),i))
    val metaInfo = createMetainfo(name,metas)
    new PlainStructured(metaInfo, byIndex.toIndexedSeq)
  }

}

