package termware.etaCalculus

import termware.util.FastRefOption

import scala.annotation.tailrec
import scala.collection.immutable.IntMap


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

  def foldSubterms[S](t:T,s0:S)(f: (S,ITerm) => S)(p: S => Boolean): S  =
    foldSubtermsWhile(t,s0)(f)(_ => true)

  def foldSubtermsWhile[S](t:T,s0:S)(f: (S,ITerm) => S)(p: S => Boolean): S

  def foldMetas[S](t:T, s0:S)(f: (S,StructuredComponent) => S): S = {
    foldMetasWhile(t,s0)(f)(_ => true)
  }

  def foldMetasWhile[S](t:T,s0:S)(f: (S,StructuredComponent) => S)(p: S=>Boolean):S

  def mapSubterms(t:T, f: ITerm => ITerm, vo:Map[IEtaTerm,IEtaTerm], fProcessVO: Boolean): ITerm

  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption.empty
  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption.empty
  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption.empty
  override def tcStructured(t: T): FastRefOption[TCStructured[T]] = FastRefOption(this)
  override def tcEta(t: T): FastRefOption[TCEtaTerm[T]] = FastRefOption.empty
  override def tcError(t: T): FastRefOption[TCErrorTerm[T]] = FastRefOption.empty
  override def tcPatternCondition(t: T): FastRefOption[TCPatternCondition[T]] = FastRefOption.empty
  override def tcArrows(t: T): FastRefOption[TCArrows[T]] = FastRefOption.empty

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

  def subtermMeta(i:Int): FastRefOption[StructuredComponent] = {
    tcStructured.subtermMeta(carrier,i)
  }

  def subtermMeta(n:IName): FastRefOption[StructuredComponent] = {
    tcStructured.subtermMeta(carrier,n)
  }


  def mapSubterms(f:ITerm => ITerm, vo:Map[IEtaTerm,IEtaTerm], fProcessVO: Boolean): ITerm = {
    tcStructured.mapSubterms(carrier,f,vo, fProcessVO)
  }

  def foldSubtermsWhile[S](s0:S)(f: (S,ITerm) => S)(p: S => Boolean): S = {
    tcStructured.foldSubtermsWhile(carrier,s0)(f)(p)
  }

  def foldMetas[S](s0:S)(f: (S,StructuredComponent) => S): S = {
    tcStructured.foldMetas(carrier,s0)(f)
  }

  def foldMetasWhile[S](s0:S)(f: (S,StructuredComponent) => S)(p: S=>Boolean): S = {
    tcStructured.foldMetasWhile(carrier,s0)(f)(p)
  }

  def metas(): IndexedSeq[StructuredComponent]

  override def kindTransform[B](matcher: TermKindTransformer[B], vo: Map[IEtaTerm,IEtaTerm]): B = {
    matcher.onStructured(this,vo)
  }

  override def kindFold[S](s0: S)(folder: TermKindFolder[S]): S = {
    folder.onStructured(this,s0)
  }

  def builder(): IStructuredBuilder

}


trait IStructuredBuilder {

  def  addArg(value:ITerm): Either[String,IStructuredBuilder]

  def  setNamedArg(name: IName, value: ITerm): Either[String, IStructuredBuilder]

  def  toTerm(): IStructured

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

  // make all default values explicit
  def fullVariant(arg:IStructured): Either[String,IStructured] = {
    val metas = arg.metas()
    val metaArity = metas.size
    // Think - prototype with mutable builder
    var builder = arg.builder()
    var i = arg.arity()
    var retval: Either[String,IStructured] = Right(arg)
    while(i < metas.size && retval.isRight) {
      metas(i).defValue match {
        case None =>
          retval = Left(s"Can't expand: ${arg.name()}(${i}) have no default value")
        case Some(v) => builder.addArg(v) match {
          case Left(message) => retval = Left(message)
          case Right(nBuilder) => builder = nBuilder
        }
      }
      i = i+1
    }
    if (retval.isRight) {
      retval = Right(builder.toTerm())
    }
    retval
  }

  def allVariants(arg:IStructured): Seq[IStructured] = {
    var candidate = arg
    var retval: List[IStructured] = List(arg)
    var i = arg.arity()
    val metas = arg.metas()
    val builder = arg.builder()
    var quit = false
    while(i < metas.size && !quit) {
      metas(i).defValue match {
        case None => quit = false
        case Some(v) =>
          builder.addArg(v) match {
            case Left(e) => // error, assume
                            quit = true
            case Right(nBuilder) =>
               retval = nBuilder.toTerm() :: retval
          }
      }
      i = i + 1
    }
    retval.reverse
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

  override def foldMetas[S](t: Carrier, s0: S)(f: (S, StructuredComponent) => S): S = {
    var s = s0
    var i = 0
    val n = arity(t)
    while(i<n) {
      s = f(s,t.metainfo.components(i))
      i += 1
    }
    s
  }

  override def foldMetasWhile[S](t: Carrier, s0: S)(f: (S, StructuredComponent) => S)(p: S => Boolean): S = {
    var s = s0
    var i = 0
    val n = arity(t)
    while(p(s) && i<n) {
      s = f(s,t.metainfo.components(i))
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

  /*
  override def subst[N<: ITerm, V <: ITerm](t: Carrier, s: Substitution[N, V], vo: Map[IEtaTerm, IEtaTerm])(implicit nTag:ClassTag[N]): ITerm = {
     t.subst(s,vo)
  }

   */

  override def leftUnifyInSubst(t: Carrier, s: VarSubstitution, o: ITerm): UnificationResult = {
    o match {
      case IStructured(otherStructured) =>
        name(t).leftUnifyInSubst(s,otherStructured.name()) match {
          case UnificationSuccess(s) =>
            unifySubterms(t,s,otherStructured,0)
          case f: UnificationFailure =>
            UnificationFailure("name mismath",iterm(t),o,s,Some(f))
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
        UnificationFailure("term kind mismatch",iterm(t),o,s)
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
                  UnificationFailure(s"mismatched subterm ${leftMeta.name}",istructured(carrier),structured,s,Some(failure))
              }
            case FastRefOption.Empty() =>
              // No name and no default value
              UnificationFailure(s"Can't find component with name ${leftMeta.name}",istructured(carrier),structured,s)
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

  override def metas(): IndexedSeq[StructuredComponent] = {
    metainfo.components
  }


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

  override def builder(): IStructuredBuilder =
    new PlainStructuredBuilder(this,List(), IntMap.empty)

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

case class ArgsWithOptMeta(arg:ITerm, meta: Option[StructuredComponent])

case class PlainStructuredBuilder(
    origin: PlainStructured,
    additionalArgs: List[ArgsWithOptMeta],
    changedArgs: IntMap[ITerm]
 ) extends IStructuredBuilder {

  override def addArg(value: ITerm): Either[String, IStructuredBuilder] = {
    Right(copy(additionalArgs = ArgsWithOptMeta(value,None)::additionalArgs))
  }

  override def setNamedArg(name: IName, value: ITerm): Either[String, IStructuredBuilder] = {
    origin.subtermMeta(name) match {
      case FastRefOption.Empty() =>
          val nArg = ArgsWithOptMeta(value, Some(StructuredComponent(name)))
          Right(copy(additionalArgs = nArg :: additionalArgs))
      case FastRefOption.Some(m) =>
          m.constraint.leftUnifyInSubst(VarSubstitution.empty(),value) match {
            case UnificationSuccess(s) =>
              Right(copy(changedArgs = changedArgs.updated(m.index,value)))
            case UnificationFailure(msg,l,r,p,s) =>
              Left("Check unfication failed")
          }
    }
  }

  override def toTerm(): IStructured = {
    import scala.collection.mutable.{IndexedSeq => MutableIndexedSeq}
    val args = additionalArgs.reverse
    val subtermsBuilder = Array.newBuilder[ITerm]
    subtermsBuilder.addAll(origin.subterms)
    val componentsBuilder = MutableIndexedSeq.newBuilder[StructuredComponent]
    var nAddedComponents = 0
    val nMetas = origin.metainfo.components.size



    def addComponent(c: StructuredComponent): Unit = {
      if (nAddedComponents == 0 && nMetas != 0) {
        componentsBuilder.addAll(origin.metainfo.components)
        nAddedComponents += origin.metainfo.components.size
      }
      componentsBuilder.addOne(c)
    }

    var i = origin.subterms.size
    for( a <- args) {
       a.meta match {
         case None =>
           subtermsBuilder.addOne(a.arg)
           if (i >= nMetas) {
             addComponent(StructuredComponent(IntName(i)))
           }
         case Some(m) =>
             while(i < nMetas) {
               val cm = origin.metainfo.components(i)
               cm.defValue match {
                 case Some(x) => subtermsBuilder.addOne(x)
                 case None => throw new IllegalStateException("addName before meta exhaused")
               }
               i = i+1
             }
           subtermsBuilder.addOne(a.arg)
           addComponent(m)
       }
       i = i + 1
    }
    val subtermsArray = subtermsBuilder.result()
    for( (i,v) <- changedArgs) {
       subtermsArray(i) = v
    }
    val nMetaInfo = if (nAddedComponents > 0) {
      origin.metainfo.copy(components = componentsBuilder.result().toIndexedSeq)
    } else {
      origin.metainfo
    }
    PlainStructured(nMetaInfo,subtermsArray.toIndexedSeq)
  }

}