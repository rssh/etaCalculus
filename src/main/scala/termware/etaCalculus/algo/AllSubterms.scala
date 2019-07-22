package termware.etaCalculus.algo

import java.util.IdentityHashMap

import termware.etaCalculus.{IErrorTerm, IEtaTerm, IName, IPatternCondition, IPrimitive, IStructured, ITerm, IVarTerm, TermKindTransformer}

case class AllSubterms(t: ITerm) {

  trait CachingBooleanTransformer extends TermKindTransformer[Boolean] {

    def initValue: Boolean

    val cache = collection.mutable.Map[ITerm,Boolean]()

    def cachedTransform(x:ITerm, vo:Map[IEtaTerm,IEtaTerm]): Boolean = {
      cachedApply(x)(x.kindTransform(this,vo))
    }

    def cachedApply(x:ITerm)(f: => Boolean): Boolean = {
      cache.get(x) match {
        case Some(v) => v
        case None =>
           cache.put(x,initValue)
           val r = f
           cache.put(x,r)
           r
      }
    }

    def printCache():Unit = {
      cache.foreach{case (x,y) => System.err.println(s"${x}->${y}")}
    }

  }


  class ForallTransformer(p: ITerm => Boolean) extends CachingBooleanTransformer {

    override def initValue: Boolean = true

    // hold eta and structured, for which we can have cyclic loops

    override def onName(name: IName, vo: Map[IEtaTerm, IEtaTerm]): Boolean = p(name)

    override def onVar(varTerm: IVarTerm, vo: Map[IEtaTerm, IEtaTerm]): Boolean = p(varTerm)

    override def onPrimitive(primitive: IPrimitive, vo: Map[IEtaTerm, IEtaTerm]): Boolean = p(primitive)

    override def onStructured(structured: IStructured, vo: Map[IEtaTerm, IEtaTerm]): Boolean = {
      cachedApply(structured){
        structured.foldSubtermsWhile(true) {
          (s, e) => s && e.kindTransform(this, vo)
        }(s => s)
      }
    }

    override def onEta(eta: IEtaTerm, vo: Map[IEtaTerm, IEtaTerm]): Boolean = {
      cachedApply(eta){
        val rc: Boolean = eta.context().foldWhile(true)(s=>s){ (s,n,v) =>
          s && cachedTransform(v,vo)
        }
        if (rc) {
          eta.baseTerm().kindTransform(this,vo)
        } else rc
      }
    }

    override def onError(error: IErrorTerm, vo: Map[IEtaTerm, IEtaTerm]): Boolean = {
      p(error)
    }

    override def onPatternCondition(patternCondition: IPatternCondition, vo: Map[IEtaTerm, IEtaTerm]): Boolean = p(patternCondition)


  }

  def forall(p: ITerm => Boolean): Boolean = {
     val transformer = new ForallTransformer(p)
     t.kindTransform(transformer,Map.empty)
  }

  class ExistsTransformer(p: ITerm => Boolean) extends CachingBooleanTransformer {

    override def initValue: Boolean = false

    override def onName(name: IName, vo: Map[IEtaTerm, IEtaTerm]): Boolean = {
      p(name)
    }

    override def onVar(varTerm: IVarTerm, vo: Map[IEtaTerm, IEtaTerm]): Boolean = {
      p(varTerm)
    }

    override def onPrimitive(primitive: IPrimitive, vo: Map[IEtaTerm, IEtaTerm]): Boolean = {
      p(primitive)
    }

    override def onStructured(structured: IStructured, vo: Map[IEtaTerm, IEtaTerm]): Boolean = {
      cachedApply(structured){
        structured.foldSubtermsWhile(false)(
          (s,e) => s && e.kindTransform(this,vo))(s => !s)
      }
    }

    override def onEta(eta: IEtaTerm, vo: Map[IEtaTerm, IEtaTerm]): Boolean = {
      cachedApply(eta){
        val rc = eta.context().foldWhile(false)(!_){ case (s,n,v) =>
          s && cachedTransform(v,vo)
        }
        if (!rc) {
          cachedTransform(eta.baseTerm(), vo)
        } else rc
      }
    }

    override def onError(error: IErrorTerm, vo: Map[IEtaTerm, IEtaTerm]): Boolean = {
      p(error)
    }

    override def onPatternCondition(patternCondition: IPatternCondition, vo: Map[IEtaTerm, IEtaTerm]): Boolean = p(patternCondition)
  }

  def exists(p: ITerm => Boolean): Boolean = {
    val transformer = new ExistsTransformer(p)
    t.kindTransform(transformer,Map.empty)
  }


  def contains(x:ITerm): Boolean = exists(_ == x)


}
