package termware.etaCalculus.algo

import termware.etaCalculus.{IArrows, IErrorTerm, IEtaTerm, IName, IPatternCondition, IPrimitive, IStructured, ITerm, IVarTerm, TermKindTransformer}

import scala.annotation.tailrec

object Contains {

  def apply(owner: ITerm, target: ITerm): Boolean = {
     val t = new ContainsTransformer(target)
     owner.kindTransform(t,Map.empty)
  }

  class ContainsTransformer(target: ITerm) extends TermKindTransformer[Boolean] {
    def onName(name: IName, vo: Map[IEtaTerm,IEtaTerm]): Boolean =
      name == target

    def onVar(varTerm: IVarTerm, vo: Map[IEtaTerm,IEtaTerm]): Boolean =
      varTerm == target

    def onPrimitive(primitive: IPrimitive, vo: Map[IEtaTerm,IEtaTerm]): Boolean =
      primitive == target

    def onStructured(structured: IStructured, vo: Map[IEtaTerm,IEtaTerm]): Boolean = {
      structured.foldSubtermsWhile(false){
        (s,e) => e.kindTransform(this,vo)
      }(!_)
    }

    def onEta(eta: IEtaTerm, vo: Map[IEtaTerm,IEtaTerm]): Boolean = {
      eta.baseTerm().kindTransform(this,vo) ||
      eta.context().foldWhile(false)(!_){
        (s,n,t) => t.kindTransform(this,vo)
      }
    }

    def onError(error: IErrorTerm, vo: Map[IEtaTerm,IEtaTerm]): Boolean =
      error == target

    def onPatternCondition(patternCondition: IPatternCondition, vo: Map[IEtaTerm,IEtaTerm]): Boolean = {
      patternCondition.expression.kindTransform(this,vo)
    }

    def onArrows(arrow:IArrows, vo: Map[IEtaTerm,IEtaTerm]): Boolean = {
      @tailrec
      def inSeq(seq: Seq[(ITerm,ITerm)]): Boolean = {
        seq.headOption match {
          case None => false
          case Some(h) => if (!h._1.kindTransform(this,vo) &&
                              !h._2.kindTransform(this,vo))
                           inSeq(seq.tail)
                          else false
        }
      }
      inSeq(arrow.linear())
    }

  }

}
