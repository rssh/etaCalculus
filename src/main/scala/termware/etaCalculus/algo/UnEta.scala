package termware.etaCalculus.algo

import termware.etaCalculus._
import termware.util.FastRefOption

object UnEta extends TermKindTransformer[ITerm] {

  override def onName(name: IName, vo: Map[IEtaTerm, IEtaTerm]): ITerm = name

  override def onVar(varTerm: IVarTerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    varTerm.owner.context().get(varTerm.name) match {
      case FastRefOption.Some(value) =>
         if (value.hasPatterns()) {
           varTerm
         } else {
           value.kindTransform(UnEta, vo)
         }
      case FastRefOption.Empty() => IErrorTerm(s"Invalid name in term: ${varTerm}")
    }
  }

  override def onPrimitive(primitive: IPrimitive, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    primitive
  }

  override def onStructured(structured: IStructured, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    structured.mapVars( v =>
       v.owner.context().get(v.name) match {
         case FastRefOption.Some(value) => value.kindTransform(UnEta, vo)
         case FastRefOption.Empty() => IErrorTerm(s"can't resolve variable ${v.owner}")
       },
       vo
    )
  }

  override def onEta(eta: IEtaTerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    eta.baseTerm().kindTransform(UnEta,vo)
  }

  override def onError(error: IErrorTerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = error

  override def onPatternCondition(patternCondition: IPatternCondition, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    patternCondition
  }

  override def onArrows(arrows: IArrows, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    val nPairs = arrows.linear().map{
      case (x,y) => (x.kindTransform(this,vo),y.kindTransform(this,vo))
    }
    IArrows.fromPairs(nPairs: _*) match {
      case Right(x) => x
      case Left(contratiction) => // impossible
        throw new IllegalStateException("contradition during uneta:"+contratiction.message)
    }
  }

}
