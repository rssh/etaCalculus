package termware.etaCalculus.algo

import termware.etaCalculus._

object UnEta extends TermKindTransformer[ITerm] {

  override def onName(name: IName, vo: Map[IEtaTerm, IEtaTerm]): ITerm = name

  override def onVar(varTerm: IVarTerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    varTerm.owner.context().get(varTerm.name) match {
      case Some(value) =>
         if (value.hasPatterns()) {
           varTerm
         } else {
           value.transform(UnEta, vo)
         }
      case None => IErrorTerm(s"Invalid name in term: ${varTerm}")
    }
  }

  override def onPrimitive(primitive: IPrimitive, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    primitive
  }

  override def onStructured(structured: IStructured, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    structured.mapVars( v =>
       v.owner.context().get(v.name) match {
         case Some(value) => value.transform(UnEta, vo)
         case None => IErrorTerm(s"can't resolve variable ${v.owner}")
       },
       vo
    )
  }

  override def onEta(eta: IEtaTerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    eta.baseTerm().transform(UnEta,vo)
  }

  override def onError(error: IErrorTerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = error

  override def onPatternCondition(patternCondition: IPatternCondition, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    patternCondition
  }

}
