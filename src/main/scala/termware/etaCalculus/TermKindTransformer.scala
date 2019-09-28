package termware.etaCalculus

trait TermKindTransformer[B] {

  def onName(name: IName, vo: Map[IEtaTerm,IEtaTerm]): B

  def onVar(varTerm: IVarTerm, vo: Map[IEtaTerm,IEtaTerm]): B

  def onPrimitive(primitive: IPrimitive, vo: Map[IEtaTerm,IEtaTerm]): B

  def onStructured(structured: IStructured, vo: Map[IEtaTerm,IEtaTerm]): B

  def onEta(eta: IEtaTerm, vo: Map[IEtaTerm,IEtaTerm]): B

  def onError(error: IErrorTerm, vo: Map[IEtaTerm,IEtaTerm]): B

  def onPatternCondition(patternCondition: IPatternCondition, vo: Map[IEtaTerm,IEtaTerm]): B

  def onArrows(arrow:IArrows, vo: Map[IEtaTerm,IEtaTerm]): B

}

