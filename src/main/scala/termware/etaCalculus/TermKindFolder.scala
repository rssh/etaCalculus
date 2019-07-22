package termware.etaCalculus

trait TermKindFolder[S] {

  def onName(name: IName, state:S): S

  def onVar(varTerm: IVarTerm, state:S): S

  def onPrimitive(primitive: IPrimitive, state: S): S

  def onStructured(structured: IStructured, state: S): S

  def onEta(eta: IEtaTerm, state: S): S

  def onError(error: IErrorTerm, state: S): S

  def onPatternCondition(patternCondition: IPatternCondition, state: S): S

}
