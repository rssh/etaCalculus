package termware.etaCalculus

trait TermKindFolderState {

  def updateVo(newEta: IEtaTerm, oldEta: IEtaTerm)

}

trait TermKindFolder[S] {

  def onName(name: IName, state:S): S

  def onVar(varTerm: IVarTerm, state:S): S

  def onPrimitive(primitive: IPrimitive, state: S): S

  def onStructured(structured: IStructured, state: S): S

  def onEta(eta: IEtaTerm, state: S): S

  def onError(error: IErrorTerm,  state: S): S

  def onPatternCondition(patternCondition: IPatternCondition, state: S): S

}

/**
  * Basic Empty kind folder which do nothing.
  * @tparam S
  */
class BaseTermKindFolder[S] extends TermKindFolder[S] {

  override def onName(name: IName, state: S): S = state

  override def onVar(varTerm: IVarTerm, state: S): S = state

  override def onPrimitive(primitive: IPrimitive, state: S): S = state

  override def onStructured(structured: IStructured, state: S): S = state

  override def onEta(eta: IEtaTerm, state: S): S = state

  override def onError(error: IErrorTerm, state: S): S = state

  override def onPatternCondition(patternCondition: IPatternCondition, state: S): S = state

}


