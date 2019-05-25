package termware.etaCalculus

trait TermKindMatcher[B] {

  def onName(name: IName): B

  def onVar(varTerm: IVarTerm): B

  def onPrimitive(primitive: IPrimitive): B

  def onStructured(structured: IStructured): B

  def onEta(eta: IEtaTerm): B

  def onError(error: IErrorTerm): B

}
