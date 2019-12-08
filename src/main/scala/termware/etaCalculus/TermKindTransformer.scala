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

/**
  * Delegated transformer.  Usage - creating subclass, where one of methods is overrided.
  * @param origin - origin transformer, on which all calls delegated by default.
  * @tparam B - transformer result
  */
class DelegatedTermKindTransformer[B](val origin: TermKindTransformer[B]) extends TermKindTransformer[B] {

  override def onName(name: IName, vo: Map[IEtaTerm, IEtaTerm]): B =
    origin.onName(name,vo)

  override def onVar(varTerm: IVarTerm, vo: Map[IEtaTerm, IEtaTerm]): B =
    origin.onVar(varTerm,vo)

  override def onPrimitive(primitive: IPrimitive, vo: Map[IEtaTerm, IEtaTerm]): B =
    origin.onPrimitive(primitive,vo)

  override def onStructured(structured: IStructured, vo: Map[IEtaTerm, IEtaTerm]): B =
    origin.onStructured(structured,vo)

  override def onEta(eta: IEtaTerm, vo: Map[IEtaTerm, IEtaTerm]): B =
    origin.onEta(eta,vo)

  override def onError(error: IErrorTerm, vo: Map[IEtaTerm, IEtaTerm]): B =
    origin.onError(error,vo)

  override def onPatternCondition(patternCondition: IPatternCondition, vo: Map[IEtaTerm, IEtaTerm]): B =
    origin.onPatternCondition(patternCondition, vo)

  override def onArrows(arrow: IArrows, vo: Map[IEtaTerm, IEtaTerm]): B =
    origin.onArrows(arrow, vo)
}