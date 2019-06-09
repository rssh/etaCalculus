package termware.etaCalculus


object EtaSubstitution extends Substitution[IVarTerm,ITerm] {

  //TODO: are
  override def isEmpty(): Boolean = false

  override def get(x: EtaSubstitution.Name): Option[ITerm] = ???

  override def update(x: EtaSubstitution.Name, y: EtaSubstitution.Value): Substitution[EtaSubstitution.Name, EtaSubstitution.Value] = ???

  override def remove(x: EtaSubstitution.Name): Substitution[EtaSubstitution.Name, EtaSubstitution.Value] = ???

  override def empty(): Substitution[EtaSubstitution.Name, EtaSubstitution.Value] = ???

  override def foldWhile[S](s0: S)(f: (S, (EtaSubstitution.Name, EtaSubstitution.Value)) => S)(p: S => Boolean): S = ???

  override def keys(): Set[IVarTerm] = ???

  override def mapValues(f: ITerm => ITerm): Substitution[IVarTerm, ITerm] = ???
}
