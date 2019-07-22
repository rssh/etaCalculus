package termware.etaCalculus.compact

import termware.etaCalculus.{ITerm, IVarTerm, Substitution, UnificationResult}


/**
  * TermWare2-like compact
  */
trait IFacts {


  def get(t:ITerm, s: Substitution[IVarTerm,ITerm]): UnificationResult


  def set(t:ITerm, s: Substitution[IVarTerm,ITerm]): Unit


}
