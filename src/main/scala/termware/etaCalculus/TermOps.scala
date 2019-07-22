package termware.etaCalculus

import termware.util.FastRefOption

/**
  * Quasy-universal term operations.
  */

object TermOps {

  def subterm(t:ITerm, i:Int): FastRefOption[ITerm] = {
    t match {
      case IEtaTerm(t1) => subterm(t1.baseTerm(),i)
      case IStructured(t1) => t1.subterm(i)
      case _ => FastRefOption.empty
    }
  }

  def subterm(t:ITerm, n:IName): FastRefOption[ITerm] = {
    t match {
      case IEtaTerm(t1) => subterm(t1.baseTerm(),n)
      case IStructured(t1) => t1.subterm(n)
      case _ => FastRefOption.empty
    }
  }

  def arity(t: ITerm): Int = {
    t match {
      case IEtaTerm(t1) => arity(t1.baseTerm())
      case IStructured(t1) => t1.arity()
      case _ => 0
    }
  }

}
