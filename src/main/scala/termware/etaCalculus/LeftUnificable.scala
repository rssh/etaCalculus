package termware.etaCalculus

import termware.util.FastRefOption

object ILeftUnificable {

  val STAR = AnyLeftUnificable

  def putOrMerge(s: VarSubstitution, k: IVarTerm, v: ITerm): UnificationResult = {
    s.get(k) match {
      case FastRefOption.Empty() => UnificationSuccess(s.updated(k,v))
      case FastRefOption.Some(v1) =>
        v1.leftUnifyInSubst(s,v)
    }
  }


}

trait TCLeftUnificable[T] {

  def leftUnifyInSubst(t: T, s: VarSubstitution, o: ITerm): UnificationResult



}

trait ILeftUnificable {

  def leftUnifyInSubst(s: VarSubstitution, o: ITerm): UnificationResult


}


object AnyLeftUnificable extends ILeftUnificable
{
  override def leftUnifyInSubst(s: VarSubstitution, o: ITerm): UnificationResult = UnificationSuccess(s)
}

