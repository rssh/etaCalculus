package termware.etaCalculus

object ILeftUnificable {

  val STAR = AnyLeftUnificable

  def putOrMerge(s: ISubstitution[IVarTerm,ITerm], k: IVarTerm, v: ITerm): UnificationResult = {
    s.get(k) match {
      case None => UnificationSuccess(s.update(k,v))
      case Some(v1) =>
        v1.leftUnifyInSubst(s,v)
    }
  }


}

trait TCLeftUnificable[T] {

  def leftUnifyInSubst(t: T, s:ISubstitution[IVarTerm,ITerm], o: ITerm): UnificationResult



}

trait ILeftUnificable {

  def leftUnifyInSubst(s:ISubstitution[IVarTerm,ITerm], o: ITerm): UnificationResult


}


object AnyLeftUnificable extends ILeftUnificable
{
  override def leftUnifyInSubst(s: ISubstitution[IVarTerm,ITerm], o: ITerm): UnificationResult = UnificationSuccess(s)
}

