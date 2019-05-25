package termware.etaCalculus

sealed trait UnificationResult {

  def isSuccess(): Boolean

  def isFailure(): Boolean

  val substitution: Substitution[IVarTerm,ITerm]

}



case class UnificationSuccess(substitution: Substitution[IVarTerm,ITerm]) extends UnificationResult {
  override def isFailure(): Boolean = false
  override def isSuccess(): Boolean = true
}

case class UnificationFailure(
    msg: String,
    frsTerm:ITerm,
    sndTerm: ITerm,
    prevFailure:Option[UnificationFailure],
    substitution: Substitution[IVarTerm,ITerm]) extends UnificationResult {
  override def isFailure(): Boolean = true
  override def isSuccess(): Boolean = false

  def toIErrorTerm(implicit tce: TCErrorTerm[UnificationFailure]) = tce.ierror(this)
}

object UnificationFailure {

  implicit val vartermError = TCUnificationFailureError

}

object TCUnificationFailureError extends TCErrorTerm[UnificationFailure]
{
  override def shortMessage(t: UnificationFailure): String = {
    s"Unification Failure: ${t.frsTerm} and ${t.sndTerm}"
  }

  override def detailedMessage(t: UnificationFailure): String = {
    s"Unification Failure: ${t.frsTerm} and ${t.sndTerm}: ${t.msg} ${t.prevFailure.getOrElse("")}"
  }

  override def traceData(t: UnificationFailure): Any = {
    this
  }

}


