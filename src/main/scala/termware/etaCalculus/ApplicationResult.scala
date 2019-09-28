package termware.etaCalculus

sealed trait ApplicationResult1 {
  def isSuccess(): Boolean
  def isFailure(): Boolean
}

case class ApplicationSuccess(s: VarSubstitution, term: ITerm) extends ApplicationResult1 {
  override def isSuccess(): Boolean = true
  override def isFailure(): Boolean = false
}

case class ApplicationFailure(s: VarSubstitution, message: String) extends ApplicationResult1 {
  override def isSuccess(): Boolean = false
  override def isFailure(): Boolean = true
}



