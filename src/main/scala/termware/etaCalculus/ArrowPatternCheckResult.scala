package termware.etaCalculus

sealed trait ArrowPatternCheckResult {

   def isSuccess(): Boolean

   def isFailure(): Boolean

}

case class ArrowPatternCheckSuccess(
    substitution: VarSubstitution,
    selectedRight: ITerm,
    originPattern: ITerm)  extends ArrowPatternCheckResult {

  override final def isSuccess(): Boolean = true

  override final def isFailure(): Boolean = false

}

case class ArrowPatternCheckFailure(
    originPattern: ITerm,
    msg: String
) extends ArrowPatternCheckResult {

  override final def isSuccess(): Boolean = false

  override final def isFailure(): Boolean = true

}