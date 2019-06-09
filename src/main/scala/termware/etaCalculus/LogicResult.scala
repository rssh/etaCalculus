package termware.etaCalculus

trait LogicResult {

  def isSuccess(): Boolean

  def isFailure(): Boolean

}



case class LogicProof(inp:Set[LogicProof], expr:ITerm, rule:ITerm, result:ITerm) extends LogicResult {
  override def isSuccess(): Boolean = true

  override def isFailure(): Boolean = false
}

object LogicProof {

  val TRUE = LogicProof(Set(),TCBoolPrimitive.TRUE, StringName("id"), TCBoolPrimitive.TRUE)


}

case class LogicFailure(inp:Set[LogicProof], expr:ITerm, reason:String) extends LogicResult {

  override def isSuccess(): Boolean = false

  override def isFailure(): Boolean = true

}

object LogicFailure {

  def fromMessage(message:String, expr:ITerm) =
    LogicFailure(Set(),expr,message)

}
