package termware.util

case class PrettyPrintParams(
    policy: PrettyPrintParams.Policy = PrettyPrintParams.NoPolicy,
    maxColumn: Int = 80,
    ident: Int = 2
)


object PrettyPrintParams {

  sealed trait Policy
  case object NoPolicy extends Policy
  case object C_Like extends Policy

}
