package termware.etaCalculus.impl




object ImplementationConfig {

  sealed trait ArrowsAddPolicy

  object ArrrowsAddPolicy {
    case object ArrowsSequence extends ArrowsAddPolicy
    case object MatchingNet  extends ArrowsAddPolicy
  }


  var arrowsAddPolicy: ArrowsAddPolicy = ArrrowsAddPolicy.ArrowsSequence

}
