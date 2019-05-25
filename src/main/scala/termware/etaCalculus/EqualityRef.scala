package termware.etaCalculus

class EqualityRef[T<:ITerm](term:T) {

  override def equals(obj: scala.Any): Boolean = {
    if (obj.isInstanceOf[ITerm]) {
       val otherTerm = obj.asInstanceOf[ITerm]
       if (term.isEta()) {
         return term eq otherTerm
       } else {
         term.equals(otherTerm)
       }
    } else false
  }


}
