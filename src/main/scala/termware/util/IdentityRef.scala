package termware.util

class IdentityRef[T <: AnyRef](val ref:T) extends AnyRef {

  override def hashCode(): Int = {
     ref.hashCode()
  }

  override def equals(obj: scala.Any): Boolean = {
     if (obj.isInstanceOf[IdentityRef[_]]) {
       val oref = obj.asInstanceOf[IdentityRef[_]]
       ref eq oref
     } else {
       false
     }
  }

}
