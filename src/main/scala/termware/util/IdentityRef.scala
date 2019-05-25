package termware.util

class IdentityRef[+T <: AnyRef](val ref:T) extends AnyRef {

  override def hashCode(): Int = {
     ref.hashCode()
  }

  override def equals(obj: scala.Any): Boolean = {
     if (obj.isInstanceOf[IdentityRef[_ <: AnyRef]]) {
       val oref = obj.asInstanceOf[IdentityRef[_ <: AnyRef]]
       ref eq oref
     } else {
       false
     }
  }

}

object IdentityRef {

  def apply[T <: AnyRef](ref: T): IdentityRef[T] = new IdentityRef[T](ref)

}