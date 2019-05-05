package termware.util

/**
  * Value-like option for use in pattern matching.
  * @param value
  * @tparam T
  */
class FastRefOption[+T <: AnyRef](val value:T) extends AnyVal {

  def isEmpty: Boolean = (value eq null)
  
  def isDefined: Boolean = !(value eq null)

  def get(): T = value

  def getOrElse[S >: T](df: =>S): S = {
    if (isEmpty) {
      df
    } else value
  }

  def map[S <: AnyRef](f: T => S): FastRefOption[S] =
    if (isEmpty) FastRefOption.empty else FastRefOption(f(value)) 
  
  def flatMap[S <: AnyRef](f: T=> FastRefOption[S]): FastRefOption[S] = {
    if (isEmpty) FastRefOption.empty
    else f(value)
  }
  
}


object FastRefOption
{


  @inline
  final def apply[T <: AnyRef](x:T) = new FastRefOption[T](x)

  def empty[S <: AnyRef]: FastRefOption[S] = _empty.asInstanceOf[FastRefOption[S]]

  val _empty: FastRefOption[Null] = new FastRefOption(null)

  object Some {

    def unapply[T <: AnyRef](arg: FastRefOption[T]): FastRefOption[T] = {
      arg
    }

    
  }

  object Empty {
    def unapply[T <: AnyRef](arg: FastRefOption[T]): FastRefOption[Empty.type] = {
       if (arg.isEmpty) {
         FastRefOption(Empty)
       } else {
         FastRefOption.empty
       }
    }
  }
  
}


