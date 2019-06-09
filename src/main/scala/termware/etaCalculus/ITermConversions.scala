package termware.etaCalculus

trait ITermConversions {

  implicit def fromInt(x:Int): IPrimitive =
    TCIntPrimitive.iprimitive(x)

  implicit def fromBoolean(x:Boolean): IPrimitive =
    TCBoolPrimitive.iprimitive(x)

}
