package termware.etaCalculus

trait ITermConversions {

  implicit def fromInt(x:Int): IPrimitive =
    TCIntPrimitive.iprimitive(x)


}
