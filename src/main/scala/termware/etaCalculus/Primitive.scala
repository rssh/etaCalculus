package termware.etaCalculus

import termware.util.FastRefOption


trait TCPrimitive[T] extends TCTerm[T]
{

  def  iprimitive(t:T): IPrimitive = CPrimitive(t,this)

  def  primitiveTypeIndex(t:T): Int

  def  value(t:T):T = t

  def  valueEqual(t:T,o:T): Boolean = (t == o)

  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption.empty

  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption.empty

  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption(this)


  override def leftUnifyInSubst(t: T, s: IVarSubstitution, o: ITerm): IVarSubstitution = {
     o.asPrimitive match {
       case FastRefOption.Some(otherPrimitive) =>
         if (primitiveTypeIndex(t) == otherPrimitive.primitiveTypeIndex) {
           if (valueEqual(t,otherPrimitive.carrier.asInstanceOf[T])) {
             s
           } else {
             ContradictionSubstitution(s"mismatch with ${o}",iterm(t),s)
           }
         } else {
           ContradictionSubstitution("mismatch: other primitive index",iterm(t),s)
         }
       case other =>
         ContradictionSubstitution(s"mismatch: non-primitive ${o}",iterm(t),s)
     }
  }

  override def substVars(t: T, s: IVarSubstitution): ITerm = iprimitive(t)


}

object TCPrimitive {
  // TODO: move to more appro

}

trait IPrimitive extends ITerm
{

  override type Carrier = Value

  type Value

  def  tcPrimitive: TCPrimitive[Value]

  override final def tcTerm = tcPrimitive

  def  primitiveTypeIndex: Int = tcPrimitive.primitiveTypeIndex(carrier)


}

case class CPrimitive[T](carrier:T, tcPrimitive: TCPrimitive[T]) extends IPrimitive
{

  type Value = T

}


object PrimitiveTypeIndexes
{

  final val INT = 1

}

object TCIntPrimitive extends TCPrimitive[Int] {

  override def primitiveTypeIndex(t: Int): Int = PrimitiveTypeIndexes.INT

}
