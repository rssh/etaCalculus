package termware.etaCalculus

import termware.util.FastRefOption

import scala.reflect.ClassTag


trait TCPrimitive[T] extends TCTerm[T]
{

  def  iprimitive(t:T): IPrimitive = CPrimitive(t,this)

  def  primitiveTypeIndex(t:T): Int

  def  value(t:T):T = t

  def  valueEqual(t:T,o:T): Boolean = (t == o)

  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption.empty

  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption.empty

  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption(this)

  override def tcError(t: T): FastRefOption[TCErrorTerm[T]] = FastRefOption.empty

  override def tcStructured(t: T): FastRefOption[TCStructured[T]] = FastRefOption.empty

  override def tcEta(t: T): FastRefOption[TCEtaTerm[T]] = FastRefOption.empty

  override def leftUnifyInSubst(t: T, s: Substitution[IVarTerm,ITerm], o: ITerm): UnificationResult = {
     o.asPrimitive match {
       case FastRefOption.Some(otherPrimitive) =>
         if (primitiveTypeIndex(t) == otherPrimitive.primitiveTypeIndex) {
           if (valueEqual(t,otherPrimitive.carrier.asInstanceOf[T])) {
             UnificationSuccess(s)
           } else {
             UnificationFailure(s"value mismatch",iterm(t),o,None,s)
           }
         } else {
           UnificationFailure(s"other primitive index",iterm(t),o,None,s)
         }
       case other =>
         UnificationFailure(s"unification value with non-value",iterm(t),o,None,s)
     }
  }

  override def substVars(t: T, s: Substitution[IVarTerm,ITerm], vo: Map[IEtaTerm,IEtaTerm]): ITerm = iprimitive(t)

  override def mapVars(t: T, f: IVarTerm => ITerm, vo: Map[IEtaTerm,IEtaTerm]): ITerm = iprimitive(t)

  override def subst[N <: ITerm, V <: ITerm](t: T, s: Substitution[N, V], vo: Map[IEtaTerm, IEtaTerm])(implicit nTag:ClassTag[N]): ITerm = {
    val pt = iprimitive(t)
    pt match {
      case nTag(npt) => s.get(npt).getOrElse(pt)
      case _ => pt
    }
  }

  override def map(t: T, f: ITerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    iprimitive(t)
  }

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

  override def transform[B](matcher: TermKindTransformer[B], vo:Map[IEtaTerm,IEtaTerm]): B =
     matcher.onPrimitive(this,vo)

}

object IPrimitive {

  def unapply(arg: ITerm): FastRefOption[IPrimitive] = arg.asPrimitive()

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
