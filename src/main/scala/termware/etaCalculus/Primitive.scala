package termware.etaCalculus

import termware.util.FastRefOption

import scala.reflect.ClassTag


trait TCPrimitive[T] extends TCTerm[T]
{

  def  iprimitive(t:T): IPrimitive = CPrimitive(t,this)

  def  primitiveTypeIndex(t:T): Int

  def  value(t:T):T = t

  def  valueEqual(t:T,o:T): Boolean = (t == o)

  override def hasPatterns(t: T): Boolean = false
  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption.empty
  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption.empty
  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption(this)
  override def tcError(t: T): FastRefOption[TCErrorTerm[T]] = FastRefOption.empty
  override def tcStructured(t: T): FastRefOption[TCStructured[T]] = FastRefOption.empty
  override def tcEta(t: T): FastRefOption[TCEtaTerm[T]] = FastRefOption.empty
  override def tcPatternCondition(t: T): FastRefOption[TCPatternCondition[T]] = FastRefOption.empty

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

  override def hasPatternsRec(t: T, trace: Map[IVarTerm, Boolean]): Boolean = false

  override def termEqNoRef(t: T, otherTerm: ITerm): Boolean = {
    otherTerm match {
      case IPrimitive(otherPrimitive) =>
        if (otherPrimitive.primitiveTypeIndex == primitiveTypeIndex(t)) {
          valueEqual(t,otherPrimitive.asInstanceOf[IPrimitive.Aux[T]].value)
        } else false
      case IEtaTerm(eta) =>
        termEqNoRef(t,eta.baseTerm())
    }
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

  def value: Value = carrier

  def  primitiveTypeIndex: Int = tcPrimitive.primitiveTypeIndex(carrier)



  override def transform[B](matcher: TermKindTransformer[B], vo:Map[IEtaTerm,IEtaTerm]): B =
     matcher.onPrimitive(this,vo)


}

object IPrimitive {

  def unapply(arg: ITerm): FastRefOption[IPrimitive] = arg.asPrimitive()

  type Aux[T] = IPrimitive{ type Value = T }

}

case class CPrimitive[T](carrier:T, tcPrimitive: TCPrimitive[T]) extends IPrimitive
{

  type Value = T

}


object PrimitiveTypeIndexes
{

  final val INT = 1
  final val BOOLEAN = 2

}

object IntPrimitive {

  def typeIndex = TCIntPrimitive.primitiveTypeIndex(0)

  def apply(arg:Int): IPrimitive.Aux[Int] = {
    CPrimitive[Int](arg, TCIntPrimitive)
  }

  def unapply(arg: ITerm): Option[Int] = {
    Term.unapply(arg) match {
      case FastRefOption.Some(p) => Some(p.value)
      case FastRefOption.Empty() => None
    }
  }

  object Term {

    def unapply(arg: ITerm): FastRefOption[IPrimitive.Aux[Int]] = {
      if (arg.isPrimitive()) {
        val p = arg.asPrimitive().get()
        if (p.primitiveTypeIndex == IntPrimitive.typeIndex) {
          new FastRefOption[IPrimitive.Aux[Int]](p.asInstanceOf[IPrimitive.Aux[Int]])
        } else FastRefOption.empty
      } else FastRefOption.empty
    }

  }

}

object TCIntPrimitive extends TCPrimitive[Int] {

  override def primitiveTypeIndex(t: Int): Int = PrimitiveTypeIndexes.INT

}

object TCBoolPrimitive extends TCPrimitive[Boolean] {

  override def primitiveTypeIndex(t: Boolean): Int = PrimitiveTypeIndexes.BOOLEAN

  val TRUE = CPrimitive[Boolean](true,TCBoolPrimitive)
  val FALSE = CPrimitive[Boolean](false,TCBoolPrimitive)

}

object BoolPrimitive {

  final def primitiveTypeIndex = PrimitiveTypeIndexes.BOOLEAN

  object Term {
    def unapply(arg: ITerm): FastRefOption[IPrimitive.Aux[Boolean]] = {
      if (arg.isPrimitive()) {
        val p = arg.asPrimitive().get()
        if (p.primitiveTypeIndex == BoolPrimitive.primitiveTypeIndex) {
          new FastRefOption(p.asInstanceOf[IPrimitive.Aux[Boolean]])
        } else {
          FastRefOption.empty
        }
      } else FastRefOption.empty
    }
  }

  def apply(arg:Boolean) = CPrimitive[Boolean](arg,TCBoolPrimitive)

  def unapply(arg: ITerm): Option[Boolean] = {
    arg match {
      case Term(p) => Some(p.value)
      case _ => None
    }
  }

  val TRUE = TCBoolPrimitive.TRUE
  val FALSE = TCBoolPrimitive.FALSE

}