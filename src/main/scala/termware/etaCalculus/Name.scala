package termware.etaCalculus

import termware.etaCalculus.IName.Aux
import termware.util.FastRefOption

import scala.reflect.ClassTag


trait TCName[T] extends TCTerm[T]
{

  type V

  /**
    * nameTypeIndex. same typeIndex => same V
    * @param t
    * @return
    */
  def  nameTypeIndex(t:T): Int

  def  valueHash(t:T): Int

  def  valueString(t:T): String

  def  value(t:T): V

  def  iname(t:T): IName

  def  compare(t:T, otherName: IName): Int = {
     val c = nameTypeIndex(t) - otherName.nameTypeIndex
     if (c != 0)
       c
     else
       compareSame(t,otherName.value.asInstanceOf[V])
  }

  def  compareSame(t:T, other:V): Int

  override def hasPatternsRec(t: T, trace: Map[IVarTerm,Boolean]): Boolean = false

  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption(this)
  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption.empty
  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption.empty
  override def tcError(t: T): FastRefOption[TCErrorTerm[T]] = FastRefOption.empty
  override def tcEta(t: T): FastRefOption[TCEtaTerm[T]] = FastRefOption.empty
  override def tcStructured(t: T): FastRefOption[TCStructured[T]] = FastRefOption.empty
  override def tcPatternCondition(t: T): FastRefOption[TCPatternCondition[T]] = FastRefOption.empty
  override def tcArrows(t: T): FastRefOption[TCArrows[T]] = FastRefOption.empty

  override def leftUnifyInSubst(t: T, s: VarSubstitution, o: ITerm): UnificationResult = {
     o.asName match {
       case FastRefOption.Some(otherName) =>
              if (compare(t,otherName) == 0) {
                UnificationSuccess(s)
              } else {
                UnificationFailure("name mismatch",iname(t),otherName,None,s)
              }
       case FastRefOption.Empty() =>
                UnificationFailure("not name in right part",iname(t),o,None,s)
     }
  }

  override def termEqNoRef(t: T, otherTerm: ITerm): Boolean = {
    otherTerm.asName() match {
      case FastRefOption.Some(otherName) => (compare(t,otherName)==0)
      case FastRefOption.Empty() =>
        otherTerm.asEta() match {
          case FastRefOption.Some(otherEta) => termEqNoRef(t,otherEta.baseTerm())
          case FastRefOption.Empty() => false
        }
    }
  }

  override def substVars(t:T, s: VarSubstitution, vo:Map[IEtaTerm,IEtaTerm]): ITerm = iname(t)
  override def mapVars(t: T, f: IVarTerm => ITerm, vo: Map[IEtaTerm,IEtaTerm]): ITerm = iname(t)

  /*
  override def subst[N <: ITerm, V <: ITerm](t: T, s: Substitution[N, V], vo: Map[IEtaTerm, IEtaTerm])(implicit nTag: ClassTag[N]): ITerm = {
    val name = iname(t)
    name match {
      case nTag(x) => s.get(x).map(_.kindTransform(VarOwnerChangeTransformer,vo)).getOrElse(name)
      case other => name
    }

  }
   */

  //override def map(t: T, f: ITerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
  //  iname(t)
  //}




}

object TCName {


  implicit object  FromString extends TCName[String] {

    type V = String

    final val NAME_TYPE_INDEX = 1

    override def iname(t: String): IName = StringName(t)

    override def nameTypeIndex(t: String): Int = NAME_TYPE_INDEX

    override def valueHash(t: String): Int = t.hashCode

    override def valueString(t: String): String = t

    override def value(t: String): String = t

    override def compareSame(t: String, other: String): Int = t.compare(other)

  }


}

trait IName extends ITerm
{

  type Carrier
  type Value   // = tcName.V

  def  tcName: TCName[Carrier]

  override final def tcTerm = tcName

  def  nameTypeIndex: Int = tcName.nameTypeIndex(carrier)

  def  valueHash: Int = tcName.valueHash(carrier)
  def  valueString: String = tcName.valueString(carrier)

  def  value: Value = tcName.value(carrier).asInstanceOf[Value]

  override def kindTransform[B](matcher: TermKindTransformer[B], vo: Map[IEtaTerm,IEtaTerm]): B =
    matcher.onName(this,vo)

  override def kindFold[S](s0: S)(folder: TermKindFolder[S]): S = {
    folder.onName(this,s0)
  }

  // DSL
  def apply(x:ITerm*): ITerm =
    IStructured.nameFreeIndexed(this,x: _*)


}

object IName {

  def unapply(arg: ITerm): FastRefOption[IName] = arg.asName()

  type Aux[X] = IName { type Carrier = X }

}



case class StringName(override val value: String) extends IName
{

  type Carrier = StringName

  type Value = String

  override def carrier: StringName = this

  override def tcName: TCName[StringName] = StringName.TC

  override def valueHash: Int = value.hashCode

}

object StringName {

  object TC extends TCName[StringName] {

    type V = String

    override def nameTypeIndex(t: StringName): Int = TCName.FromString.NAME_TYPE_INDEX

    override def valueHash(t: StringName): Int = t.value.hashCode

    override def valueString(t: StringName): String = t.value

    override def value(t: StringName): String = t.value

    override def iname(t: StringName): IName = t

    override def compareSame(t: StringName, other: String): Int = t.value.compare(other)
  }

}

case class IntName(override val value: Int) extends IName {

  type Carrier = IntName

  type Value = Int

  override def carrier: IntName = this
  override def tcName: TCName[IntName] = IntName.TC

 // override def map(f: ITerm => ITerm, vo:Map[IEtaTerm,IEtaTerm]) = this

}

object IntName {

  object TC extends TCName[IntName] {

    type V = Int

    /**
      * nameTypeIndex. same typeIndex => same V
      *
      * @param t
      * @return
      */
    override def nameTypeIndex(t: IntName): Int = 2
    override def valueHash(t: IntName): Int = t.value
    override def valueString(t: IntName): String = t.value.toString
    override def value(t: IntName): Int = t.value
    override def iname(t: IntName): IName = t
    override def compareSame(t: IntName, other: Int): Int = t.value - other




  }

}


