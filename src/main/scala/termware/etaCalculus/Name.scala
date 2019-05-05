package termware.etaCalculus

import termware.util.FastRefOption


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

  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption(this)

  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption.empty

  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption.empty

  override def leftUnifyInSubst(t: T, s: IVarSubstitution, o: ITerm): IVarSubstitution = {
     o.asName match {
       case FastRefOption.Some(otherName) =>
              if (compare(t,otherName) == 0) {
                Substitution.STAR
              } else {
                Substitution.contradiction()
              }
       case FastRefOption.Empty(_) =>
                Substitution.contradiction(s"match failer: ${t} ${o}")
     }
  }

  override def substVars(t:T, s: IVarSubstitution): ITerm = iname(t)



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




