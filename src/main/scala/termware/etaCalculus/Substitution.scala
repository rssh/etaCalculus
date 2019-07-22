package termware.etaCalculus

import termware.NameSubstitution
import termware.util.FastRefOption

trait Substitution[L <: ITerm, R <: ITerm]
{

  thisSubstitution =>

  type Name = L

  type Value = R

  def isEmpty(): Boolean

  def get(x: Name): FastRefOption[ITerm]

  def getOrElse(x: Name, ifNot: ITerm): ITerm =
    get(x).getOrElse(ifNot)

  def containsKey(x:Name): Boolean = get(x).isDefined

  def updated(x:Name,y:Value): Substitution[Name,Value]

  def remove(x:Name): Substitution[Name,Value]

  def empty(): Substitution[Name,Value]

  def keys(): Set[L]


}

object Substitution {

  def empty[L<: ITerm,R <: ITerm]: Substitution[L,R] = MapBasedTermSubstitution.empty

  def namedTerms( pairs: (String,ITerm)* ): NameSubstitution = {
    NameSubstitution.fromMap(pairs.map{
      case (n,v) => (StringName(n), v)
    }.toMap)
  }

}



class MapBasedTermSubstitution[L <: ITerm, R <: ITerm](val value:Map[L,R]) extends Substitution[L,R]
{


  override def isEmpty(): Boolean = value.isEmpty

  override def updated(x: L, y: R): Substitution[L, R] =
    new MapBasedTermSubstitution(value.updated(x,y))

  override def get(x: L): FastRefOption[ITerm] =
    value.get(x) match {
      case Some(v) => FastRefOption(v)
      case None => FastRefOption.empty
    }

  override def remove(x: L): Substitution[L, R] = {
    val nv = value - x
    new MapBasedTermSubstitution[L,R](nv)
  }

  override def empty(): Substitution[L, R] = MapBasedTermSubstitution.empty

  override def keys(): Set[L] = value.keySet

}

object MapBasedTermSubstitution {

  val _empty: Substitution[ITerm,ITerm] = new MapBasedTermSubstitution[ITerm,ITerm](Map.empty)

  def empty[L<:ITerm,R<:ITerm] = _empty.asInstanceOf[MapBasedTermSubstitution[L,R]]

}


