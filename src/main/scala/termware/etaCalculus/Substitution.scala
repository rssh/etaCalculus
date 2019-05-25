package termware.etaCalculus

import termware.util.{IdentityRef}

trait Substitution[L <: ITerm, R <: ITerm]
{

  thisSubstitution =>

  type Name = L

  type Value = R

  def isEmpty(): Boolean

  // TODO: change to FastRefOption
  def get(x: Name): Option[ITerm]

  def getOrElse(x: Name, ifNot: ITerm): ITerm =
    get(x) match {
      case Some(y) => y
      case None => ifNot
    }

  def update(x:Name,y:Value): Substitution[Name,Value]

  def remove(x:Name): Substitution[Name,Value]

  def orElse(s:Substitution[L,R]): Substitution[L,R] = {
    s.foldWhile(this){ case (s,(n,v)) =>
      if (s.get(n).isDefined) {
         s
      } else {
        s.update(n,v)
      }
    }(_ => true)
  }


  def empty(): Substitution[Name,Value]

  def foldWhile[S](s0:S)(f:(S,(Name,Value)) => S)(p:S => Boolean):S

  def mapValues(f:R=>ITerm): Substitution[L,ITerm]

}


case class MapBasedVarSubstitution(values: Map[IdentityRef[IEtaTerm],Map[IName,ITerm]]) extends Substitution[IVarTerm,ITerm]
{

  override type Name = IVarTerm

  override type Value = ITerm

  override def get(x: IVarTerm): Option[ITerm] = {
     values.get(x.ownerRef).flatMap(n => n.get(x.name))
  }

  override def update(x: IVarTerm, y: ITerm): Substitution[IVarTerm, ITerm] = {
    MapBasedVarSubstitution(
      values.get(x.ownerRef) match {
        case None => values.updated(x.ownerRef,Map(x.name -> y))
        case Some(xOwner) => values.updated(x.ownerRef,
          xOwner.updated(x.name,y))
      }
    )
  }

  override def remove(x:IVarTerm): Substitution[IVarTerm,ITerm] = {
    val ownerRef = x.ownerRef
    values.get(ownerRef) match {
      case None => this
      case Some(w) => val newNameMap = w - x.name
        if (newNameMap.isEmpty) {
          MapBasedVarSubstitution(values - ownerRef)
        } else {
          MapBasedVarSubstitution(values.updated(ownerRef, newNameMap))
        }
    }
  }

  override def foldWhile[S](s0: S)(f: (S, (IVarTerm, ITerm)) => S)(p: S => Boolean): S = {
    var quit = false
    var varIterator = values.iterator
    var s = s0
    while(varIterator.hasNext && !quit) {
      val (varRef, names) = varIterator.next()
      val nameIterator = names.iterator
      while(nameIterator.hasNext && !quit) {
        val (name, term) = nameIterator.next()
        quit = ! p(s)
        if (!quit) {
          s = f(s,(PlainVarTerm(varRef.ref, name), term))
        }
      }
    }
    s
  }

  override def empty(): Substitution[IVarTerm, ITerm] = MapBasedVarSubstitution.empty

  override def isEmpty(): Boolean = values.isEmpty

  override def mapValues(f: ITerm => ITerm): Substitution[IVarTerm, ITerm] = {
    new MapBasedVarSubstitution(values.mapValues(_.mapValues(f)))
  }


}

object MapBasedVarSubstitution {

  val empty = MapBasedVarSubstitution(Map.empty)

}



class MapBasedTermSubstitution[L <: ITerm, R <: ITerm](val value:Map[L,R]) extends Substitution[L,R]
{


  override def isEmpty(): Boolean = value.isEmpty

  override def update(x: L, y: R): Substitution[L, R] =
    new MapBasedTermSubstitution(value.updated(x,y))

  override def get(x: L): Option[ITerm] =
    value.get(x)

  override def remove(x: L): Substitution[L, R] = {
    val nv = value - x
    new MapBasedTermSubstitution[L,R](nv)
  }

  override def foldWhile[S](s0: S)(f: (S, (L, R)) => S)(p: S => Boolean): S = {
    var s = s0
    val it = value.iterator
    while(it.hasNext && p(s)) {
      val lr = it.next()
      s = f(s,lr)
    }
    s
  }

  override def empty(): Substitution[L, R] = MapBasedTermSubstitution.empty

  override def mapValues(f: R => ITerm): Substitution[L, ITerm] = {
    new MapBasedTermSubstitution(value.mapValues(f))
  }


}

object MapBasedTermSubstitution {

  val _empty: Substitution[ITerm,ITerm] = new MapBasedTermSubstitution[ITerm,ITerm](Map.empty)

  def empty[L<:ITerm,R<:ITerm] = _empty.asInstanceOf[MapBasedTermSubstitution[L,R]]

}