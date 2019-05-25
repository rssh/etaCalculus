package termware.etaCalculus

import termware.util.{FastRefOption, IdentityRef}

trait   TCSubstitution[T,N <: ITerm, V <: ITerm]
{

  type Carrier = T

  type Name = N

  type Value = V

  def isubstitution(t:T): ISubstitution[N,V]

  def isEmpty(t:T): Boolean

  def update(t:T,x:N,y:V): ISubstitution[N,V]

  def get(t:T, x:N): Option[ITerm]

  def remove(t:T, x:N): ISubstitution[N,V]

  def foldWhile[S](t:T,s0:S)(f:(S,(N,V)) => S)(p: S => Boolean):S

  def mapValues(t:T,f:V=>ITerm):ISubstitution[N,ITerm]

  def empty(): ISubstitution[N,V]

}

trait ISubstitution[L <: ITerm, R <: ITerm]
{

  thisSubstitution =>

  type Carrier

  type Name = L

  type Value = R

  def  carrier: Carrier

  def tcSubstitution: TCSubstitution[Carrier,Name,Value]

  def isStar(): Boolean = tcSubstitution.isEmpty(carrier)

  def isEmpty(): Boolean = tcSubstitution.isEmpty(carrier)

  // TODO: change to FastRefOption
  def get(x: Name): Option[ITerm] =
    tcSubstitution.get(carrier,x)

  def getOrElse(x: Name, ifNot: ITerm): ITerm =
    get(x) match {
      case Some(y) => y
      case None => ifNot
    }

  def update(x:Name,y:Value): ISubstitution[Name,Value] =
    tcSubstitution.update(carrier,x,y)


  def remove[T](x:Name): ISubstitution[Name,Value] =
    tcSubstitution.remove(carrier,x)

  def orElse(s:ISubstitution[L,R]): ISubstitution[L,R] = {
    s.foldWhile(this){ case (s,(n,v)) =>
      if (s.get(n).isDefined) {
         s
      } else {
        s.update(n,v)
      }
    }(_ => true)
  }


  def empty(): ISubstitution[Name,Value] = tcSubstitution.empty()

  def foldWhile[S](s0:S)(f:(S,(Name,Value)) => S)(p:S => Boolean) =
    tcSubstitution.foldWhile(carrier,s0)(f)(p)

  def mapValues(f:R=>ITerm): ISubstitution[L,ITerm] =
    tcSubstitution.mapValues(carrier,f)


}


case class CSubstitution[T,N <: ITerm,V <: ITerm](t:T, tc:TCSubstitution[T,N,V]) extends ISubstitution[N,V]
{
  override type Carrier = T

  override type Name = N

  override type Value = V

  override def carrier: T = t

  override def tcSubstitution: TCSubstitution[Carrier,Name,Value] = tc

}


case class MapBasedVarSubstitution(values: Map[IdentityRef[IEtaTerm],Map[IName,ITerm]]) extends ISubstitution[IVarTerm,ITerm]
{

  override type Carrier = MapBasedVarSubstitution

  override type Name = IVarTerm

  override type Value = ITerm

  override def carrier: MapBasedVarSubstitution = this

  override def tcSubstitution: TCSubstitution[MapBasedVarSubstitution,IVarTerm,ITerm] = TCMapBasedVarSubstitution

}

object MapBasedVarSubstitution {

  val empty = MapBasedVarSubstitution(Map.empty)

}

object TCMapBasedVarSubstitution extends TCSubstitution[MapBasedVarSubstitution,IVarTerm,ITerm]
{

  override type Carrier = MapBasedVarSubstitution

  override def isubstitution(t: Carrier): ISubstitution[IVarTerm,ITerm] = t

  override def get(t: Carrier, x: IVarTerm): Option[ITerm] = {
    t.values.get(x.ownerRef).flatMap(n => n.get(x.name))
  }

  override def update(t: Carrier, x: IVarTerm, y: ITerm): ISubstitution[IVarTerm, ITerm] = {
    MapBasedVarSubstitution(
      t.values.get(x.ownerRef) match {
        case None => t.values.updated(x.ownerRef,Map(x.name -> y))
        case Some(xOwner) => t.values.updated(x.ownerRef,
          xOwner.updated(x.name,y))
      }
    )
  }


  override def remove(t:Carrier, x:IVarTerm): ISubstitution[IVarTerm,ITerm] = {
     val ownerRef = x.ownerRef
     t.values.get(ownerRef) match {
       case None => isubstitution(t)
       case Some(w) => val newNameMap = w - x.name
         if (newNameMap.isEmpty) {
           MapBasedVarSubstitution(t.values - ownerRef)
         } else {
           MapBasedVarSubstitution(t.values.updated(ownerRef, newNameMap))
         }
     }
  }

  override def foldWhile[S](t: Carrier, s0: S)(f: (S, (IVarTerm, ITerm)) => S)(p: S => Boolean): S = {
    var quit = false
    var varIterator = t.values.iterator
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

  override def empty(): ISubstitution[IVarTerm, ITerm] = MapBasedVarSubstitution.empty

  override def isEmpty(t: Carrier): Boolean = t.values.isEmpty

  override def mapValues(t: Carrier, f: ITerm => ITerm): ISubstitution[IVarTerm, ITerm] = {
    new MapBasedVarSubstitution(t.values.mapValues(_.mapValues(f)))
  }

}



class TCMapBasedTermSubstitution[L <: ITerm, R <: ITerm] extends TCSubstitution[MapBasedTermSubstitution[L,R],L,R]
{
  override def isubstitution(t: MapBasedTermSubstitution[L, R]): ISubstitution[L, R] = t

  override def isEmpty(t: MapBasedTermSubstitution[L, R]): Boolean = t.value.isEmpty

  override def update(t: MapBasedTermSubstitution[L, R], x: L, y: R): ISubstitution[L, R] =
           new MapBasedTermSubstitution(t.value.updated(x,y))

  override def get(t: MapBasedTermSubstitution[L, R], x: L): Option[ITerm] =
    t.value.get(x)

  override def remove(t: MapBasedTermSubstitution[L, R], x: L): ISubstitution[L, R] = {
    val nv = t.value - x
    isubstitution(new MapBasedTermSubstitution[L,R](nv))
  }

  override def foldWhile[S](t: MapBasedTermSubstitution[L, R], s0: S)(f: (S, (L, R)) => S)(p: S => Boolean): S = {
    var s = s0
    val it = t.value.iterator
    while(it.hasNext && p(s)) {
      val lr = it.next()
      s = f(s,lr)
    }
    s
  }

  override def empty(): ISubstitution[L, R] = MapBasedTermSubstitution.empty

  override def mapValues(t: MapBasedTermSubstitution[L, R], f: R => ITerm): ISubstitution[L, ITerm] = ???
}



class MapBasedTermSubstitution[L <: ITerm, R <: ITerm](val value:Map[L,R]) extends ISubstitution[L,R]
{

   type Carrier = MapBasedTermSubstitution[L,R]

  override def carrier: MapBasedTermSubstitution[L, R] = this

  override val tcSubstitution: TCSubstitution[MapBasedTermSubstitution[L, R], L, R] =
    new TCMapBasedTermSubstitution[L,R]


}

object MapBasedTermSubstitution {

  val _empty: ISubstitution[ITerm,ITerm] = new MapBasedTermSubstitution[ITerm,ITerm](Map.empty)

  def empty[L<:ITerm,R<:ITerm] = _empty.asInstanceOf[MapBasedTermSubstitution[L,R]]

}