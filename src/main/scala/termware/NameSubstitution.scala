package termware

import termware.etaCalculus.{IName, ITerm, Substitution}
import termware.util.FastRefOption

trait NameSubstitution extends  {

  def isEmpty(): Boolean

  def keys(): Set[IName]

  def get(n:IName): FastRefOption[ITerm]

  def updated(n:IName, v:ITerm): NameSubstitution

  def without(n:IName): NameSubstitution = remove(n)

  def remove(n:IName): NameSubstitution

  def contains(n:IName): Boolean

  def mapValues(f: ITerm => ITerm): NameSubstitution

  def filter(f: (IName, ITerm) => Boolean): NameSubstitution

  def foldLeft[S](s0:S)(f:(S,IName,ITerm) => S):S =
    foldWhile[S](s0)(_ => true)(f)

  def foldWhile[S](s0:S)(p:S=>Boolean)(f: (S,IName,ITerm) => S):S

  def foreach(f: (IName,ITerm) => Unit ) =
    foldLeft(()){ (s,ne,nv) => f(ne,nv) }

}

object NameSubstitution
{

  def apply(pairs:(IName,ITerm)*): NameSubstitution = new MapBasedNameSubstitution(Map(pairs: _*))

  def fromMap(map:Map[IName,ITerm]): NameSubstitution = new MapBasedNameSubstitution(map)

}

class MapBasedNameSubstitution(values: Map[IName,ITerm]) extends NameSubstitution {

  override def isEmpty(): Boolean = values.isEmpty

  override def keys(): Set[IName] = values.keySet

  override def get(n: IName): FastRefOption[ITerm] = {
    values.get(n) match {
      case Some(v) => FastRefOption(v)
      case None => FastRefOption.empty
    }
  }

  override def updated(n: IName, v: ITerm): NameSubstitution = {
    new MapBasedNameSubstitution(values.updated(n,v))
  }

  override def remove(n: IName): NameSubstitution = {
    new MapBasedNameSubstitution(values - n)
  }

  override def contains(n: IName): Boolean = {
    values.contains(n)
  }

  override def mapValues(f: ITerm => ITerm): NameSubstitution = {
    new MapBasedNameSubstitution(values.view.mapValues(f).toMap)
  }

  override def filter(f: (IName,ITerm) => Boolean): NameSubstitution = {
    new MapBasedNameSubstitution(values.filter{case (n,v) => f(n,v)})
  }

  override def foldWhile[S](s0:S)(p:S=>Boolean)(f: (S,IName,ITerm) => S):S = {
    var s = s0
    val it = values.iterator;
    while(p(s) && it.hasNext) {
      val c = it.next();
      s = f(s,c._1,c._2)
    }
    s
  }


}



