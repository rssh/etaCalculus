package termware.etaCalculus

import termware.util.FastRefOption

/**
  * Substitution of variables.
  */
trait VarSubstitution {

  def isEmpty(): Boolean

  def get(x:IVarTerm): FastRefOption[ITerm]

  def getOrElse(x:IVarTerm, ifNot: ITerm): ITerm

  def updated(x:IVarTerm, value:ITerm): VarSubstitution

  def remove(x: IVarTerm): VarSubstitution

  def keys(): Set[IVarTerm]

  def foldWhile[S](s0: S)(p: S => Boolean)(f: (S, IVarTerm, ITerm) => S): S

  def map[B](f: (IVarTerm, ITerm) => B):Iterable[B]

}


object VarSubstitution {

  def empty() = MapBasedVarSubstitution.empty

  def apply(pairs: (IVarTerm,ITerm)* ): VarSubstitution = {
    val s0: VarSubstitution = empty()
    pairs.foldLeft(s0) { (s, e) =>
      s.updated(e._1, e._2)
    }
  }

}


case class MapBasedVarSubstitution(values: Map[IEtaTerm,Map[IName,ITerm]]) extends VarSubstitution
{

  //override type Name = IVarTerm

  //override type Value = ITerm

  override def get(x: IVarTerm): FastRefOption[ITerm] = {
    values.get(x.owner).flatMap(n => n.get(x.name)) match {
      case Some(value) => FastRefOption(value)
      case None => FastRefOption.empty
    }
  }

  override def getOrElse(x: IVarTerm, ifNot: ITerm): ITerm = {
    values.getOrElse(x.owner,Map.empty).getOrElse(x.name,ifNot)
  }

  override def updated(x: IVarTerm, y: ITerm): VarSubstitution = {
    MapBasedVarSubstitution(
      values.get(x.owner) match {
        case None => values.updated(x.owner,Map(x.name -> y))
        case Some(xOwner) => values.updated(x.owner,
          xOwner.updated(x.name,y))
      }
    )
  }


  override def remove(x:IVarTerm): VarSubstitution = {
    val owner = x.owner
    values.get(owner) match {
      case None => this
      case Some(w) => val newNameMap = w - x.name
        if (newNameMap.isEmpty) {
          MapBasedVarSubstitution(values - owner)
        } else {
          MapBasedVarSubstitution(values.updated(owner, newNameMap))
        }
    }
  }

  override def foldWhile[S](s0: S)(p: S => Boolean)(f: (S, IVarTerm, ITerm) => S): S = {
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
          s = f(s,PlainVarTerm(varRef, name), term)
        }
      }
    }
    s
  }

  def empty(): VarSubstitution = MapBasedVarSubstitution.empty

  override def isEmpty(): Boolean = values.isEmpty

  override def keys(): Set[IVarTerm] = {
    { def retrieveVar(eta: IEtaTerm, names: Map[IName,ITerm]): IterableOnce[IVarTerm] = {
      names.keySet.map(x => new PlainVarTerm(eta,x))
    }
      values.flatMap{case (ref,names) => retrieveVar(ref,names)}.toSet
    }
  }

  override def map[B](f: (IVarTerm, ITerm) => B):Iterable[B] = {
    values.flatMap[B]{ case (eta,namesMap) =>
        namesMap.map[B]{ case (name,value) =>
          f(PlainVarTerm(eta,name),value)
        }
    }
  }

  // TODO: implement lazy substitution
  def mapVars(f: ITerm => ITerm): VarSubstitution = {
    new MapBasedVarSubstitution(
      values.view.mapValues(_.view.mapValues(f).toMap).toMap
    )
  }


}

object MapBasedVarSubstitution {

  val empty = MapBasedVarSubstitution(Map.empty)

}

