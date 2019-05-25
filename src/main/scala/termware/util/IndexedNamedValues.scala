package termware.util

trait IndexedNamedValues[N,V <: AnyRef] {

  def isEmpty: Boolean

  def size(): Int

  // inded by name of -1
  def indexByName(name:N): Int

  def nameByIndex(i:Int): N

  def valueByName(n:N): FastRefOption[V]

  def valueByIndex(i:Int): FastRefOption[V]

  def updated(i:Int, v:V): IndexedNamedValues[N,V]

  def modified(i:Int, f: V=>V): IndexedNamedValues[N,V]

  def map(f: V=>V): IndexedNamedValues[N,V]

  def mapWithName(f: (N,V) => V)

  def withoutName(n: N): IndexedNamedValues[N,V]

}

object IndexedNamedValues {

  def apply[N,V <: AnyRef](): IndexedNamedValues[N,V] = ???

}