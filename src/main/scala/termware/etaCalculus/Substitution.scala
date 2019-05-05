package termware.etaCalculus

import termware.util.IdentityRef


trait IVarSubstitution
{

   type Carrier

   def  carrier: Carrier

   def tcVarSubstitution: TCVarSubstitution[Carrier]

   def isContradiction(): Boolean = tcVarSubstitution.isContradiction(carrier)

   def isStar(): Boolean = tcVarSubstitution.isStar(carrier)

  // TODO: change to FastRefOption
   def get[T](x: T)(implicit tcx: TCVarTerm[T]): Option[ITerm] =
     tcVarSubstitution.get(carrier,x)(tcx)

   def get(x: IVarTerm): Option[ITerm] =
     get(x.carrier)(x.tcVarTerm)

   def getOrElse(x: IVarTerm, ifNot: ITerm): ITerm =
     get(x) match {
       case Some(y) => y
       case None => ifNot
     }

   def put[T,S](x:T,y:S)(implicit tcx:TCVarTerm[T], tcy: TCTerm[S]): IVarSubstitution =
     tcVarSubstitution.put(carrier,x,y)

  def put(x:IVarTerm,y:ITerm): IVarSubstitution =
    tcVarSubstitution.put(carrier,x.carrier,y.carrier)(x.tcVarTerm,y.tcTerm)

   def remove[T](x:T)(implicit tcx:TCVarTerm[T]): IVarSubstitution =
     tcVarSubstitution.remove(carrier,x)

   def empty(): IVarSubstitution = STARSubstitution

   def foldWhile[S](s0:S)(f:(S,(IVarTerm,ITerm)) => S)(p:S => Boolean) =
     tcVarSubstitution.foldWhile(carrier,s0)(f)(p)

}

trait   TCVarSubstitution[T]
{

  type Carrier = T

  def isubstitution(t:T): IVarSubstitution

  def isContradiction(t:T):Boolean

  def isStar(t:T): Boolean

  def put[X,Y](t:T,x:X,y:Y)(implicit tcx:TCVarTerm[X], tcy: TCTerm[Y]): IVarSubstitution

  def get[X](t:T, x:X)(implicit tcx:TCVarTerm[X]): Option[ITerm]

  def remove[X](t:T, x:X)(implicit tcx:TCVarTerm[X]): IVarSubstitution

  def merge[S](t:T,s:S)(implicit tcs: TCVarSubstitution[S]): IVarSubstitution = {
    merge(t,tcs.isubstitution(s))
  }

  def merge(t:T,s:IVarSubstitution): IVarSubstitution = {
    foldWhile(t,s){(s,e) =>
      s.put(e._1,e._2)
    }{ ! _.isContradiction() }
  }

  def foldWhile[S](t:T,s0:S)(f:(S,(IVarTerm,ITerm)) => S)(p: S => Boolean):S

  def empty(): IVarSubstitution = STARSubstitution

}

case class CVarSubstitution[T](t:T, tc:TCVarSubstitution[T]) extends IVarSubstitution
{
  override type Carrier = T

  override def carrier: T = t

  override def tcVarSubstitution: TCVarSubstitution[T] = tc

}


case class  VarSubstitutionInMap(owner:IEtaTerm, nameMap: Map[IName,ITerm]) extends IVarSubstitution
{
  override type Carrier = VarSubstitutionInMap

  override def carrier: VarSubstitutionInMap = this

  override def tcVarSubstitution: TCVarSubstitution[VarSubstitutionInMap] = TCSubstitutionInMap

}

object TCSubstitutionInMap extends TCVarSubstitution[VarSubstitutionInMap]{

  override def isContradiction(t: VarSubstitutionInMap): Boolean = false

  override def isStar(t: VarSubstitutionInMap): Boolean = t.nameMap.isEmpty

  override def get[X](t: VarSubstitutionInMap, x: X)(implicit tcx: TCVarTerm[X]): Option[ITerm] = {
    if (tcx.owner(x) eq t.owner) {
      t.nameMap.get(tcx.name(x))
    } else None
  }

  override def put[X, Y](t: VarSubstitutionInMap, x: X, y: Y)(implicit tcx: TCVarTerm[X], tcy: TCTerm[Y]): IVarSubstitution = {
    if (t.owner eq tcx.owner(x)) {
      t.nameMap.get(tcx.name(x)) match {
        case None => VarSubstitutionInMap(t.owner, t.nameMap.updated(tcx.name(x),tcy.iterm(y)))
        case Some(otherDefinition) =>
            tcy.leftUnifyInSubst(y,t,otherDefinition.carrier)(this,otherDefinition.tcTerm)
      }
    } else {
      ContradictionSubstitution(s"Siubstitution owner should be ${t.owner} we have ${tcx.owner(x)} for ${tcx.name(x)}")
    }
  }

  override def remove[X](t: VarSubstitutionInMap, x: X)(implicit tcx: TCVarTerm[X]): IVarSubstitution = {
     if (t.owner eq tcx.owner(x)) {
       VarSubstitutionInMap(t.owner,t.nameMap - tcx.name(x))
     } else {
       isubstitution(t)
     }
  }

  override def isubstitution(t: VarSubstitutionInMap): IVarSubstitution = t


  override def foldWhile[S](t: VarSubstitutionInMap, s0: S)(f: (S, (IVarTerm, ITerm)) => S)(p: S => Boolean): S = {
    var q = false
    var s = s0
    val it = t.nameMap.iterator
    while(!q && it.hasNext) {
      val (name,value) = it.next()
      val vv = (PlainVarTerm(t.owner,name),value)
      q = !p(s)
      if (!q) {
        s = f(s, vv)
      }
    }
    s
  }

}

case class MapBasedVarSubstitution(values: Map[IdentityRef[IEtaTerm],Map[IName,ITerm]]) extends IVarSubstitution
{

  override type Carrier = MapBasedVarSubstitution

  override def carrier: MapBasedVarSubstitution = this

  override def tcVarSubstitution: TCVarSubstitution[MapBasedVarSubstitution] = TCMapBasedSubstitution

}

object TCMapBasedSubstitution extends TCVarSubstitution[MapBasedVarSubstitution]
{

  override type Carrier = MapBasedVarSubstitution

  override def isContradiction(t:Carrier):Boolean = false

  override def isStar(t:Carrier): Boolean = t.values.isEmpty

  override def isubstitution(t: Carrier): IVarSubstitution = t

  override def get[X](t: Carrier, x: X)(implicit tcx: TCVarTerm[X]): Option[ITerm] = {
    t.values.get(tcx.ownerRef(x)).flatMap(n => n.get(tcx.name(x)))
  }

  def put[X,Y](t:Carrier,x:X,y:Y)(implicit tcx:TCVarTerm[X], tcy: TCTerm[Y]): IVarSubstitution = {
    val ownerRef = tcx.ownerRef(x)
    val (nMap, merged) = t.values.get(ownerRef) match {
      case None => (Map(ownerRef -> Map(tcx.name(x) -> tcy.iterm(y))),empty())
      case Some(oldNames) => val name = tcx.name(x)
        val e = empty()
        oldNames.get(name) match {
          case None => (t.values.updated(ownerRef, oldNames.updated(name, tcy.iterm(y))),e)
          case Some(oldValue) =>
            val s = oldValue.tcTerm.leftUnifyInSubst(oldValue.carrier, e.carrier, y)(e.tcVarSubstitution,tcy)
            if (s.isContradiction()) {
              (t.values,s)
            } else {
              (t.values.updated(ownerRef, oldNames.updated(name, tcy.iterm(y))),s)
            }
        }
    }
    if (merged.isContradiction()) {
      merged
    } else {
      merge(MapBasedVarSubstitution(nMap),merged)
    }
  }

  def remove[X](t:MapBasedVarSubstitution, x:X)(implicit tcx:TCVarTerm[X]): IVarSubstitution = {
     val ownerRef = tcx.ownerRef(x)
     t.values.get(ownerRef) match {
       case None => isubstitution(t)
       case Some(w) => val newNameMap = w - tcx.name(x)
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

}

object STARSubstitution extends IVarSubstitution {

  override type Carrier = STARSubstitution.type

  val tcVarSubstitution = new TCVarSubstitution[STARSubstitution.type ] {

    override def isContradiction(t: STARSubstitution.type): Boolean = false

    override def isStar(t: STARSubstitution.type): Boolean = true

    override def put[X, Y](t: STARSubstitution.type , x: X, y: Y)(implicit tcx: TCVarTerm[X], tcy: TCTerm[Y]): IVarSubstitution = MapBasedVarSubstitution(Map(tcx.ownerRef(x) -> Map( tcx.name(x) -> tcy.iterm(y))))

    override def isubstitution(t: STARSubstitution.type): IVarSubstitution = STARSubstitution

    override def get[X](t: STARSubstitution.type, x: X)(implicit tcx: TCVarTerm[X]): Option[ITerm] = None

    override def remove[X](t: STARSubstitution.type, x: X)(implicit tcx: TCVarTerm[X]): IVarSubstitution = STARSubstitution

    override def merge[S](t: STARSubstitution.type, s: S)(implicit tcs: TCVarSubstitution[S]): IVarSubstitution = t

    override def empty(): Carrier = STARSubstitution

    override def foldWhile[S](t: STARSubstitution.type, s0: S)(f: (S, (IVarTerm, ITerm)) => S)(p: S => Boolean): S = {
      s0
    }

  }

  lazy val carrier = this

}

class ContradictionSubstitution(message: => String,
    term: ITerm = PredefinedNames.UNKNOWN,
    prev: IVarSubstitution = STARSubstitution,
    trace: List[ContradictionSubstitution] = Nil) extends IVarSubstitution
{
  override type Carrier = ContradictionSubstitution

  override def carrier: Carrier = this

  override def tcVarSubstitution: TCVarSubstitution[Carrier] = ContradictionSubstitution.TC
}

object ContradictionSubstitution
{
  def apply(message: => String, term: ITerm = PredefinedNames.UNKNOWN, prev: IVarSubstitution = STARSubstitution, trace: List[ContradictionSubstitution] = Nil ) =
     new ContradictionSubstitution(message, term, prev, trace)

  object TC extends TCVarSubstitution[ContradictionSubstitution] {

    override def isubstitution(t: ContradictionSubstitution): IVarSubstitution = t

    override def isContradiction(t: ContradictionSubstitution): Boolean = true

    override def isStar(t: ContradictionSubstitution): Boolean = false

    override def put[X, Y](t: ContradictionSubstitution, x: X, y: Y)(implicit tcx: TCVarTerm[X], tcy: TCTerm[Y]): IVarSubstitution = t

    override def get[X](t: ContradictionSubstitution, x: X)(implicit tcx: TCVarTerm[X]): Option[ITerm] = None

    override def remove[X](t: ContradictionSubstitution, x: X)(implicit tcx: TCVarTerm[X]): IVarSubstitution = t

    override def foldWhile[S](t: ContradictionSubstitution, s0: S)(f: (S, (IVarTerm, ITerm)) => S)(p: S => Boolean): S
       = s0
  }

}




object Substitution {

  val STAR = STARSubstitution

  def contradiction(message: => String = "mismatch",
      term: ITerm = PredefinedNames.UNKNOWN,
      prev: IVarSubstitution = STARSubstitution,
      trace: List[ContradictionSubstitution] = Nil) =
     new ContradictionSubstitution(message, term, prev, trace)

}