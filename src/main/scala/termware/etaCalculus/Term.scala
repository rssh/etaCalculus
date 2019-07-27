package termware.etaCalculus

import termware.util.FastRefOption

import scala.reflect.ClassTag


trait TCTerm[T] extends TCLeftUnificable[T] {

   def iterm(t:T):ITerm = CTerm(t,this)

   def substVars(t:T, s: VarSubstitution, vo:Map[IEtaTerm,IEtaTerm]): ITerm = {
     mapVars(t, v => s.getOrElse(v,v), vo )
   }

   def mapVars(t:T, f: IVarTerm => ITerm, vo:Map[IEtaTerm,IEtaTerm]): ITerm

   def subst[N<:ITerm, V<:ITerm](t:T, s: Substitution[N,V], vo:Map[IEtaTerm,IEtaTerm])(implicit nTag: ClassTag[N]): ITerm

   def hasPatterns(t:T): Boolean = hasPatternsRec(t,Map.empty)

   def hasPatternsRec(t:T, trace:Map[IVarTerm,Boolean]): Boolean

  /**
    * Check for term equality without referential transparency  (but ignoring extra context layers)
    * i.e.  termEqNoRef(eta x -> 1 : 1, 1 ) == true
    *  but
    *       termEqNoRef(eta x -> 1 : x, 1) == false
    * @param t
    * @param otherTerm
    * @return
    */
   def termEqNoRef(t:T, otherTerm:ITerm): Boolean

   // TODO: define with trace and equential transparency.
   //def termEq(t:T, otherTerm: ITerm, trace: Map[IVarTerm,ITerm]): Boolean

   def tcName(t: T): FastRefOption[TCName[T]]
   def isName(t: T): Boolean = tcName(t).isDefined
   def tcVar(t:T): FastRefOption[TCVarTerm[T]]
   def isVar(t:T): Boolean = tcVar(t).isDefined
   def tcPrimitive(t:T): FastRefOption[TCPrimitive[T]]
   def isPrimitive(t:T): Boolean = tcPrimitive(t).isDefined
   def tcStructured(t:T): FastRefOption[TCStructured[T]]
   def isStructured(t:T): Boolean = tcStructured(t).isDefined
   def tcPatternCondition(t:T): FastRefOption[TCPatternCondition[T]]
   def isPatternCondition(t:T): Boolean = tcPatternCondition(t).isDefined
   def tcEta(t:T): FastRefOption[TCEtaTerm[T]]
   def isEta(t:T): Boolean = tcEta(t).isDefined
   def tcError(t:T): FastRefOption[TCErrorTerm[T]]
   def isError(t:T): Boolean = tcError(t).isDefined



}

trait ITerm extends ILeftUnificable
{

  type Carrier

  def tcTerm: TCTerm[Carrier]

  def carrier: Carrier

  def hasPatterns(): Boolean = hasPatternsRec(Map.empty)

  def hasPatternsRec(trace:Map[IVarTerm,Boolean]): Boolean = {
    tcTerm.hasPatternsRec(carrier, trace)
  }

  def isName(): Boolean = tcTerm.isName(carrier)

  def asName(): FastRefOption[IName] = tcTerm.tcName(carrier).map(_.iname(carrier))

  def isPrimitive(): Boolean = tcTerm.isPrimitive(carrier)

  def asPrimitive(): FastRefOption[IPrimitive] = tcTerm.tcPrimitive(carrier).map(_.iprimitive(carrier))

  def isVar(): Boolean = tcTerm.isVar(carrier)

  def asVar(): FastRefOption[IVarTerm] = tcTerm.tcVar(carrier).map(_.ivar(carrier))

  def isStructured(): Boolean = tcTerm.isStructured(carrier)

  def asStructured(): FastRefOption[IStructured] = tcTerm.tcStructured(carrier).map(_.istructured(carrier))

  def isEta(): Boolean = tcTerm.isEta(carrier)

  def asEta(): FastRefOption[IEtaTerm] = tcTerm.tcEta(carrier).map(_.ieta(carrier))

  def isPatternCondition(): Boolean = tcTerm.isPatternCondition(carrier)

  def asPatternCondition(): FastRefOption[IPatternCondition] =
      tcTerm.tcPatternCondition(carrier).map(_.iPatternCondition(carrier))

  def isError(): Boolean = tcTerm.isError(carrier)

  def asError(): FastRefOption[IErrorTerm] = tcTerm.tcError(carrier).map(_.ierror(carrier))

  def leftUnifyInSubst(s: VarSubstitution, o: ITerm): UnificationResult = {
    tcTerm.leftUnifyInSubst(carrier,s,o)
  }

  def substVars(s: VarSubstitution, vo: Map[IEtaTerm,IEtaTerm]): ITerm = {
    tcTerm.substVars(carrier,s,vo)
  }

  def mapVars(f: IVarTerm => ITerm, vo: Map[IEtaTerm,IEtaTerm]): ITerm = {
    tcTerm.mapVars(carrier,f, vo)
  }

  def subst[N<:ITerm,V<:ITerm](s: Substitution[N,V], vo: Map[IEtaTerm,IEtaTerm])(implicit nTag:ClassTag[N]): ITerm = {
    tcTerm.subst(carrier,s,vo)
  }

  def termEqNoRef(o: ITerm): Boolean = {
    tcTerm.termEqNoRef(carrier,o)
  }

  def kindTransform[B](matcher: TermKindTransformer[B], vo: Map[IEtaTerm,IEtaTerm]):B

  def kindFold[S](s0:S)(folder: TermKindFolder[S]):S

}

object ITerm extends ITermConversions {

  type Aux[T] = ITerm{ type Carrier = T }

}

case class CTerm[T](t:T,tc:TCTerm[T]) extends ITerm
{
   type Carrier = T

   def tcTerm: TCTerm[Carrier] = tc

   def carrier: Carrier = t

  override def kindTransform[B](matcher: TermKindTransformer[B], vo: Map[IEtaTerm,IEtaTerm]): B = {
    // need to be reviewed after adding of each term type
    this match {
      case IName(x) => matcher.onName(x,vo)
      case IPrimitive(x) => matcher.onPrimitive(x,vo)
      case IVarTerm(x) => matcher.onVar(x,vo)
      case IEtaTerm(x) => matcher.onEta(x,vo)
      case IStructured(x) => matcher.onStructured(x,vo)
      case IErrorTerm(x) => matcher.onError(x,vo)
      case IPatternCondition(x) => matcher.onPatternCondition(x,vo)
    }
  }

  override def kindFold[S](s0: S)(folder: TermKindFolder[S]): S = {
    // need to be reviewed after adding of each term type
    this match {
      case IName(x) => folder.onName(x,s0)
      case IPrimitive(x) => folder.onPrimitive(x,s0)
      case IVarTerm(x) => folder.onVar(x,s0)
      case IEtaTerm(x) => folder.onEta(x,s0)
      case IStructured(x) => folder.onStructured(x,s0)
      case IErrorTerm(x) => folder.onError(x,s0)
      case IPatternCondition(x) => folder.onPatternCondition(x,s0)
    }
  }

}

trait TCTermLowPriorityImplicits {



}
