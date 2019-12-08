package termware.etaCalculus
import termware.etaCalculus.algo.Contains
import termware.util.FastRefOption


trait TCPatternCondition[T] extends TCTerm[T] {

  def iPatternCondition(t:T): IPatternCondition = CPatternCondition(t,this)

  def expression(t:T): ITerm

  def thisVar(t:T): FastRefOption[IVarTerm]

  def substExpression(t:T, nExpr: ITerm, nThisVar: FastRefOption[IVarTerm]): IPatternCondition

  override def hasPatternsRec(t: T,trace: Map[IVarTerm, Boolean]): Boolean = true

  override def tcPatternCondition(t: T): FastRefOption[TCPatternCondition[T]] = FastRefOption(this)

  override def tcEta(t: T): FastRefOption[TCEtaTerm[T]] = FastRefOption.empty
  override def tcName(t: T): FastRefOption[TCName[T]] = FastRefOption.empty
  override def tcVar(t: T): FastRefOption[TCVarTerm[T]] = FastRefOption.empty
  override def tcPrimitive(t: T): FastRefOption[TCPrimitive[T]] = FastRefOption.empty
  override def tcError(t: T): FastRefOption[TCErrorTerm[T]] = FastRefOption.empty
  override def tcArrows(t: T): FastRefOption[TCArrows[T]] =  FastRefOption.empty
  override def tcStructured(t: T): FastRefOption[TCStructured[T]] = FastRefOption.empty


  override def termEqNoRef(t: T, otherTerm: ITerm): Boolean = {
    otherTerm match {
      case IPatternCondition(otherExpression) => expression(t).termEqNoRef(otherExpression)
      case _ => false
    }
  }

}


trait IPatternCondition extends ITerm {

  override final def tcTerm = tcGuarded

  def tcGuarded: TCPatternCondition[Carrier]

  def expression: ITerm =
    tcGuarded.expression(carrier)

  def thisVar: FastRefOption[IVarTerm] =
    tcGuarded.thisVar(carrier)

  def substExpression(nExpr: ITerm, nThisVar: FastRefOption[IVarTerm]): IPatternCondition =
    tcGuarded.substExpression(carrier, nExpr, nThisVar)

  override def kindTransform[B](matcher: TermKindTransformer[B], vo: Map[IEtaTerm, IEtaTerm]): B = {
     matcher.onPatternCondition(this,vo)
  }

  override def kindFold[S](s0: S)(folder: TermKindFolder[S]): S = {
    folder.onPatternCondition(this,s0)
  }

}

object IPatternCondition
{

  def unapply(arg: ITerm): FastRefOption[IPatternCondition] = {
    if (arg.isInstanceOf[IPatternCondition]) {
      FastRefOption(arg.asInstanceOf[IPatternCondition])
    } else {
      FastRefOption.empty
    }
  }

  private lazy val prototypeStaticContext = IEtaTerm(
    PredefinedNames.THIS -> IPatternCondition.all
  )(StringName("StaticContext"))

  lazy val universalThis = IVarTerm(prototypeStaticContext,PredefinedNames.THIS)

  lazy val all: IPatternCondition = ConstantPatternCondition(BoolPrimitive.TRUE)
  lazy val nothing: IPatternCondition = ConstantPatternCondition(BoolPrimitive.FALSE)

}

case class CPatternCondition[T](override val carrier:T, override val tcGuarded:TCPatternCondition[T]) extends IPatternCondition {

  type Carrier = T

}


case class PlainPatternCondition(
    override val expression: ITerm,
    val inThisVar: IVarTerm) extends IPatternCondition {

  override type Carrier = PlainPatternCondition

  override def carrier: PlainPatternCondition = this

  override def thisVar: FastRefOption[IVarTerm] = FastRefOption(inThisVar)

  override def tcGuarded: TCPatternCondition[Carrier] = TCPlainPatternCondition

  private final def change(modify: (ITerm, Map[IEtaTerm,IEtaTerm]) => (ITerm,Map[IEtaTerm,IEtaTerm]), vo0: Map[IEtaTerm,IEtaTerm]): IPatternCondition = {
    val (ne, vo) = modify(expression,vo0)
    if (ne eq expression) {
      this
    } else {
      val nThisEta = vo.getOrElse(inThisVar.owner,inThisVar.owner)
      val nThisVar = if (nThisEta eq inThisVar.owner) {
        inThisVar
      } else {
        PlainVarTerm(nThisEta,inThisVar.name)
      }
      PlainPatternCondition(ne, nThisVar)
    }
  }

  override def mapVars(f: IVarTerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    change((t,vo) => (t.mapVars(f,vo),vo), vo )
  }

  override def substVars(s: VarSubstitution, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    change((t,vo) => (t.substVars(s,vo), vo), vo)
  }

  override def leftUnifyInSubst(s: VarSubstitution, o: ITerm): UnificationResult = {
        PredefLogicInterpretations.instance.check(o,s)
  }

  override def substExpression(nExpr: ITerm, nThisVar: FastRefOption[IVarTerm]): IPatternCondition = {
    nThisVar match {
      case FastRefOption.Some(v) =>
         copy(expression = nExpr, inThisVar = v)
      case FastRefOption.Empty() =>
         // check, are we have old var there ?
         if (Contains(nExpr,inThisVar)) {
           copy(expression = nExpr)
         } else {
           new ConstantPatternCondition(nExpr)
         }


    }
  }


}

object TCPlainPatternCondition extends TCPatternCondition[PlainPatternCondition] {

  type Carrier = PlainPatternCondition

  override def expression(t: PlainPatternCondition): ITerm = t.expression

  override def substExpression(t: PlainPatternCondition, nExpr: ITerm, nThisVar: FastRefOption[IVarTerm]): IPatternCondition =
    t.substExpression(nExpr,nThisVar)

  override def thisVar(t: Carrier): FastRefOption[IVarTerm] = t.thisVar

  override def mapVars(t: PlainPatternCondition, f: IVarTerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = t.mapVars(f,vo)

  override def leftUnifyInSubst(t: PlainPatternCondition, s: VarSubstitution, o: ITerm): UnificationResult = t.leftUnifyInSubst(s,o)

}

/**
  * Condition, which is not depends from this.
  * They should be in onw class, because unoversalThis, which is
  * used as argumement for PlainPatternCondition, is vaiable
  * which should be resolved in context to IPatternCondition.ALL
  * So if we will use ALL as PlainPattern, we will receive loop in
  * value initializations.
  *
  */
case class ConstantPatternCondition(override val expression: ITerm) extends IPatternCondition {

  override type Carrier = ConstantPatternCondition

  override def tcGuarded: TCPatternCondition[ConstantPatternCondition] = TCConstantPatternCondition

  override def carrier: ConstantPatternCondition = this

  override def thisVar: FastRefOption[IVarTerm] = FastRefOption.empty

  override def substExpression(nExpr: ITerm, nThisVar: FastRefOption[IVarTerm]): IPatternCondition =
    nExpr match {
      case IPrimitive(p) =>
        if (p.primitiveTypeIndex == PrimitiveTypeIndexes.BOOLEAN) {
          val b:Boolean = p.value.asInstanceOf[Boolean]
          ConstantPatternCondition(CPrimitive[Boolean](b,TCBoolPrimitive))
        } else {
           PlainPatternCondition(nExpr,IPatternCondition.universalThis)
        }
      case _ =>
        PlainPatternCondition(nExpr,IPatternCondition.universalThis)
    }

  override def mapVars(f: IVarTerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
    this
  }

  override def leftUnifyInSubst(s: VarSubstitution, o: ITerm): UnificationResult = {
    PredefLogicInterpretations.instance.check(expression,s)
  }

}

object ConstantPatternCondition {

  val TRUE = ConstantPatternCondition(BoolPrimitive(true))

  val FALSE = ConstantPatternCondition(BoolPrimitive(false))

}

object TCConstantPatternCondition extends TCPatternCondition[ConstantPatternCondition] {


  override def expression(t: ConstantPatternCondition): ITerm = t.expression

  override def thisVar(t: ConstantPatternCondition): FastRefOption[IVarTerm] =
    FastRefOption.empty

  override def substExpression(t: ConstantPatternCondition, nExpr: ITerm, nThisVar: FastRefOption[IVarTerm]): IPatternCondition = t.substExpression(nExpr, nThisVar)

  override def mapVars(t: ConstantPatternCondition, f: IVarTerm => ITerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = t.mapVars(f,vo)


  override def leftUnifyInSubst(t: ConstantPatternCondition, s: VarSubstitution, o: ITerm): UnificationResult = t.leftUnifyInSubst(s,o)


}