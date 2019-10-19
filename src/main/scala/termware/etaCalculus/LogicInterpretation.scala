package termware.etaCalculus

import termware.etaCalculus.algo.UnEta
import termware.util.FastRefOption

trait LogicInterpretation {


  def check(expression: ITerm, substitution: VarSubstitution): UnificationResult


}


/**
  * Needed for bootstrapping
  * @param functions
  */
class StdLogicInterpretation(
     functions: Map[IName, StdLogicInterpretation.Fun]
   ) extends LogicInterpretation {

  thisLogic =>

  class CheckMatcher(substitution: VarSubstitution) extends TermKindTransformer[UnificationResult] {

    override def onName(name: IName, vo: Map[IEtaTerm, IEtaTerm]): UnificationResult =
      UnificationFailure.fromMessage("boolean expected", name, BoolPrimitive(true), substitution)

    override def onVar(varTerm: IVarTerm, vo: Map[IEtaTerm, IEtaTerm]): UnificationResult = {
      varTerm.owner.context().get(varTerm.name) match {
         case FastRefOption.Some(value) => value.kindTransform(this, vo) // TODO: add history
         case FastRefOption.Empty() =>
           substitution.get(varTerm) match {
             case FastRefOption.Some(value) => value.kindTransform(this,vo)
             case FastRefOption.Empty() => failure(s"undefined variable", varTerm, substitution)
           }
      }
    }

    override def onPrimitive(primitive: IPrimitive, vo: Map[IEtaTerm, IEtaTerm]): UnificationResult = {
      if (primitive.primitiveTypeIndex == TCBoolPrimitive.primitiveTypeIndex(true)) {
        val booleanPrimitive = primitive.asInstanceOf[IPrimitive{ type Carrier=Boolean }]
        if (booleanPrimitive.carrier) {
          UnificationSuccess(substitution)
        } else {
          failure("false result",primitive,substitution)
        }
      } else {
        failure("non-boolean result", primitive,substitution)
      }
    }

    override def onStructured(structured: IStructured, vo: Map[IEtaTerm, IEtaTerm]): UnificationResult = {
      functions.get(structured.name()) match {
        case Some(f) => val evaluated = new EvalMatcher(substitution).onStructured(structured,vo)
          evaluated.kindTransform(this,vo)
        case None => failure("Unknown function ",structured, substitution)
      }
    }

    override def onEta(eta: IEtaTerm, vo: Map[IEtaTerm, IEtaTerm]): UnificationResult = {
      eta.baseTerm().kindTransform(this,vo)
    }

    override def onError(error: IErrorTerm, vo: Map[IEtaTerm, IEtaTerm]): UnificationResult = {
      failure("error occured",error,substitution)
    }

    override def onPatternCondition(patternCondition: IPatternCondition, vo: Map[IEtaTerm, IEtaTerm]): UnificationResult = {
      failure("pattern can't be a logical expression",patternCondition,substitution)
    }

    // TODO: applyArroe
    override def onArrows(arrow: IArrows, vo: Map[IEtaTerm, IEtaTerm]): UnificationResult = {
      failure("arrow can't be a logical expression",arrow,substitution)
    }

  }

  class EvalMatcher(substitution: VarSubstitution) extends TermKindTransformer[ITerm] {

    thisMatcher =>

    override def onName(name: IName, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
      name
    }

    override def onVar(varTerm: IVarTerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
        varTerm.owner.context().get(varTerm.name) match {
          case FastRefOption.Some(value) =>
            value.asPatternCondition() match {
              case FastRefOption.Some(ptv) =>
                 substitution.get(varTerm) match {
                   case FastRefOption.Some(v) =>
                     v.kindTransform(this,vo)
                   case FastRefOption.Empty() =>
                     IErrorTerm(s"unbinded variable: ${varTerm}")
                 }
              case FastRefOption.Empty() =>
                value.kindTransform(thisMatcher,vo)
            }
          case FastRefOption.Empty() =>
            IErrorTerm(s"Unbinded variable: ${varTerm}")
        }
    }

    override def onPrimitive(primitive: IPrimitive, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
      primitive
    }

    override def onStructured(structured: IStructured, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
      functions.get(structured.name()) match {
        case Some(f) =>
          val n = structured.mapSubterms(_.kindTransform(thisMatcher,vo),vo,true)
          n.asStructured() match {
            case FastRefOption.Some(ns) => f.apply(StdLogicInterpretation.FunInput(ns,substitution,this,vo))
            case other => IErrorTerm(s"Structure expected, have ${n}")
          }
        case None =>
          IErrorTerm(s"Unknown function ${structured.name()}")
      }
    }

    override def onEta(eta: IEtaTerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
       if (eta.hasPatterns()) {
         new PlainEtaTerm(
           PlainEtaTerm.TraveseTransformers(eta, (x,vo) => x.kindTransform(thisMatcher,vo) ,vo),
           eta.context(),
           eta.baseTerm()
         )
       } else {
         eta.baseTerm().kindTransform(thisMatcher,vo)
         //eta.transform(UnEta, vo).transform(thisMatcher, vo)
       }
    }

    override def onError(error: IErrorTerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
      error
    }

    override def onPatternCondition(patternCondition: IPatternCondition, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
      IErrorTerm("Pattern Condition can't be part of PatternCondition expression")
    }

    // TODO: applyArroe
    override def onArrows(arrow: IArrows, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
      IErrorTerm("arrow can't be a logical expression")
    }


  }

  override def check(expression: ITerm, substitution: VarSubstitution): UnificationResult = {
     expression.kindTransform(new CheckMatcher(substitution), Map.empty)
  }

  def failure(msg:String,frs: ITerm, s: VarSubstitution): UnificationFailure = {
    UnificationFailure(msg,frs,BoolPrimitive.TRUE,s)
  }

}

object StdLogicInterpretation {

  case class FunInput(
      term: IStructured,
      substitution: VarSubstitution,
      eval: TermKindTransformer[ITerm],
      vo: Map[IEtaTerm,IEtaTerm]
  )

  type Fun = FunInput => ITerm



}

object PredefLogicInterpretations {


  def lAnd(in: StdLogicInterpretation.FunInput):ITerm = {
    in.term.foldSubtermsWhile[ITerm](TCBoolPrimitive.TRUE){ (s,e) =>
        val e1 = e.kindTransform(in.eval,in.vo) //Mb move deep down
        e1.asPrimitive() match {
          case FastRefOption.Some(pe) =>
            if (pe.primitiveTypeIndex == PrimitiveTypeIndexes.BOOLEAN) {
              pe
            } else {
              IErrorTerm("boolean required")
            }
          case FastRefOption.Empty() =>
            if (e1.isError())
              e1
             else
              IErrorTerm(s"boolean primitive required, have ${e1}")
        }
    }{ BoolPrimitive.unapply(_).contains(true) }
  }

  def lOr(in:StdLogicInterpretation.FunInput):ITerm = {
    in.term.foldSubtermsWhile[ITerm](TCBoolPrimitive.FALSE){ (s,e) =>
      val e1 = e.kindTransform(in.eval, in.vo)
      e1 match {
        case BoolPrimitive(x) => e1
        case IErrorTerm(x) => x
        case other => IErrorTerm(s"boolean primitive required: ${e1}")
      }
    }{ x => ! x.isError() && BoolPrimitive.unapply(x).contains(false) }
  }

  def lNot(in:StdLogicInterpretation.FunInput):ITerm = {
    if (in.term.arity() != 1) {
      IErrorTerm(s"not arity shpuld be 1 in ${in.term}")
    } else {
      in.term.subterm(0) match {
        case FastRefOption.Some(e) => val e1 = e.kindTransform(in.eval,in.vo)
          e1 match {
            case BoolPrimitive(x) => BoolPrimitive(!x)
            case IErrorTerm(x) => x
            case other => IErrorTerm(s"Boolean primive required: ${other}")
          }
        case FastRefOption.Empty() =>
          IErrorTerm(s"0 subterm is absent in ${in.term}")
      }
    }
  }

  def lUnify(in: StdLogicInterpretation.FunInput): ITerm = {
    if (in.term.arity()!=2) {
      IErrorTerm(s"arity fo unificated should be 2 in ${in.term}")
    } else {
      val l = in.term.subterm(0).get()
      val r = in.term.subterm(1).get()
      val ur = l.leftUnifyInSubst(in.substitution,r)
      BoolPrimitive(ur.isSuccess())
    }
  }

  def lEq(in: StdLogicInterpretation.FunInput): ITerm = {
    if (in.term.arity()!=2) {
      IErrorTerm(s"arity for eq should be 2 in ${in.term}")
    } else {
      val l = in.term.subterm(0).get()
      val r = in.term.subterm(1).get()
      val er = l.termEqNoRef(r)
      BoolPrimitive(er)
    }
  }


  val functions = Map[IName, StdLogicInterpretation.FunInput => ITerm](
    PredefinedNames.AND -> lAnd,
    PredefinedNames.OR -> lOr,
    PredefinedNames.NOT -> lNot,
    PredefinedNames.EQ -> lEq,
    PredefinedNames.UNIFY -> lUnify
  )

  val instance = new StdLogicInterpretation(functions)

}


