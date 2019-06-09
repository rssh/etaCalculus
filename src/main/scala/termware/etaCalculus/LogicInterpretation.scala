package termware.etaCalculus

import termware.etaCalculus.algo.UnEta
import termware.util.FastRefOption

trait LogicInterpretation {


  def check(expression: ITerm, substitution: Substitution[IVarTerm,ITerm]): LogicResult

}


/**
  * Needed for bootstrapping
  * @param functions
  */
class StdLogicInterpretation(
     functions: Map[IName, StdLogicInterpretation.Fun]
   ) extends LogicInterpretation {

  thisLogic =>

  class CheckMatcher(substitution: Substitution[IVarTerm,ITerm]) extends TermKindTransformer[LogicResult] {

    override def onName(name: IName, vo: Map[IEtaTerm, IEtaTerm]): LogicResult =
      LogicFailure.fromMessage("boolean expected", name)

    override def onVar(varTerm: IVarTerm, vo: Map[IEtaTerm, IEtaTerm]): LogicResult = {
      varTerm.owner.context().get(varTerm.name) match {
         case Some(value) => value.transform(this, vo) // TODO: add history
         case None =>
           substitution.get(varTerm) match {
             case Some(value) => value.transform(this,vo)
             case None => LogicFailure.fromMessage(s"undefined variable", varTerm)
           }
      }
    }

    override def onPrimitive(primitive: IPrimitive, vo: Map[IEtaTerm, IEtaTerm]): LogicResult = {
      if (primitive.primitiveTypeIndex == TCBoolPrimitive.primitiveTypeIndex(true)) {
        val booleanPrimitive = primitive.asInstanceOf[IPrimitive{ type Carrier=Boolean }]
        if (booleanPrimitive.carrier) {
          LogicProof.TRUE
        } else {
          LogicFailure.fromMessage("false result",primitive)
        }
      } else {
        LogicFailure.fromMessage("non-boolean result", primitive)
      }
    }

    override def onStructured(structured: IStructured, vo: Map[IEtaTerm, IEtaTerm]): LogicResult = {
      functions.get(structured.name()) match {
        case Some(f) => val evaluated = new EvalMatcher(substitution).onStructured(structured,vo)
          evaluated.transform(this,vo)
        case None => LogicFailure.fromMessage("Unknown function ",structured)
      }
    }

    override def onEta(eta: IEtaTerm, vo: Map[IEtaTerm, IEtaTerm]): LogicResult = {
      eta.baseTerm().transform(this,vo)
    }

    override def onError(error: IErrorTerm, vo: Map[IEtaTerm, IEtaTerm]): LogicResult = {
      LogicFailure.apply(Set(),error,"error occured")
    }

    override def onPatternCondition(patternCondition: IPatternCondition, vo: Map[IEtaTerm, IEtaTerm]): LogicResult = {
      LogicFailure.fromMessage("pattern can't be a logical expression",patternCondition)
    }
  }

  class EvalMatcher(substitution: Substitution[IVarTerm,ITerm]) extends TermKindTransformer[ITerm] {

    thisMatcher =>

    override def onName(name: IName, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
      name
    }

    override def onVar(varTerm: IVarTerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
        varTerm.owner.context().get(varTerm.name) match {
          case Some(value) =>
            value.asPatternCondition() match {
              case FastRefOption.Some(ptv) =>
                 substitution.get(varTerm) match {
                   case Some(v) =>
                     if (ptv.interpret(substitution,v)) {
                       v
                     } else {
                       //TODO: better diagnostics
                       IErrorTerm(s"check failied, expression=${ptv.expression}, value=${v}")
                     }
                   case None =>
                     IErrorTerm(s"unbinded variable: ${varTerm}")
                 }
              case FastRefOption.Empty() =>
                value.transform(thisMatcher,vo)
            }
          case None =>
            IErrorTerm(s"Unbinded variable: ${varTerm}")
        }
    }

    override def onPrimitive(primitive: IPrimitive, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
      primitive
    }

    override def onStructured(structured: IStructured, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
      functions.get(structured.name()) match {
        case Some(f) =>
          val n = structured.mapSubterms(_.transform(thisMatcher,vo),vo,true)
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
           PlainEtaTerm.TraveseTransformers(eta, (x,vo) => x.transform(thisMatcher,vo) ,vo),
           eta.context(),
           eta.baseTerm()
         )
       } else {
         eta.baseTerm().transform(thisMatcher,vo)
         //eta.transform(UnEta, vo).transform(thisMatcher, vo)
       }
    }

    override def onError(error: IErrorTerm, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
      error
    }

    override def onPatternCondition(patternCondition: IPatternCondition, vo: Map[IEtaTerm, IEtaTerm]): ITerm = {
      IErrorTerm("Pattern Condition can't be part of PatternCondition expression")
    }

  }

  override def check(expression: ITerm, substitution: Substitution[IVarTerm, ITerm]): LogicResult = {
     expression.transform(new CheckMatcher(substitution), Map.empty)
  }

}

object StdLogicInterpretation {

  case class FunInput(
      term: IStructured,
      substitution: Substitution[IVarTerm,ITerm],
      eval: TermKindTransformer[ITerm],
      vo: Map[IEtaTerm,IEtaTerm]
  )

  type Fun = FunInput => ITerm



}

object PredefLogicInterpretations {


  def lAnd(in: StdLogicInterpretation.FunInput):ITerm = {
    in.term.foldSubtermsWhile[ITerm](TCBoolPrimitive.TRUE){ (s,e) =>
        val e1 = e.transform(in.eval,in.vo) //Mb move deep down
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
      val e1 = e.transform(in.eval, in.vo)
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
        case FastRefOption.Some(e) => val e1 = e.transform(in.eval,in.vo)
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


