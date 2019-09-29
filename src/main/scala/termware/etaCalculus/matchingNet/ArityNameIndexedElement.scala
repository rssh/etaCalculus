package termware.etaCalculus.matchingNet

import termware.etaCalculus.{ArrowsMergingPolicy, IArrows, IErrorTerm, IEtaTerm, IName, IPatternCondition, IPrimitive, IStructured, ITerm, IVarTerm, VarSubstitution}
import termware.util.FastRefOption

import scala.annotation.tailrec
import scala.collection.immutable.IntMap


case class ArityNameIndexedElement(
    byArity: IntMap[NameSameArityElement],
    anyArity: Map[IName, MatchingNetElement],
    anyNameAnyArity: PatternConditionMatchingElement,
    namesOnly: Map[IName, MatchingNetElement],
    primitives: Map[IPrimitive, MatchingNetElement],
    arrows: MatchingNetElement,
    patternConditions: MatchingNetElement,
    onFail: MatchingNetElement = NotFoundElement) extends MatchingNetElement {

  // TODO: add trace parameter  for recursive defs ?
  override def checkPattern(s: VarSubstitution, pattern: ITerm): MatchingNetPatternCheckResult = {
    pattern match {
      case IStructured(is) =>
         byArity.get(is.arity()) match {
           case Some(right) => right.checkPattern(s,pattern)
           case None => onFail.checkPattern(s,pattern)
         }
      case IName(x) =>
         namesOnly.get(x) match {
           case Some(right) => MatchingNetPatternCheckResult.success(s,right)
           case None => MatchingNetPatternCheckResult.failure(s,onFail)
         }
      case IPrimitive(p) =>
        primitives.get(p) match {
          case Some(x) => MatchingNetPatternCheckResult.success(s,x)
          case None => MatchingNetPatternCheckResult.failure(s,onFail)
        }
      case IArrows(a) =>
        arrows.checkPattern(s,pattern)
      case IPatternCondition(pc) =>
        patternConditions.checkPattern(s,pc)
      case IEtaTerm(e) =>
        checkPattern(s,e.baseTerm())
      case IVarTerm(e) =>
        e.resolve() match {
          case FastRefOption.Empty() =>
            MatchingNetPatternCheckResult.error(s,"can;t resolve var",e)
          case FastRefOption.Some(x) =>
            checkPattern(s,x)
        }
      case IErrorTerm(e) =>
        MatchingNetPatternCheckResult.error(s, "Attempt to match IError",e)
    }
  }

  override def add(index: ITerm, value: ITerm, mergingPolicy: ArrowsMergingPolicy): Either[MNContradiction,MatchingNetElement] = {
    index match {
      case IStructured(si) => addStructured(si,value, mergingPolicy)
      case IArrows(ai) => addArrows(ai,value, mergingPolicy)
      case IPatternCondition(pi) => addAnyNameAnyArity(pi, value, mergingPolicy)
      case IName(n) => addName(n,value, mergingPolicy)
      case IPrimitive(p) => addPrimitive(p,value, mergingPolicy)
      case IEtaTerm(eta) => addEta(eta,value, mergingPolicy)
      case IVarTerm(v) => addVar(v,value, mergingPolicy)
      case IErrorTerm(e) => Left(MNContradiction(e,this,"IError can't be added"))
    }
  }


  private def addStructured(si: IStructured, term: ITerm, mergingPolicy: ArrowsMergingPolicy): Either[MNContradiction,MatchingNetElement] = {
    @tailrec
    def addForward(i: Int, c: MatchingNetElement): Either[MNContradiction,MatchingNetElement] = {
      addWithArity(i,si,term, mergingPolicy) match {
        case Left(e) => Left(e)
        case Right(r) =>
          if (i < si.arity()) {
            addForward(i+1,r)
          } else {
            Right(r)
          }
      }
    }
    addForward(minArity(si),this)
  }



  private def minArity(t: IStructured): Int = {
    t.foldMetas(0) { (s, e) =>
      if (e.defValue.isEmpty) {
        s+1
      } else s
    }
  }


  private def addWithArity(i: Int, index: IStructured, value: ITerm, mergingPolicy: ArrowsMergingPolicy):
                Either[MNContradiction,ArityNameIndexedElement] = {
    val n = byArity.get(i) match {
      case Some(n) => n.addSameArity(index,value, mergingPolicy)
      case None => Right(NameSameArityElement(
        Map(index.name -> createArgChecker(0,index,value)),
        onFail
      ))
    }
    n.map(x => copy(byArity = byArity.updated(i,x)))
  }

  private def addArrows(arrowsIndex: IArrows, value: ITerm, mergingPolicy: ArrowsMergingPolicy):
                                 Either[MNContradiction,MatchingNetElement]  = {
    arrows.add(arrowsIndex,value, mergingPolicy).map(x => copy(arrows = x))
  }

  private def addName(name: IName, term: ITerm, mergingPolicy: ArrowsMergingPolicy): Either[MNContradiction,MatchingNetElement] = {
    namesOnly.get(name) match {
      case Some(found) => found.add(name,term,mergingPolicy)
      case None => val nNames = namesOnly.updated(name,FoundElement(term))
        Right(copy(namesOnly = nNames))
    }
  }

  private def addPrimitive(primitive: IPrimitive, term: ITerm, mergingPolicy: ArrowsMergingPolicy): Either[MNContradiction,MatchingNetElement] = {
    primitives.get(primitive) match {
      case Some(found) => found.add(primitive,term,mergingPolicy)
      case None => val nPrimitives = primitives.updated(primitive,FoundElement(term))
        Right(copy(primitives = nPrimitives))
    }
  }

  private def addAnyNameAnyArity(index: IPatternCondition, value: ITerm, mergingPolicy: ArrowsMergingPolicy):
  Either[MNContradiction, MatchingNetElement] = {
    anyNameAnyArity.addPatternCondition(index,value, mergingPolicy).map( x =>
      copy(anyNameAnyArity = x)
    )
  }

  private def addEta(index: IEtaTerm, value: ITerm, mergingPolicy: ArrowsMergingPolicy):
  Either[MNContradiction,MatchingNetElement] = {
    // now - leave as it, later will retrieve top context.
    add(index.baseTerm(),value, mergingPolicy)
  }

  private def addVar(index: IVarTerm, term1: ITerm, mergingPolicy: ArrowsMergingPolicy):
  Either[MNContradiction,MatchingNetElement] = {
    index.owner.resolve(index.name) match {
      case FastRefOption.Empty() => Left(MNContradiction(index, this,"variable not exists"))
      case FastRefOption.Some(p) =>
        // TODO:  support letrec.
        add(p,term1, mergingPolicy)
    }
  }

  private def createArgChecker(i: Int, indexTerm: IStructured, value: ITerm):
  MatchingNetElement = {
    val next = if (i < indexTerm.arity()-1) {
      createArgChecker(i+1,indexTerm,value)
    } else {
      FoundElement(value)
    }
    new CheckTermMatchingElement(indexTerm,next,onFail)
  }

  override def isFinal(): Boolean = false
}

