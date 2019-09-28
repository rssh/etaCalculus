package termware.etaCalculus.matchingNet

import termware.etaCalculus.{Arrow, ArrowPatternCheckSuccess, EmptyArrows, IArrows, IErrorTerm, IEtaTerm, IName, IPatternCondition, IPrimitive, IStructured, ITerm, IVarTerm, UnificationFailure, UnificationSuccess, VarSubstitution}
import termware.util.FastRefOption

import scala.annotation.tailrec
import scala.collection.immutable.IntMap

case class MatchingNetPatternCheckResult(
    substitution: VarSubstitution,
    nextElement: MatchingNetElement,
    success: Boolean
)

object MatchingNetPatternCheckResult {

  def success(s:VarSubstitution, next: MatchingNetElement) =
    MatchingNetPatternCheckResult(s,next,true)

  def failure(s:VarSubstitution, next: MatchingNetElement) =
    MatchingNetPatternCheckResult(s,next,false)

  def error(s:VarSubstitution, msg: String, pattern: ITerm) =
    MatchingNetPatternCheckResult.failure(s,ErrorMatchingNetElement(msg,pattern))

}

case class MNContradiction(index:ITerm, target: MatchingNetElement, msg:String)

trait MatchingNetElement {

  def checkPattern(s: VarSubstitution, pattern: ITerm): MatchingNetPatternCheckResult

  def add(index:ITerm, value: ITerm): Either[MNContradiction,MatchingNetElement]

  def isFinal(): Boolean

}



sealed trait FinalElement extends MatchingNetElement {
  override def isFinal(): Boolean = true
}

case class FoundElement(value: ITerm) extends FinalElement {

  override def checkPattern(s: VarSubstitution, pattern: ITerm): MatchingNetPatternCheckResult = ???

  override def add(index: ITerm, value: ITerm): Either[MNContradiction, MatchingNetElement] = Left(MNContradiction(index,this,"attempt to "))

}

case object NotFoundElement extends FinalElement {

  override def add(index: ITerm, value: ITerm): Either[MNContradiction,MatchingNetElement] = {
     Right(CheckTermMatchingElement(index,FoundElement(value),this))
  }

  override def checkPattern(s: VarSubstitution, pattern: ITerm): MatchingNetPatternCheckResult = {
    MatchingNetPatternCheckResult(s,this,false)
  }

}

case class ErrorMatchingNetElement(msg: String, pattern: ITerm) extends FinalElement {

  override def checkPattern(s: VarSubstitution, pattern: ITerm): MatchingNetPatternCheckResult =
    MatchingNetPatternCheckResult.failure(s,this)

  override def add(index: ITerm, value: ITerm): Either[MNContradiction, MatchingNetElement] = Left(MNContradiction(index,this,"Can't add to error term"))

}