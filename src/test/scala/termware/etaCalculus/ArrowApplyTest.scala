package termware.etaCalculus

import org.scalatest.FunSuite
import termware.NameSubstitution
import termware.util.SimplePrint

class ArrowApplyTest extends FunSuite {

  test("basic structured application") {
    val st1 = PlainStructured.freeIndexed("a", 1, 2, 3)
    val st2 = PlainStructured.freeIndexed("b", 1, 2, 3)
    val arrow = Arrow(st1, st2, EmptyArrows)
    val r0 = arrow.termApply(VarSubstitution.empty(), PlainStructured.freeIndexed("a", 1, 2, 3))
    val r = r0.get()
    assert(r.isStructured())
    assert(r.asStructured().get() == st2)
  }

  test("basic eta-structured application") {
    val x = StringName("x")
    val y = StringName("y")
    val st1 = PlainStructured.freeIndexed("a", 1, x, y)

    val st2 = PlainStructured.freeIndexed("b", 1, 3, 4)
    val preArrow = Arrow(st1, st2, EmptyArrows)
    val etaArrow = IEtaTerm.create(
      NameSubstitution(
         x -> IPatternCondition.all,
         y -> IPatternCondition.all),preArrow)
    val arrow = etaArrow.baseTerm().asArrows().get()
    val arrowLeft = arrow.linear()(0)._1
    val vx = arrowLeft.asStructured().get().subterm(1).get()
    Console.println("vx = " + SimplePrint(vx))
    assert(vx.isVar())

    Console.println("etaArrow="+SimplePrint(etaArrow))
    val cr0 = arrow.checkPattern(VarSubstitution.empty(),PlainStructured.freeIndexed("a", 1, 2, 3))
    assert(cr0.isSuccess())

    val r = arrow.termApply(VarSubstitution.empty(),PlainStructured.freeIndexed("a", 1, 2, 3)).get()
    assert(r.isStructured())
    assert(r.asStructured().get() == st2)
  }



}
