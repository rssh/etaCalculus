package termware.etaCalculus

import org.scalatest.FunSuite
import termware.NameSubstitution

class SubstVarTest extends FunSuite {



  test("substitute vars in simple structured term") {
      val x = StringName("x")
      val y = StringName("y")
      val ctx = NameSubstitution(
        x -> IPatternCondition.all,
        y -> IPatternCondition.all
      )
      val st1 = PlainStructured.freeIndexed("a", 1, x, y)
      val eta = IEtaTerm.create(ctx,st1)

      val vx = eta.baseTerm().asStructured().get().subterm(1).get().asVar().get()
      assert(vx.isVar())
      assert(vx.name === x)

      val vy = eta.baseTerm().asStructured().get().subterm(2).get().asVar().get()
      assert(vy.isVar())
      assert(vy.name === y)

      val varSubstitution = VarSubstitution(vx -> 1, vy -> 2)

      val r = eta.substVars(varSubstitution, Map.empty)

      val rx = r.asEta().get().baseTerm().asStructured().get().subterm(1).get()

      assert(rx.isPrimitive())
      assert(rx.asPrimitive().!.value == 1)

      val ry = r.asEta().!.baseTerm().asStructured().!.subterm(2).!
      assert(ry.isPrimitive())
      assert(ry.asPrimitive().!.value == 2)

  }

  test("substitute vars in simple arrow term \\eta x=>*, y->1:(x->y)") {
      val x = StringName("x")
      val y = StringName("y")
      val nc = NameSubstitution(
          x -> IPatternCondition.all,
          y -> 1,
      )
      val arrow = Arrow(x,y,EmptyArrows)
      val ea = IEtaTerm.create(nc,arrow)
      val (l,r) = ea.baseTerm().asArrows().!.linear()(0)
      assert(l.isVar())
      assert(r.isVar())

  }


}
