package termware.etaCalculus

import org.scalatest.FunSuite

class StructuredConstractionText extends FunSuite {

  test("create same structured terms") {
    val st1 = PlainStructured.freeIndexed("a",1,2,3)
    val st2 = PlainStructured.freeIndexed("a",1,2,3)
    assert(st1 == st2)
    assert(st1.hashCode() == st2.hashCode())
  }

  test("create different structured terms") {
    val st1 = PlainStructured.freeIndexed("a",1,2,3)
    val st2 = PlainStructured.freeIndexed("a",1,2,3,4)
    assert(st1 != st2)
  }

  test("left unify constant terms success") {
    val st1 = PlainStructured.freeIndexed("a",1,2,3)
    val st2 = PlainStructured.freeIndexed("a",1,2,3,4)
    val r = st1.leftUnifyInSubst(MapBasedVarSubstitution.empty,st2)
    assert(r.isSuccess())
  }

  test("left unify constant terms failure") {
    val st1 = PlainStructured.freeIndexed("a",1,3,3)
    val st2 = PlainStructured.freeIndexed("a",1,2,3,4)
    val r = st1.leftUnifyInSubst(MapBasedVarSubstitution.empty,st2)
    assert(r.isFailure())
  }


}
