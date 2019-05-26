package termware.etaCalculus

import org.scalatest.FunSuite

class StructuredConstractionText extends FunSuite {

  test("create structured terms") {
    val st1 = PlainStructured.freeIndexed("a",1,2,3)
    val st2 = PlainStructured.freeIndexed("a",1,2,3)
    assert(st1 == st2)
    assert(st1.hashCode() == st2.hashCode())
  }


}
