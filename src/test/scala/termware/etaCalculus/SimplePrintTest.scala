package termware.etaCalculus

import org.scalatest.FunSuite
import termware.NameSubstitution
import termware.util.SimplePrint

class SimplePrintTest extends FunSuite {

  test("name should be printed as name") {
    val s = StringName("s")
    val printed = SimplePrint(s)
    assert(printed == "s")
  }

  test("eta should not include SimpeName") {
    val p = StringName("p")
    val q = StringName("q")
    val f = StringName("f")
    val eta = IEtaTerm.create(NameSubstitution(p -> 1, q -> "q"), f(p, q))
    val printed = SimplePrint(eta)
    assert(!printed.contains("StringName"))
  }

}