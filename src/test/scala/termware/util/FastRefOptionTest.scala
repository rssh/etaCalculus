package termware.util

import org.scalatest.FunSuite
import termware.etaCalculus.{ITerm, StringName}

class FastRefOptionTest extends FunSuite {

  test("FastRefOptionEmpty should be empty") {
    val optterm: FastRefOption[ITerm] = FastRefOption.empty
    assert(optterm.isEmpty)
    assert(!optterm.isDefined)
    optterm match {
      case FastRefOption.Some(x) => assert(false)
      case FastRefOption.Empty(x) => // all ok
    }
  }

  test("FastRefOptionFull should be full") {
    val optterm: FastRefOption[ITerm] = FastRefOption(StringName("A"))
    assert(!optterm.isEmpty)
    assert(optterm.isDefined)
    optterm match {
      case FastRefOption.Some(x) => assert(x.asName().get().valueString == "A")
      case FastRefOption.Empty(_) => assert(false)
    }
  }


}
