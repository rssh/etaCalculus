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
      case FastRefOption.Empty() => // all ok
    }
  }

  test("FastRefOptionFull should be full") {
    val optterm: FastRefOption[ITerm] = FastRefOption(StringName("A"))
    assert(!optterm.isEmpty)
    assert(optterm.isDefined)
    optterm match {
      case FastRefOption.Some(x) => assert(x.asName().get().valueString == "A")
      case FastRefOption.Empty() => assert(false)
    }
  }

  // https://github.com/scala/bug/issues/7396
  test("FastRefOption should have hashcode [scalac https://github.com/scala/bug/issues/7396]") {
    pending
    val optterm: FastRefOption[ITerm] = FastRefOption.empty
    val h = optterm.hashCode()
    assert(h != 0)
  }

}
