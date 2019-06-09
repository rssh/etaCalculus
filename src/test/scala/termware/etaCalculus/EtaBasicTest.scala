package termware.etaCalculus

import org.scalatest.FunSuite
import termware.util.FastRefOption

class EtaBasicTest extends FunSuite {

  test("create basic eta-term") {
      val st1 = IEtaTerm.create(
        Substitution.namedTerms("p" -> 1,"q" -> 2),
        IStructured.freeIndexed("f",StringName("p"),StringName("q"))
      )
      st1.baseTerm().asStructured() match {
        case FastRefOption.Empty() => assert(false,"base term should be structured")
        case FastRefOption.Some(x) =>
           assert(x.subterm(0).isDefined)
           assert(x.subterm(0).value.isVar())
           x.subterm(0).flatMap(_.asVar()) match {
             case FastRefOption.Empty() => assert(false,"f(p,q).subterm(0) should be var")
             case FastRefOption.Some(y) =>
               assert(y.owner eq st1)
           }
      }
  }


}
