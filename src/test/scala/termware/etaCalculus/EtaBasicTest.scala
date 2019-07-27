package termware.etaCalculus

import org.scalatest.FunSuite
import termware.etaCalculus.algo.AllSubterms
import termware.util.{FastRefOption, SimplePrint}

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

  test("create deeper eta-term") {
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


  test("subst-var in new eta-term") {
    // st1 = f(p,q)
    val st1 = IStructured.freeIndexed("f",StringName("p"),StringName("q") )

    // st2 = \eta p->2, q->4 : f(p,q)
    val st2 = IEtaTerm.create(
      Substitution.namedTerms("p" -> 2,"q" -> 4),
      IStructured.freeIndexed("f",StringName("p"),StringName("q"))
    )

    // st2vp = var(st2,p)
    val st2vP = TermOps.subterm(st2,0).get()
    assert(st2vP.isVar(),s"var 'p' expected, ${}")

    // st3 = \eta p->3, q->5 : f(p,q,f(p,q),st2)
    val st3 = IEtaTerm.create(
      Substitution.namedTerms("p" -> 3,"q" -> 5),
      IStructured.freeIndexed("f",StringName("p"), StringName("q"), st1, st2 )
    )

    //System.out.println("st3="+SimplePrint(st3))


    // s = st2.p -> st3.q
    val st2InSt3 = TermOps.subterm(st3,3).get().asEta().get()
    val s = MapBasedVarSubstitution(Map(
        st2InSt3 -> Map( StringName("p") -> PlainVarTerm(st3,StringName("q")) )
    ))
    //System.out.println(s"s=${SimplePrint(s)}")

    // st4 = st3.subst(s) = \eta p->3, q-> 5 f(p,q,f(p,q),\eta p'->2, q->5 f(p,q))
    val st4 = st3.substVars(s,Map())

    //System.out.println("st4="+SimplePrint(st4))

    // check, that st4 not contains reference

    val c = AllSubterms(st4).forall{ x =>
      x match {
        case IVarTerm(v) =>
          ! ((v.owner eq st3) || (v.owner eq st2))
        case _ => true
      }
    }

    assert(c, "all var changed")

    val newSt2 = TermOps.subterm(st4,3).get()
    val newQ = TermOps.subterm(newSt2,0).get()

    //System.out.println("newSt2:"+SimplePrint(newSt2))
    //System.out.println("newQ:"+SimplePrint(newQ))

    newQ match {
      case IVarTerm(vq)=>
        assert(vq.name.valueString === "q")
        assert(vq.owner eq st4)
      case _ =>
        assert(false,s"should be var, have $newQ")
    }

    val oldQ = TermOps.subterm(newSt2,1).get()
    oldQ match {
      case IVarTerm(vq) =>
        assert(vq.name.valueString === "q")
        assert(vq.owner eq newSt2)
      case _ =>
        assert(false,s"should be var, have $oldQ")
    }

  }


}
