package termware.util

import termware.etaCalculus.{IArrows, IErrorTerm, IEtaTerm, IName, IPatternCondition, IPrimitive, IStructured, ITerm, IVarTerm, TermKindTransformer, VarSubstitution}

class SimplePrint(sb: StringBuilder) extends TermKindTransformer[Unit] {

  override def onName(name: IName, vo: Map[IEtaTerm, IEtaTerm]): Unit = {
    sb.append(name.value.toString)
  }

  override def onVar(varTerm: IVarTerm, vo: Map[IEtaTerm, IEtaTerm]): Unit = {

    sb.append("v[").append(varTerm.owner.hashCode().toHexString)
      .append(":")
      .append(varTerm.name.valueString).append("]")
  }

  override def onPrimitive(primitive: IPrimitive, vo: Map[IEtaTerm, IEtaTerm]): Unit = {
    sb.append(primitive.value.toString)
  }

  override def onStructured(structured: IStructured, vo: Map[IEtaTerm, IEtaTerm]): Unit = {
    sb.append(structured.name.value.toString)
      .append("(")
    structured.foldSubtermsWhile(0){ (s,e) =>
      structured.subterm(s) match {
        case FastRefOption.Some(t) =>
          if (s != 0) sb.append(",")
          t.kindTransform(this,vo)
        case  FastRefOption.Empty() =>
      }
      s+1
    }(_ < structured.arity() )
    sb.append(")")
  }

  override def onEta(eta: IEtaTerm, vo: Map[IEtaTerm, IEtaTerm]): Unit = {
    sb.append("\\eta[")
    sb.append(eta.hashCode().toHexString)
    sb.append("]")
    var frs = true
    eta.context().foreach{ (n, v) =>
       if (frs) {
         frs = false
       } else {
         sb.append(", ")
       }
       sb.append(n.value.toString)
        .append("->")
        .append("")
        v.kindTransform(this,vo)
    }
    sb.append(":")
    eta.baseTerm().kindTransform(this,vo)
  }

  override def onError(error: IErrorTerm, vo: Map[IEtaTerm, IEtaTerm]): Unit = {
    sb.append("ERROR:[")
      .append(error.shortMessage())
      .append("]")
  }

  override def onPatternCondition(patternCondition: IPatternCondition, vo: Map[IEtaTerm, IEtaTerm]): Unit = {
    sb.append("*(")
    patternCondition.expression.kindTransform(this,vo)
    sb.append(")")
  }

  override def onArrows(arrows: IArrows, vo: Map[IEtaTerm, IEtaTerm]): Unit = {
    val linear = arrows.linear()
    sb.append("[")
    var frs = true
    linear.foreach { case (x,y) =>
      if (!frs) {
        sb.append("|")
      } else frs = false
      x.kindTransform(this,vo)
      sb.append("->")
      y.kindTransform(this,vo)
    }
    sb.append("]")
  }

}

object SimplePrint  {

  def apply(t:ITerm): String = {
    val sb = new StringBuilder()
    val sp = new SimplePrint(sb)
    t.kindTransform(sp,Map.empty)
    sb.toString()
  }

  def apply(s: VarSubstitution): String = {
    val vars = s.map( (v,t) => SimplePrint(v) + "->" + SimplePrint(t) )
    s"""{ ${vars.mkString(" , ")} }"""
  }

}
