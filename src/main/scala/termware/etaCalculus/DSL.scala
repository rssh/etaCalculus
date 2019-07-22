package termware.etaCalculus

import com.sun.tools.javac.util.Pair
import termware.NameSubstitution

trait DSL {

  def name(value: String): StringName = StringName(value)

  def eta(nvs: (IName,ITerm)* )(v: ITerm): IEtaTerm = {
    IEtaTerm.create(NameSubstitution(nvs: _*),v)
  }

}


object DSL extends DSL {

}