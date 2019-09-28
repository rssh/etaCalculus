package termware.etaCalculus


trait TCEffect[T] extends TCTerm[T] {

  def name(t:T): IName


}

