package termware.etaCalculus

trait Contradiction {

  def message: String

  def longMessage: String

  def operation: String

  def args: Seq[ITerm]

}
