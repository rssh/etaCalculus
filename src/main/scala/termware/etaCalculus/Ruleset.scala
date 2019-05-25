package termware.etaCalculus

import scala.language.higherKinds

trait ITermTransformer {

  def add(arrow: Arrow): ITermTransformer

  def apply[F[_]](x:ITerm): F[ITerm]

}
