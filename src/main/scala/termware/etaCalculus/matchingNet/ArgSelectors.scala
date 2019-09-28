package termware.etaCalculus.matchingNet

import termware.etaCalculus.{ILeftUnificable, IName}
import termware.util.FastRefOption

import scala.collection.immutable.IntMap

sealed trait ArgSelectors


case class ByPosArgSelector(pos:Int, check: ILeftUnificable, next: FastRefOption[ArgSelectors])
