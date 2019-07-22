package termware.util

import java.io.PrintWriter

import termware.etaCalculus.TermKindTransformer

abstract class PrettyPrint(writer: PrintWriter) extends TermKindTransformer[Unit] {

  var level: Int

}
