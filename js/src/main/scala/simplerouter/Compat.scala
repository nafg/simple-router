package simplerouter

import scala.scalajs.js.URIUtils


private object Compat extends CompatBase {
  def encodeURIComponent: String => String = URIUtils.encodeURIComponent
}
