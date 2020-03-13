package simplerouter

import java.net.URLEncoder


private object Compat extends CompatBase {
  val encodeURIComponent: String => String = URLEncoder.encode(_: String, "UTF-8")
}
