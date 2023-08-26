package testvite

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.`import`
import scala.scalajs.js.annotation.JSImport

import com.raquo.laminar.api.L.{*, given}

import org.scalajs.dom
import scalacss.DevDefaults._

object Main {
  def main(args: Array[String]): Unit = {
    windowEvents(_.onLoad).foreach { _ =>
      val sty = styleTag(CSS.render[String])
      dom.document.querySelector("head").appendChild(sty.ref)
      val containerNode = dom.document.querySelector("#app")
      render(
        containerNode,
        app(),
      )
    }(unsafeWindowOwner)
  }
}
