package testvite

import com.raquo.laminar.api.L.{*}

import org.scalajs.dom
import scalacss.DevDefaults._

object Main {
  def main(args: Array[String]): Unit = {
    windowEvents(_.onLoad).foreach { _ =>
      println(CSS.render)
      val sty = styleTag(CSS.render[String])
      dom.document.querySelector("head").appendChild(sty.ref)
      val containerNode = dom.document.querySelector("#app")
      render(
        containerNode,
        app()
      )
    }(unsafeWindowOwner)
  }
}
