package testvite

import scala.scalajs.js
import com.raquo.laminar.api.L._
import typings.d3.mod as d3Mod
import typings.d3Hierarchy as d3Hierarchy
import typings.d3.global.d3 as d3
import typings.std.{CanvasRenderingContext2D, FrameRequestCallback, HTMLCanvasElement, stdStrings}
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportAll
import scala.scalajs.js.annotation.JSExportTopLevel
import upickle.default.{ReadWriter => RW, macroRW}
import upickle.default._

case class Node(name: String, children: List[Node])
object Node{
  implicit val rw: RW[Node] = macroRW
}
val data = Node("Eve", List(
  Node("Cain", List()),
  Node("Seth", List(
    Node("Enos", List()),
    Node("Noam", List())
  )),
  Node("Abel", List()),
  Node("Awan", List(
    Node("Enoch", List())
  )),
  Node("Azura", List())
))
final case class Graph() extends Component {
  val d = write(data)
  val g = d3Mod.hierarchy(js.JSON.parse(d))
  pprint.pprintln(g)
  //println(g.children.map(_.map(pprint(_))))
  println(g.descendants().map(_.x))
  pprint.pprintln(g.leaves().map(_.x))
  val g2 = d3Mod.select("body").append("svg").attr("width", "100%").attr("height", "450px")
  val s = svg.svg(
        svg.idAttr := "graph",
        svg.height := "800",
        svg.width := "500")
  def body: Div =
    div(
      h3("Graphh"),
      //s,
      //svg(g2.enter().append("circle").attr("r", 2.5)),
    )
}