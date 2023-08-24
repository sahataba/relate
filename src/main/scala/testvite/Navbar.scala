package testvite

import com.raquo.laminar.api.L._
import testvite.Page._

final case class NavBar() extends Component {

  def body: Div =
    div(
      justifyContent.spaceBetween,
      div(
        textTransform.uppercase,
        display.flex,
        flexDirection.row,
        fontSize("18px"),
        navLink("Home", HomePage),
        navLink("Object", ViewObject(1)),
        navLink("Search", Search("aa")),
      )
    )

  private def navLink(
    text: String,
    page: Page
  ): Div = {
    val $isActive =
      Router.router.currentPageSignal.map { currentPage =>
        currentPage == page
      }
    div(
      cursor.pointer,
      text,
      onClick --> { _ =>
        Router.router.pushState(page)
      },
      cls <-- $isActive.map(CSS.navLink(_)).map(_.htmlClass)
    )
  }
}
