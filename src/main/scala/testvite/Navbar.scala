package testvite

import com.raquo.laminar.api.L._
import testvite.Page._

final case class NavBar() extends Component {

  def body: Div =
    div(
      textTransform.uppercase,
      display.flex,
      flexDirection.row,
      justifyContent.spaceEvenly,
      fontSize("18px"),
      navLink("Home", HomePage),
      navLink("View", View(Value("v-9"))), // todo
      navLink("Add", Add),
      navLink("Search", Search("")),
      navLink("Database", ViewDatabase)
    )

  private def navLink(
      text: String,
      page: Page
  ): HtmlElement = {
    val $isActive =
      Router.router.currentPageSignal.map { currentPage =>
        currentPage == page
      }
    a(
      cursor.pointer,
      text,
      onClick --> { _ => Router.router.pushState(page) },
      cls <-- $isActive.map(CSS.navLink(_)).map(_.htmlClass)
    )
  }
}
