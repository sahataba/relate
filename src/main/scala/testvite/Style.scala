package testvite

import com.raquo.laminar.api.L._

val blue = "#0072c6"

val roundedBorder = List(
  border := s"1px solid ${blue}",
  borderRadius := "1em",
  paddingLeft := "1em",
  paddingRight := "1em"
)

val aLink = List(
  textDecoration := "underline",
  cursor := "pointer"
)

val simpleInline =
  List(
    display.flex,
    flexDirection.row,
    alignItems.center
  )
