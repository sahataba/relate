package testvite

import scalacss.DevDefaults._

object CSS extends StyleSheet.Inline {
  import dsl._

  val yellowBack = style(
    backgroundColor.yellow,
    fontWeight.bold,
    &.hover -
      cursor.zoomIn,
  )

  val navLink = styleF.bool(ok => styleS(
    if (ok) textDecorationLine.underline else textDecorationLine.none,
    display.flex,
    flexDirection.column,
    fontSize(18.px),
    textTransform.uppercase,
    cursor.pointer,
    &.hover -
      fontWeight.bold,
  ))
}