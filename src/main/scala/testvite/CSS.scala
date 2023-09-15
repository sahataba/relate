package testvite

import scalacss.DevDefaults._

object CSS extends StyleSheet.Inline {
  import dsl._

  val ff = fontFace("appFont")(_
    .src("local(Avenir)")
    .fontStretch.ultraCondensed
    .fontWeight._200)


  val app = style(
    fontFamily(ff),
    //`-webkit-font-smoothing`: antialiased,
    //`-moz-osx-font-smoothing`: grayscale,
    textAlign.center,
    color(c"rgb(245, 135, 31)"),
    marginTop(60.px),
  )

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