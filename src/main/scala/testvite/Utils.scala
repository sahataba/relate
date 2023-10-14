package testvite

def idToString(id: Id): String = id match {
  case id: URI   => s"r-${id.value}"
  case id: Value => s"v-${id.value}"
}
def stringToId(s: String): Id = {
  val parts = s.split("-")
  parts(0) match {
    case "r" => URI(parts(1))
    case "v" => Value(parts(1))
  }
}
def relationIdToString(id: URI): String = s"r-${id.value}"
def stringToRelationId(s: String): URI = {
  val parts = s.split("-")
  parts(0) match {
    case "r" => URI(parts(1))
    case "v" => throw Exception("not a relation id")
  }
}
def valueIdToString(id: Value): String = s"v-${id.value}"
def stringToValueId(s: String): Value = {
  val parts = s.split("-")
  parts(0) match {
    case "v" => Value(parts(1))
    case "r" => throw Exception("not a value id")
  }
}
