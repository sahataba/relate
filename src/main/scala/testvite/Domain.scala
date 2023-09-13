package testvite

import zio.json._

type Concept = String
type Concepts = List[Concept] //we can store
case class URI(value: String)
case class Relation(subject: URI, `object`: URI | Value, predicate: URI)
case class EditRelation(subject: Option[URI], `object`: Option[URI | Value], predicate: Option[URI])
type Relations = Set[Relation]
type References = Set[Relation]
case class Value(value: String)
type Id = URI | Value
case class Entity(id: URI | Value, relations: Relations, references: References)

def idToString(id: Id): String = id match {
  case id: URI => s"r-${id.value}"
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

object URI:
  given JsonDecoder[URI] = JsonDecoder[String].map(stringToRelationId)
  given JsonEncoder[URI] = JsonEncoder[String].contramap(relationIdToString)

object Id:
  given JsonDecoder[Id] = JsonDecoder[String].map(stringToId)
  given JsonEncoder[Id] = JsonEncoder[String].contramap(idToString)


//adding values
//add relations to select
//think about queries
//view relation
//more correct initial example


//relation has a id
//relation can reference relation or a value
//entities are groups of relations

//values are separate

//creating a new relation
//displaying a relation with <a> links
//nesting
//displaying ids:     |id|       (long path(sentence)) 




// from to =!= kind  + orientation
//michas has a musician =!= musician is a michael

//vocabulary
//entity === set of relations over time === instance of object === object
//concept === attribute === property === relation kind

//more common words for programmer are: object, attribute, property, relation, type
//words for end user: thing
//     (value born)
// 2 is an concept "born" which is a entity
//1 ->       2       -> 3
//1 ->       "born"       -> 4
//bprn is kind of relation


//concept is an entity that doesnt change over time like born