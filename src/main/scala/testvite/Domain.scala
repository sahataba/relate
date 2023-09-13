package testvite

import zio.json._

type Concept = String
type Concepts = List[Concept] //we can store
case class RelationId(value: Int)
case class Relation(id: RelationId, subject: Id, `object`: Id, predicate: Concept = "has a")
case class EditRelation(id: RelationId, subject: Id, `object`: Option[Id], predicate: Concept = "has a")
type Relations = Set[Relation]
type References = Set[Relation]
type Value = String
case class ValueId(value: Int)
type Id = RelationId | ValueId
case class Entity(id: Id, value: Value, relations: Relations, references: References)

def idToString(id: Id): String = id match {
  case id: RelationId => s"r-${id.value}"
  case id: ValueId => s"v-${id.value}"
}
def stringToId(s: String): Id = {
  val parts = s.split("-")
  parts(0) match {
    case "r" => RelationId(parts(1).toInt)
    case "v" => ValueId(parts(1).toInt)
  }
}
def relationIdToString(id: RelationId): String = s"r-${id.value}"
def stringToRelationId(s: String): RelationId = {
  val parts = s.split("-")
  parts(0) match {
    case "r" => RelationId(parts(1).toInt)
    case "v" => throw Exception("not a relation id")
  }
}
def valueIdToString(id: ValueId): String = s"v-${id.value}"
def stringToValueId(s: String): ValueId = {
  val parts = s.split("-")
  parts(0) match {
    case "v" => ValueId(parts(1).toInt)
    case "r" => throw Exception("not a value id")
  }
}

object RelationId:
  given JsonDecoder[RelationId] = JsonDecoder[String].map(stringToRelationId)
  given JsonEncoder[RelationId] = JsonEncoder[String].contramap(relationIdToString)

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