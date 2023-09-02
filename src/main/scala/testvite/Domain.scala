package testvite

type Concept = String
type Concepts = List[Concept] //we can store
case class RelationId(value: Int, kind: "r" = "r")
case class Relation(id: RelationId, from: Id, to: Id, kind: Concept = "has a")
case class EditRelation(id: RelationId, from: Id, to: Option[Id], kind: Concept = "has a")
type Relations = Set[Relation]
type References = Set[Relation]
type Value = String
case class ValueId(value: Int)
type Id = RelationId | ValueId
case class Entity(id: Id, value: Value, relations: Relations, references: References)
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