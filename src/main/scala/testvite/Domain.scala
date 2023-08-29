package testvite

type RelationId = String
case class Relation(id: RelationId, from: Id, to: Id, kind: "is a" | "has a" = "has a")
type Relations = Set[Relation]
type References = Set[Relation]
type Value = String
type ValueId = Int
type Id = RelationId | ValueId
case class Entity(id: Id, value: Value, relations: Relations, references: References)
//relation has a id
//relation can reference relation or a value
//entities are groups of relations

//values are separate