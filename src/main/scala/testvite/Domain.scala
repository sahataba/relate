package testvite

case class Relation(from: Id, to: Id)
type Relations = Set[Relation]
type References = Set[Relation]
type Value = String
type Id = Int
case class Entity(id: Id, value: Value, relations: Relations, references: References)