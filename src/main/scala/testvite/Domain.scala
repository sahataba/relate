package testvite

type Relation = (Id, Id)
type Relations = Set[Relation]
type Value = String
type Id = Int
case class Entity(id: Id, value: Value, relations: Relations)