package testvite

val entities = Map(
  1 -> "one",
  2 -> "two",
  3 -> "three",
  4 -> "four",
)

val relations = Set(
  Relation(1, 2),
  Relation(1, 3),
  Relation(2, 3),
  Relation(3, 1),
)

case class Database(private val entities: Map[Id, Value], private val relations: Set[Relation]):
  def search(query: String): List[Entity] =
    entities
      .filter((id, value) => value.contains(query))
      .map((id, value) => Entity(
        id,
        value,
        relations.filter(r => r.from == id).toSet,
        relations.filter(r => r.to == id).toSet,
      )).toList
  def get(id: Id): Option[Entity] = entities.get(id).map(value => Entity(
        id,
        value,
        relations.filter(r => r.from == id).toSet,
        relations.filter(r => r.to == id).toSet,
      ))

object Database:
  val dummy = Database(entities, relations)