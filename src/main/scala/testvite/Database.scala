package testvite

val entities = Map(
  1 -> Entity(
    1,
    "one",
    Set(Relation(1, 2), Relation(1, 3)),
    Set()
  ),
  2 -> Entity(
    2,
    "two",
    Set(Relation(2, 3)),
    Set(),
  ),
  3 -> Entity(
    3,
    "three",
    Set(Relation(3, 1)),
    Set()
  ),
)

case class Database(private val entities: Map[Id, Entity]):
  def search(query: String): List[Entity] = entities.values.toList.filter(_.value.contains(query))
  def get(id: Id): Option[Entity] = entities.get(id)

object Database:
  val dummy = Database(entities)