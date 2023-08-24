package testvite

val entities = Map(
  1 -> Entity(1, "one", Set((1, 2), (1, 3))),
  2 -> Entity(2, "two", Set((2, 3))),
  3 -> Entity(3, "three", Set((3, 1))),
)

case class Database(private val entities: Map[Id, Entity]):
  def search(query: String): List[Entity] = entities.values.toList.filter(_.value.contains(query))
  def get(id: Id): Option[Entity] = entities.get(id)

object Database:
  val dummy = Database(entities)