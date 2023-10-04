package testvite

case class EditRelation(
    id: URI,
    subject: Option[URI],
    `object`: Option[URI | Value],
    predicate: Option[URI]
) {
  def toRelation: Option[Relation] =
    for {
      subject <- subject
      `object` <- `object`
      predicate <- predicate
    } yield Relation(id, subject, `object`, predicate)//todo fix
}

case class SelectedRelation(
  relationId: URI,
  position: "SetPredicate" | "ExtractObjectSetPredicate",
)