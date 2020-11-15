package com.zx80live.gofp.bootstrap.refactored

case class ListType(underlined: Type) extends Type {
  override def raw: String = s"${underlined.view}List"

  override def view: String = raw

  override def declare: String =
    s"""
       |type $raw struct {
       |  head *${underlined.raw}
       |  tail *${raw}
       |}""".stripMargin

  override def consView: String = s"Make$raw"
}

object ListType {
  val underlinedTypes: Seq[Type] = OptionType.underlinedTypes ++ OptionType.underlinedTypes.map(ListType.apply)

  val types: Seq[Type] = underlinedTypes.map(ListType.apply)
}
