package com.zx80live.gofp.bootstrap.refactored.types

case class ListType(underlined: Type) extends MonadType {
  override def raw: String = s"${underlined.view}List"

  override def view: String = raw

  override def declaration: String =
    s"""
       |type $raw struct {
       |  head *${underlined.raw}
       |  tail *${raw}
       |}""".stripMargin

  override def consView: String = s"Make$raw"

  def nilName: String = s"Nil$view"

  def nilDeclaration: String =
    s"""
       |var $nilName $raw = $raw { nil, nil }""".stripMargin

  def funcIsEmpty: String =
    s"""
       |func (l $raw) IsEmpty() bool { return l == $nilName }""".stripMargin

  def funcNonEmpty: String =
    s"""
       |func (l $raw) NonEmpty() bool { return !l.IsEmpty() }""".stripMargin
}

object ListType {
  val underlinedTypes: Seq[Type] = OptionType.underlinedTypes ++ OptionType.underlinedTypes.map(ListType.apply)

  val types: Seq[ListType] = underlinedTypes.map(ListType.apply)

  val declarations: Seq[String] = types.map(_.declaration)

  val nilDeclarations: Seq[String] = types.map(_.nilDeclaration)

  val functionsIsEmpty: Seq[String] = types.map(_.funcIsEmpty)
  val functionsNonEmpty: Seq[String] = types.map(_.funcNonEmpty)
}
