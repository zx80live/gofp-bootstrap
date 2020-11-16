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

  def funcPrepend: String =
    s"""
       |func (l $raw) Cons(e ${underlined.raw}) $raw { return $raw { &e, &l } }""".stripMargin

  override def funcCons: String =
    s"""
       |func $consView(elements ...${underlined.raw}) $raw {
       |  xs := $nilName
       |  for _, e := range elements {
       |    xs = xs.Cons(e)
       |  }
       |  return xs
       |}""".stripMargin

  def funcIsEmpty: String =
    s"""
       |func (l $raw) IsEmpty() bool { return l == $nilName }""".stripMargin

  def funcNonEmpty: String =
    s"""
       |func (l $raw) NonEmpty() bool { return !l.IsEmpty() }""".stripMargin

  override def funcForeach: String =
    s"""
       |func (l $raw) Foreach(f func(${underlined.raw})) {
       |  xs := l
       |  for xs.NonEmpty() {
       |    f(*xs.head)
       |    xs = *xs.tail
       |  }
       |}""".stripMargin
}

object ListType {
  val underlinedTypes: Seq[Type] = OptionType.underlinedTypes ++ OptionType.underlinedTypes.map(ListType.apply)

  val types: Seq[ListType] = underlinedTypes.map(ListType.apply)

  val declarations: Seq[String] = types.map(_.declaration)

  val nilDeclarations: Seq[String] = types.map(_.nilDeclaration)

  val functionsPrepend: Seq[String] = types.map(_.funcPrepend)
  val functionsCons: Seq[String] = types.map(_.funcCons)
  val functionsIsEmpty: Seq[String] = types.map(_.funcIsEmpty)
  val functionsNonEmpty: Seq[String] = types.map(_.funcNonEmpty)
  val functionsForeach: Seq[String] = types.map(_.funcForeach)
}
