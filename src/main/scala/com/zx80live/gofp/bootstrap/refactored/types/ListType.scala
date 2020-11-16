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

  def funcReverse: String =
    s"""
       |func (l $raw) Reverse() $raw {
       |  acc := $nilName
       |  xs := l
       |  for xs.NonEmpty() {
       |    acc = acc.Cons(*xs.head)
       |    xs = *xs.tail
       |  }
       |  return acc
       |}""".stripMargin

  def funcCopy: String =
    s"""
       |func (l $raw) Copy() $raw {
       |  acc := $nilName
       |  xs := l
       |  for xs.NonEmpty() {
       |    acc = acc.Cons(*xs.head)
       |    xs = *xs.tail
       |  }
       |  return acc.Reverse()
       |}""".stripMargin

  def funcSize: String =
    s"""
       |func (l $raw) Size() int {
       |  count := 0
       |  xs := l
       |  for xs.NonEmpty() {
       |    count ++
       |    xs = *xs.tail
       |  }
       |  return count
       |}
       |""".stripMargin
}

object ListType {
  def underlinedTypes: Seq[Type] = OptionType.underlinedTypes ++ OptionType.underlinedTypes.map(ListType.apply)

  def types: Seq[ListType] = underlinedTypes.map(ListType.apply)

  def declarations: Seq[String] = types.map(_.declaration)

  def nilDeclarations: Seq[String] = types.map(_.nilDeclaration)

  def functionsPrepend: Seq[String] = types.map(_.funcPrepend)
  def functionsCons: Seq[String] = types.map(_.funcCons)
  def functionsIsEmpty: Seq[String] = types.map(_.funcIsEmpty)
  def functionsNonEmpty: Seq[String] = types.map(_.funcNonEmpty)
  def functionsForeach: Seq[String] = types.map(_.funcForeach)
  def functionsReverse: Seq[String] = types.map(_.funcReverse)
  def functionsCopy: Seq[String] = types.map(_.funcCopy)
  def functionsFilter: Seq[String] = types.map(_.funcFilter)
  def functionsSize: Seq[String] = types.map(_.funcSize)
}
