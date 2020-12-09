package com.zx80live.gofp.bootstrap.types

import com.zx80live.gofp.bootstrap.functions.{FuncEquals, FuncToString}

case class OptionType(override val underlined: Type) extends MonadType {

  override def raw: String = s"${underlined.view}Option"

  override def emptyName: String = s"None${underlined.view}"

  override def alias: String = view

  override def view: String = raw

  override def declaration: String =
    s"""
       |type $raw struct { value *${underlined.raw} }""".stripMargin

  override def consView: String = s"Make${underlined.view}Option"

  override def emptyDeclaration: String =
    s"""
       |var $emptyName $raw = $raw { nil }""".stripMargin

  override def funcCons: String =
    s"""
       |func $consView(e ${underlined.raw}) $raw { return $raw { &e } }""".stripMargin

  def funcShortCons: String =
    s"""
       |func ${underlined.view}Opt(e ${underlined.raw}) $raw { return $raw { &e } }""".stripMargin

  def funcIsDefined: String =
    s"""
       |func (o $raw) IsDefined() bool { return o != $emptyName }
       |""".stripMargin

  def funcIsEmpty: String =
    s"""
       |func (o $raw) IsEmpty() bool { return o == $emptyName }
       |""".stripMargin

  def funcForeach: String =
    s"""
       |func (o $raw) Foreach(f func(${underlined.raw})) { if o.IsDefined() { f(*o.value) } }""".stripMargin

  def funcHead: String =
    s"""
       |func (o $raw) Head() ${underlined.raw} { if o.IsDefined() { return *o.head } else { panic("can't get head from None") } }""".stripMargin

  def funcHeadOption: String =
    s"""
       |func (o $raw) HeadOption() $raw { return o }""".stripMargin

  def funcTail: String =
    s"""
       |func (m $raw) ${ListType(underlined)} { return m.ToList().Tail() }""".stripMargin

  override def funcFilter: String =
    s"""
       |func (m $raw) Filter(p ${Predicate.name(underlined)}) $raw { if m.IsDefined() && p(*m.value) { return m } else { return $emptyName } }""".stripMargin

  override def funcMap(out: Type): String = {
    val o = OptionType(out)
    s"""
       |func (m $raw) Map${out.view}(f func(${underlined.raw}) ${out.raw}) ${o.raw} {
       |  if m.IsDefined() { return ${o.consView}(f(*m.value)) } else { return ${o.emptyName} } }""".stripMargin
  }

  def funcDrop: String =
    s"""
       |func (m $raw) Drop(i int) ${ListType(underlined)} { return m.ToList().Drop(i) }""".stripMargin

  override def funcToList: String = {
    val l = ListType(underlined)
    s"""
       |func (m $raw) ToList() ${l.raw} { if m.IsDefined() { return ${l.consView}(*m.head) } else { return ${l.emptyName} } }""".stripMargin
  }

  override def funcEquals: String =
    s"""
       |func (a $raw) Equals(b $raw) bool {
       |  if a.IsDefined() {
       |    if b.IsDefined() {
       |      return ${FuncEquals.name(underlined)}(${underlined.alias}(*a.value), ${underlined.alias}(*b.value))
       |    } else { return false}
       |  } else if b.IsDefined() { return false } else { return true } }""".stripMargin


  override def funcToString: String =
    s"""
       |func (o $raw) ToString() String { if o.IsDefined() { return String(fmt.Sprintf("Some(%v)", ${FuncToString.name(underlined)}(${underlined.alias}(*o.value)))) } else { return "None" } }""".stripMargin

  override def setUnderlined(t: Type): MonadType = OptionType(t)

  override def funcFlatMap(out: MonadType): String = out match {
    case o: OptionType =>
      s"""
         |func (m $raw) FlatMap${out.underlined.view}(f func(${underlined.raw}) ${out.raw}) ${out.raw} {
         |  if m.IsDefined() { return f(*m.value) } else { return ${o.emptyName} }
         |}""".stripMargin
    case _ => ???
  }

  override def funcReduce: String = ""

  override def funcFoldLeft(out: Type): String =
    s"""
       |func (o $raw) FoldLeft${out.view}(z ${out.raw}, f func(${out.raw}, ${underlined.raw}) ${out.raw}) ${out.raw} {
       |  if o.IsDefined() { return f(z, *o.value) } else { return z }}""".stripMargin

  override def funcFind: String = ???

  override def funcZipWithIndex: String = ???

  override def funcZipWith: String = ???

  override def funcZip: String = ???
}

object OptionType {

  def allowedBaseTypes: Seq[Type] = BaseType.reducedTypes ++ Seq(Tuple2Type.defaultType)

  def types: Seq[OptionType] =
    allowedBaseTypes.map(OptionType.apply) ++ // Option[T]
      allowedBaseTypes.map(OptionType.apply).map(OptionType.apply) ++ // Option[Option[T]]
      allowedBaseTypes.map(ArrayType.apply).map(OptionType.apply) ++ // Option[Array[T]]
      allowedBaseTypes.map(ListType.apply).map(OptionType.apply) // Option[List[T]]

  def transformers: Seq[Transformer] = {
    val inTypes = types.map(_.underlined).distinct
    val outTypes = inTypes
    for {
      in <- inTypes
      out <- outTypes
    } yield Transformer(in, out)
  }

  def declarations: Seq[String] = types.map(_.declaration)

  def emptyDeclarations: Seq[String] = types.map(_.emptyDeclaration)

  def functionsCons: Seq[String] = types.map(_.funcCons)

  def functionsShortCons: Seq[String] = types.map(_.funcShortCons)

  def functionsIsDefined: Seq[String] = types.map(_.funcIsDefined)

  def functionsIsEmpty: Seq[String] = types.map(_.funcIsEmpty)

  def functionsEquals: Seq[String] = types.map(_.funcEquals)

  def functionsForeach: Seq[String] = types.map(_.funcForeach)

  def functionsFilter: Seq[String] = types.map(_.funcFilter)

  def functionsMap: Seq[String] = transformers.map(t => OptionType(t.in).funcMap(t.out))

  def functionsToString: Seq[String] = types.map(_.funcToString)

  def functionsFlatMap: Seq[String] = {
    val outTypes = allowedBaseTypes.map(OptionType.apply)
    for {
      o1 <- types
      o2 <- outTypes
    } yield o1.funcFlatMap(o2)
  }

  def functionsFlatten: Seq[String] = {
    val inTypes = allowedBaseTypes.map(OptionType.apply).map(OptionType.apply)
    inTypes.map(_.funcFlatten)
  }

  def functionsFoldLeft: Seq[String] = {
    val inTypes = allowedBaseTypes.map(OptionType.apply)
    val outTypes = allowedBaseTypes
    for {
      in <- inTypes
      out <- outTypes
    } yield in.funcFoldLeft(out)
  }
}