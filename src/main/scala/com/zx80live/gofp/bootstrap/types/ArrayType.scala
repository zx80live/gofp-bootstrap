package com.zx80live.gofp.bootstrap.types

case class ArrayType(override val underlined: Type) extends MonadType {

  override def raw: String = s"[]${underlined.raw}"

  override def emptyName: String = ""

  override def view: String = s"${underlined.view}Array"

  override def alias: String = raw

  override def declaration: String = ""

  override def consView: String = ""

  override def funcCons: String = ""

  override def emptyDeclaration: String = ""

  override def funcHead: String =
    s"""
       |func ${view}Head(m $raw) ${underlined.raw} { if len(m) > 0 { return m[0] } else { panic("can't get head from empty $raw slice") } }""".stripMargin

  override def funcHeadOption: String = {
    val opt = OptionType(underlined)
    s"""
       |func ${view}HeadOption(m $raw) ${opt.raw} { if len(m) > 0 { return ${opt.consView}(m[0]) } else { return ${opt.emptyName} } }""".stripMargin
  }

  override def funcTail: String =
    s"""
       |func ${view}Tail(m $raw) $raw { s := len(m); if s > 0 { return m[1:s-1] } else {return []${underlined.raw}{} } }""".stripMargin

  override def funcSize: String =
    s"""
       |func ${view}Size(m $raw) int { return len(m) }
       |""".stripMargin

  override def funcForeach: String =
    s"""
       |func ${view}Foreach(m $raw, f func(${underlined.raw})) { for _, e := range m { f(e) } }""".stripMargin

  override def funcFilter: String =
    s"""
       |func ${view}Filter(m $raw, p ${Predicate(underlined).name}) $raw {
       |  l := len(m)
       |  acc := make($raw, l)
       |  i := 0
       |  for _, e := range m {
       |    if p(e) { acc[i] = e; i ++ }
       |  }
       |  return acc[0:i]
       |}""".stripMargin

  override def funcMap(out: Type): String = {
    val a2 = ArrayType(out)
    s"""
       |func ${view}Map${out.view}(m $raw, f func(${underlined.raw}) ${out.raw}) ${a2.raw} {
       |  l := len(m)
       |  acc := make(${a2.raw}, l)
       |  for i, e := range m {
       |    acc[i] = f(e)
       |  }
       |  return acc
       |}""".stripMargin
  }

  override def funcDrop: String =
    s"""
       |func ${view}Drop(m $raw, i int) $raw {
       |  s := len(m)
       |  if i < 0 || i >= s { panic ("index out of bound") }
       |  if s > 0 { return m[i:s-1] } else { return make([]${underlined.raw}, 0) } }""".stripMargin

  override def funcToList: String = {
    val l = ListType(underlined)
    s"""
       |func ${view}ToList(m $raw) ${l.raw} {
       |  acc := ${l.emptyName}
       |  for _, e := range m {
       |    acc = acc.Cons(e)
       |  }
       |  return acc.Reverse()
       |}""".stripMargin
  }

  override def funcEquals: String = ""

  def funcMkString: String =
    s"""
       |func ${view}MkString(a $raw, start, sep, end string) string {
       |	 content := ""
       |	 for _, e := range a {
       |	   content = fmt.Sprintf("%v%v%v", content, ${underlined.view}ToString(e), sep)
       |	 }
       |	 l := len(content)
       |	 if l > 0 {
       |		 content = content[:l-1]
       |	 }
       |	 return fmt.Sprintf("%v%v%v", start, content, end)
       |}""".stripMargin

  override def funcToString: String = ""

  override def setUnderlined(t: Type): MonadType = ArrayType(t)

  override def funcFlatMap(out: MonadType): String = ""

  override def funcFlatten: String = ""

  override def funcReduce: String = ""

  override def funcFoldLeft(out: Type): String = ""
}

object ArrayType {

  def allowedBaseTypes: Seq[BaseType] = BaseType.reducedTypes

  def types: Seq[ArrayType] =
    allowedBaseTypes.map(ArrayType.apply) ++ // Array[T]
      allowedBaseTypes.map(ArrayType.apply).map(ArrayType.apply) // Array[Array[T]]

  def transformers: Seq[Transformer] = {
    val inTypes = types.map(_.underlined).distinct
    val outTypes = inTypes
    for {
      in <- inTypes
      out <- outTypes
    } yield Transformer(in, out)
  }

  def functionsHead: Seq[String] = types.map(_.funcHead)

  def functionsHeadOption: Seq[String] = types.map(_.funcHeadOption)

  def functionsTail: Seq[String] = types.map(_.funcTail)

  def functionsSize: Seq[String] = types.map(_.funcSize)

  def functionsForeach: Seq[String] = types.map(_.funcForeach)

  def functionsFilter: Seq[String] = types.map(_.funcFilter)

  def functionsMap: Seq[String] = transformers.map(t => ArrayType(t.in).funcMap(t.out))

  def functionsDrop: Seq[String] = types.map(_.funcDrop)

  def declarations: Seq[String] = types.map(_.declaration)

  def functionsToList: Seq[String] = types.map(_.funcToList)

  def functionsMkString: Seq[String] = types.map(_.funcMkString)
}
