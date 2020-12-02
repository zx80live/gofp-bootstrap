package com.zx80live.gofp.bootstrap.types

import com.zx80live.gofp.bootstrap.functions.FuncEquals

case class ArrayType(override val underlined: Type) extends MonadType {

  override def raw: String = s"[]${underlined.raw}"

  override def emptyName: String = ""

  override def view: String = s"${underlined.view}Array"

  override def alias: String = view

  override def declaration: String =
    s"""
       |type $alias $raw""".stripMargin

  override def consView: String = ""

  override def funcCons: String = ""

  override def emptyDeclaration: String = ""

  override def funcHead: String =
    s"""
       |func(m $alias) Head() ${underlined.raw} { if len(m) > 0 { return m[0] } else { panic("can't get head from empty $raw slice") } }""".stripMargin

  override def funcHeadOption: String = {
    val opt = OptionType(underlined)
    s"""
       |func (m $alias) HeadOption() ${opt.raw} { if len(m) > 0 { return ${opt.consView}(m[0]) } else { return ${opt.emptyName} } }""".stripMargin
  }

  override def funcTail: String =
    s"""
       |func (m $alias) Tail() $alias { s := len(m); if s > 0 { return m[1:s-1] } else {return []${underlined.raw}{} } }""".stripMargin

  override def funcSize: String =
    s"""
       |func (m $alias) Size() Int { return Int(len(${raw}(m))) }""".stripMargin

  override def funcForeach: String =
    s"""
       |func (m $alias) Foreach(f func(${underlined.raw})) { for _, e := range m { f(e) } }""".stripMargin

  override def funcFilter: String =
    s"""
       |func (m $alias) Filter(p ${Predicate(underlined).name}) $alias {
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
       |func (m $alias) Map${out.view}(f func(${underlined.raw}) ${out.raw}) ${a2.raw} {
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
       |func (m $alias) Drop(i int) $raw {
       |  s := len(m)
       |  if i < 0 || i >= s { panic ("index out of bound") }
       |  if s > 0 { return m[i:s-1] } else { return make([]${underlined.raw}, 0) } }""".stripMargin

  override def funcToList: String = {
    val l = ListType(underlined)
    s"""
       |func (m $alias) ToList() ${l.raw} {
       |  acc := ${l.emptyName}
       |  for _, e := range m {
       |    acc = acc.Cons(e)
       |  }
       |  return acc.Reverse()
       |}""".stripMargin
  }

  override def funcEquals: String =
    s"""
       |func (a $alias) Equals(b $alias) bool {
       |  len1 := a.Size()
       |  if len1 != b.Size() { return false }
       |  for i, e := range a {
       |    if ${FuncEquals.name(underlined)}(${underlined.alias}(e), ${underlined.alias}(b[i])) { return false }
       |  }
       |  return true}""".stripMargin

  def funcMkString: String =
    s"""
       |func (a $alias) MkString(start, sep, end string) String {
       |	 content := ""
       |	 for _, e := range a {
       |	   content = fmt.Sprintf("%v%v%v", content, ${underlined.view}ToString(${underlined.alias}(e)), sep)
       |	 }
       |	 l := len(content)
       |	 if l > 0 {
       |		 content = content[:l-1]
       |	 }
       |	 return String(fmt.Sprintf("%v%v%v", start, content, end))
       |}""".stripMargin

  override def funcToString: String =
    s"""
       |func (a $alias) ToString() String {
       |  return a.MkString("[", ",", "]")
       |}""".stripMargin

  override def setUnderlined(t: Type): MonadType = ArrayType(t)

  override def funcFlatMap(out: MonadType): String = ""

  override def funcFlatten: String = ""

  override def funcReduce: String = ""

  override def funcFoldLeft(out: Type): String = ""

  override def funcFind: String =
    s"""
       |func (a $alias) Find(p func(${underlined.raw}) bool) ${OptionType(underlined).raw} {
       |  for _, e := range a {
       |    if p(e) { return ${OptionType(underlined).consView}(e) }
       |  }
       |  return ${OptionType(underlined).emptyName}}""".stripMargin

  def funcCount: String = ???

  def funcTake: String = ???

  def funcTakeWhile: String = ???

  def funcTakeRight: String = ???

  def funcDropRight: String = ???

  def funcDropWhile: String = ???
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

  def functionsToString: Seq[String] = types.map(_.funcToString)

  def functionsEquals: Seq[String] = types.map(_.funcEquals)

  def functionsFind: Seq[String] = types.map(_.funcFind)
}
