package com.zx80live.gofp.bootstrap.types

import com.zx80live.gofp.bootstrap.functions.{FuncEquals, FuncToString}

case class ListType(override val underlined: Type) extends MonadType {

  override def raw: String = s"${underlined.view}List"

  override def emptyName: String = s"Nil${underlined.view}"

  override def alias: String = raw

  override def view: String = raw

  override def declaration: String =
    s"""
       |type $raw struct {
       |  head *${underlined.raw}
       |  tail *${raw}
       |}""".stripMargin

  override def consView: String = s"Make$raw"

  override def emptyDeclaration: String =
    s"""
       |var $emptyName $raw = $raw { nil, nil }""".stripMargin

  def funcPrepend: String =
    s"""
       |func (l $raw) Cons(e ${underlined.raw}) $raw { return $raw { &e, &l } }""".stripMargin

  override def funcCons: String =
    s"""
       |func $consView(elements ...${underlined.raw}) $raw {
       |  xs := $emptyName
       |  for _, e := range elements {
       |    xs = xs.Cons(e)
       |  }
       |  return xs.Reverse()
       |}""".stripMargin

  def funcIsEmpty: String =
    s"""
       |func (l $raw) IsEmpty() bool { return l == $emptyName }""".stripMargin

  def funcNonEmpty: String =
    s"""
       |func (l $raw) NonEmpty() bool { return !l.IsEmpty() }""".stripMargin

  override def funcHeadOption: String = {
    if (nestedLevel < 4) {
      s"""
         |func (l $raw) HeadOption() ${OptionType(underlined).raw} { if l.NonEmpty() { return ${OptionType(underlined).consView}(*l.head) } else { return ${OptionType(underlined).emptyName} } }""".stripMargin
    } else ""
  }

  override def funcHead: String =
    s"""
       |func (l $raw) Head() ${underlined.raw} { return *l.head }""".stripMargin

  override def funcTail: String =
    s"""
       |func (l $raw) Tail() $raw { return l.tail.Copy() }""".stripMargin

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
       |  acc := $emptyName
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
       |  acc := $emptyName
       |  xs := l
       |  for xs.NonEmpty() {
       |    acc = acc.Cons(*xs.head)
       |    xs = *xs.tail
       |  }
       |  return acc.Reverse()
       |}""".stripMargin

  override def funcSize: String =
    s"""
       |func (l $raw) Size() int { count := 0; xs := l; for xs.NonEmpty() { count ++; xs = *xs.tail }; return count }""".stripMargin

  def funcToArray: String =
    s"""
       |func (l $raw) ToArray() []${underlined.raw} { acc := make([]${underlined.raw}, l.Size()); xs := l; i := 0; for xs.NonEmpty() { acc[i] = *xs.head; xs = *xs.tail }; return acc }""".stripMargin

  override def funcFilter: String =
    s"""
       |func (l $raw) Filter(p ${Predicate(underlined).name}) $raw {
       |  acc := $emptyName
       |  xs := l
       |  for xs.NonEmpty() {
       |    if p(*xs.head) { acc = acc.Cons(*xs.head) }
       |    xs = *xs.tail
       |  }
       |  return acc.Reverse()
       |}""".stripMargin

  override def funcMap(out: Type): String = {
    val l = ListType(out)
    s"""
       |func (l $raw) Map${out.view}(f func(${underlined.raw}) ${out.raw}) ${l.raw} {
       |  acc := ${l.emptyName}
       |  xs := l
       |  for xs.NonEmpty() {
       |    acc = acc.Cons(f(*xs.head))
       |    xs = *xs.tail
       |  }
       |  return acc.Reverse() }""".stripMargin
  }

  override def funcDrop: String =
    s"""
       |func (l $raw) Drop(n int) $raw {
       |  acc = l
       |  for i = 0; i < n; i ++ {
       |    acc = *acc.tail
       |  }
       |  return acc
       |}
       |""".stripMargin

  override def funcToList: String = ""

  override def funcEquals: String =
    s"""
       |func (a $raw) Equals(b $raw) bool {
       |  len1 := a.Size()
       |  len2 := b.Size()
       |  if len1 != len2 { return false }
       |  xs1 := a
       |  xs2 := b
       |  for xs1.NonEmpty() {
       |    if !${FuncEquals.name(underlined)}(*xs1.head, *xs2.head) { return false }
       |    xs1 = *xs1.tail
       |    xs2 = *xs2.tail
       |  }
       |  return true }""".stripMargin

  def funcMkString: String =
    s"""
       |func (l $raw) MkString(start, sep, end string) string {
       |   content := ""
       |   xs := l
       |   for xs.NonEmpty() {
       |     content = fmt.Sprintf("%v%v%v", content, ${FuncToString.name(underlined)}(*xs.head), sep)
       |     xs = *xs.tail
       |   }
       |	 s := len(content)
       |	 if s > 0 {
       |		 content = content[:s-1]
       |	 }
       |	 return fmt.Sprintf("%v%v%v", start, content, end)
       |}""".stripMargin

  override def funcToString: String =
    s"""
       |func (l $raw) ToString() string { return l.MkString("List(", ",", ")") }""".stripMargin

  override def setUnderlined(t: Type): MonadType = ListType(t)

  override def funcFlatMap(out: MonadType): String = out match {
    case l: ListType =>
      s"""
         |func (m $raw) FlatMap${out.underlined.view}(f func(${underlined.raw}) ${out.raw}) ${out.raw} { if m.IsEmpty() { return ${l.emptyName} } else { acc := ${l.emptyName}; xs := m; for xs.NonEmpty() { exs := f(*xs.head); for exs.NonEmpty() { acc = acc.Cons(*exs.head); exs = *exs.tail }; xs = *xs.tail }; return acc.Reverse() } }""".stripMargin
    case _ => ???
  }

  override def funcReduce: String =
    s"""
       |func (l $raw) Reduce(f func(${underlined.raw}, ${underlined.raw}) ${underlined.raw}) ${underlined.raw} {
       |  if l.IsEmpty() { panic("Can't reduce empty list") } else if l.tail.IsEmpty() { return *l.head } else { return f(*l.head, l.tail.Reduce(f) ) } }""".stripMargin

  override def funcFoldLeft(out: Type): String =
    s"""
       |func (l $raw) FoldLeft${out.view}(z ${out.raw}, f func(${out.raw}, ${underlined.raw}) ${out.raw}) ${out.raw} {
       |  acc := z
       |  l.Foreach(func (e ${underlined.raw}) { acc = f(acc, e) })
       |  return acc}""".stripMargin
}

object ListType {
  def underlinedTypes: Seq[Type] = BaseType.types ++ OptionType.types ++ ArrayType.types

  //TODO reduce list types
  def types: Seq[ListType] = (BaseType.types ++ OptionType.types ++ ArrayType.types ++ BaseType.types.map(ListType.apply)).map(ListType.apply)

  def declarations: Seq[String] = types.map(_.declaration)

  def emptyDeclarations: Seq[String] = types.map(_.emptyDeclaration)

  def functionsPrepend: Seq[String] = types.map(_.funcPrepend)

  def functionsCons: Seq[String] = types.map(_.funcCons)

  def functionsIsEmpty: Seq[String] = types.map(_.funcIsEmpty)

  def functionsNonEmpty: Seq[String] = types.map(_.funcNonEmpty)

  def functionsHead: Seq[String] = types.map(_.funcHead)

  def functionsHeadOption: Seq[String] = types.map(_.funcHeadOption)

  def functionsTail: Seq[String] = types.map(_.funcTail)

  def functionsForeach: Seq[String] = types.map(_.funcForeach)

  def functionsReverse: Seq[String] = types.map(_.funcReverse)

  def functionsCopy: Seq[String] = types.map(_.funcCopy)

  def functionsFilter: Seq[String] = types.map(_.funcFilter)

  def functionsSize: Seq[String] = types.map(_.funcSize)

  def functionsMap: Seq[String] = for {
    o <- types
    t <- Transformer.types
    if o.underlined == t.in
  } yield o.funcMap(t.out)

  def functionsMkString: Seq[String] = types.map(_.funcMkString)

  def functionsToString: Seq[String] = types.map(_.funcToString)

  def functionsEquals: Seq[String] = types.map(_.funcEquals)

  def functionsToArray: Seq[String] = types.map(_.funcToArray)

  def functionsFlatMap: Seq[String] = {
    val outTypes = BaseType.types.map(ListType.apply)
    for {
      o1 <- types
      o2 <- outTypes
    } yield o1.funcFlatMap(o2)
  }

  def functionsFlatten: Seq[String] = {
    val inTypes = BaseType.types.map(ListType.apply).map(ListType.apply)
    inTypes.map(_.funcFlatten)
  }

  def functionsReduce: Seq[String] = types.map(_.funcReduce)

  def functionsFoldLeft: Seq[String] = {
    val inTypes = BaseType.reducedTypes.map(ListType.apply) ++ BaseType.reducedTypes.map(OptionType.apply).map(ListType.apply)
    val outTypes = BaseType.reducedTypes ++ BaseType.reducedTypes.map(ListType.apply)
    for {
      t <- inTypes
      out <- outTypes
    } yield t.funcFoldLeft(out)
  }
}
