package com.zx80live.gofp.bootstrap.types

import com.zx80live.gofp.bootstrap.functions.{FuncEquals, FuncToString}

case class ListType(override val underlined: Type) extends MonadType with Traversable {

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

  override def consView: String = s"Mk$raw"

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
       |func (l $raw) Tail() $raw { return *l.tail }""".stripMargin

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
       |    if !${FuncEquals.name(underlined)}(${underlined.alias}(*xs1.head), ${underlined.alias}(*xs2.head)) { return false }
       |    xs1 = *xs1.tail
       |    xs2 = *xs2.tail
       |  }
       |  return true }""".stripMargin

  def funcMkString: String =
    s"""
       |func (l $raw) MkString(start, sep, end string) String {
       |   content := ""
       |   xs := l
       |   for xs.NonEmpty() {
       |     content = fmt.Sprintf("%v%v%v", content, ${FuncToString.name(underlined)}(${underlined.alias}(*xs.head)), sep)
       |     xs = *xs.tail
       |   }
       |	 s := len(content)
       |	 if s > 0 {
       |		 content = content[:s-1]
       |	 }
       |	 return String(fmt.Sprintf("%v%v%v", start, content, end))
       |}""".stripMargin

  override def funcToString: String =
    s"""
       |func (l $raw) ToString() String { return l.MkString("List(", ",", ")") }""".stripMargin

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

  override def funcFind: String =
    s"""
       |func (l $raw) Find(p func(${underlined.raw}) bool) ${OptionType(underlined).raw} {
       |  xs := l
       |  for xs.NonEmpty() {
       |    if p(*xs.head) { return ${OptionType(underlined).consView}(*xs.head) }
       |    xs = *xs.tail
       |  }
       |  return ${OptionType(underlined).emptyName}}""".stripMargin


  def funcGroupBy(out: Type): String = out match {
    case _: ArrayType =>
      ""
    case _ =>
      s"""
         |func (l $raw) GroupBy${out.view}(f func(${underlined.raw}) ${out.raw}) map[${out.raw}]${ListType(underlined).raw} {
         |	m := make(map[${out.raw}]${ListType(underlined).raw})
         |
         |	l.Foreach(func(e ${underlined.raw}) {
         |		key := f(e)
         |		var group ${ListType(underlined).raw}
         |
         |		if value, found := m[key]; found {
         |			group = value
         |		} else {
         |			group = ${ListType(underlined).emptyName}
         |		}
         |		group = group.Cons(e)
         |		m[key] = group
         |	})
         |
         |	return m }""".stripMargin
  }

  def funcCount: String =
    s"""
       |func (l $raw) Count(p func(${underlined.raw}) bool) int {
       |  i := 0
       |  xs := l
       |  for xs.NonEmpty() {
       |    if p(*xs.head) { i ++ }
       |    xs = *xs.tail
       |  }
       |  return i}""".stripMargin

  override def funcSize: String =
    s"""
       |func (l $raw) Size() int { count := 0; xs := l; for xs.NonEmpty() { count ++; xs = *xs.tail }; return count }""".stripMargin

  override def funcForeach: String =
    s"""
       |func (l $raw) Foreach(f func(${underlined.raw})) {
       |  xs := l
       |  for xs.NonEmpty() {
       |    f(*xs.head)
       |    xs = *xs.tail
       |  }
       |}""".stripMargin

  def funcTake: String =
    s"""
       |func (l $raw) Take(n int) $raw {
       |  acc := $emptyName
       |  xs := l
       |  for i := 0;  xs.NonEmpty() && i < n; i ++ {
       |    acc = acc.Cons(*xs.head)
       |    xs = *xs.tail
       |  }
       |  return acc.Reverse()}
       |""".stripMargin

  def funcTakeRight: String =
    s"""
       |func (l $raw) TakeRight(n int) $raw { return l.Reverse().Take(n).Reverse() }""".stripMargin

  def funcTakeWhile: String =
    s"""
       |func (l $raw) TakeWhile(p func(${underlined.raw}) bool) $raw {
       |  acc := $emptyName
       |  xs := l
       |  for xs.NonEmpty() && p(*xs.head) {
       |    acc = acc.Cons(*xs.head)
       |    xs = *xs.tail
       |  }
       |  return acc.Reverse()}""".stripMargin


  override def funcDrop: String =
    s"""
       |func (l $raw) Drop(n int) $raw {
       |  acc := l
       |  for i := 0; acc.NonEmpty() && i < n; i ++ {
       |    acc = *acc.tail
       |  }
       |  return acc
       |}""".stripMargin

  def funcDropRight: String =
    s"""
       |func (l $raw) DropRight(n int) $raw { return l.Reverse().Drop(n).Reverse() }""".stripMargin

  def funcDropWhile: String =
    s"""
       |func (l $raw) DropWhile(p func(${underlined.raw}) bool) $raw {
       |  xs := l
       |  for xs.NonEmpty() && p(*xs.head) {
       |    xs = *xs.tail
       |  }
       |  return xs
       |}""".stripMargin


  override def funcZip(m: MonadType): String = {
    val l2 = ListType(Tuple2Type(underlined, m.underlined))

    m match {
      case _: ArrayType =>
        s"""
           |func (l $alias) Zip${m.view}(l2 ${m.alias}) ${l2.alias} {
           |  zipped := ${l2.emptyName}; xs := l; len2 := len(l2)
           |  for i := 0; xs.NonEmpty() && i < len2; i ++ {
           |    zipped = zipped.Cons(Tuple2 { *xs.head, l2[i] } )
           |    xs = *xs.tail
           |  }
           |  return zipped.Reverse() }""".stripMargin
      case _: ListType  =>
        s"""
           |func (l $alias) Zip${m.view}(l2 ${m.alias}) ${l2.alias} {
           |  zipped := ${l2.emptyName}; xs1 := l; xs2 := l2
           |  for xs1.NonEmpty() && xs2.NonEmpty() {
           |    zipped = zipped.Cons(Tuple2 { *xs1.head, *xs2.head } )
           |    xs1 = *xs1.tail
           |    xs2 = *xs2.tail
           |  }
           |  return zipped.Reverse() }""".stripMargin
      case _ => ""

    }
  }

  override def funcZipAll(m: MonadType): String = {
    val l2 = ListType(Tuple2Type(underlined, BaseType.GoInt))
    m match {
      case _: ListType =>
        s"""
           |func (l $alias) ZipAll${m.view}(l2 ${m.alias}, thisDefault ${underlined.alias}, thatDefault ${m.underlined.alias}) ${l2.alias} {
           |  zipped := ${l2.emptyName}; xs1 := l; xs2 := l2
           |  maxLen := int(Int(l.Size()).Max(Int(l2.Size())))
           |
           |  var e1, e2 Any
           |  for i := 0; i < maxLen; i++ {
           |    if xs1.NonEmpty() { e1 = *xs1.head; xs1 = *xs1.tail } else { e1 = thisDefault }
           |    if xs2.NonEmpty() { e2 = *xs2.head; xs2 = *xs2.tail } else { e2 = thatDefault }
           |
           |    zipped = zipped.Cons( Tuple2 { e1, e2 } )
           |  }
           |  return zipped.Reverse() }""".stripMargin
      case _: ArrayType =>
        s"""
           |func (l $alias) ZipAll${m.view}(l2 ${m.alias}, thisDefault ${underlined.alias}, thatDefault ${m.underlined.alias}) ${l2.alias} {
           |  zipped := ${l2.emptyName}; xs1 := l
           |  len1 := l.Size(); len2 := len(l2)
           |  maxLen := int(Int(len1).Max(Int(len2)))
           |
           |  var e1, e2 Any
           |  for i := 0; i < maxLen; i++ {
           |    if xs1.NonEmpty() { e1 = *xs1.head; xs1 = *xs1.tail } else { e1 = thisDefault }
           |    if i < len2 { e2 = l2[i] } else { e2 = thatDefault }
           |
           |    zipped = zipped.Cons( Tuple2 { e1, e2 } )
           |  }
           |  return zipped.Reverse() }""".stripMargin
      case _ => ""
    }
  }

  override def funcZipWithIndex: String = {
    val l2 = ListType(Tuple2Type(underlined, BaseType.GoInt))
    s"""
       |func (l $alias) ZipWithIndex() ${l2.raw} {
       |  zipped := ${l2.emptyName}; xs := l
       |  for i := 0; xs.NonEmpty(); i ++ {
       |    zipped = zipped.Cons(Tuple2 { *xs.head, i } )
       |    xs = *xs.tail
       |  }
       |  return zipped.Reverse() }""".stripMargin

  }
}

object ListType {

  def allowedBaseTypes: Seq[Type] = BaseType.reducedTypes ++ Seq(Tuple2Type.defaultType)

  def types: Seq[ListType] =
    allowedBaseTypes.map(ListType.apply) ++ // List[T]
      allowedBaseTypes.map(ArrayType.apply).map(ListType.apply) ++ // List[Array[T]]
      allowedBaseTypes.map(OptionType.apply).map(ListType.apply) ++ // List[Option[T]]
      allowedBaseTypes.map(ListType.apply).map(ListType.apply) // List[List[T]]

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

  def functionsMap: Seq[String] = transformers.map(t => ListType(t.in).funcMap(t.out))

  def functionsMkString: Seq[String] = types.map(_.funcMkString)

  def functionsToString: Seq[String] = types.map(_.funcToString)

  def functionsEquals: Seq[String] = types.map(_.funcEquals)

  def functionsToArray: Seq[String] = types.map(_.funcToArray)

  def functionsFlatMap: Seq[String] = {
    val outTypes = allowedBaseTypes.map(ListType.apply)
    for {
      o1 <- types
      o2 <- outTypes
    } yield o1.funcFlatMap(o2)
  }

  def functionsFlatten: Seq[String] = {
    val inTypes = allowedBaseTypes.map(ListType.apply).map(ListType.apply)
    inTypes.map(_.funcFlatten)
  }

  def functionsReduce: Seq[String] = types.map(_.funcReduce)

  def functionsFoldLeft: Seq[String] = {
    val inTypes = allowedBaseTypes.map(ListType.apply) ++ allowedBaseTypes.map(OptionType.apply).map(ListType.apply)
    val outTypes = allowedBaseTypes ++ allowedBaseTypes.map(ListType.apply)
    for {
      t <- inTypes
      out <- outTypes
    } yield t.funcFoldLeft(out)
  }

  def functionsFind: Seq[String] = types.map(_.funcFind)

  def functionsGroupBy: Seq[String] = {
    types.map(t => t.funcGroupBy(t.underlined))
    //  {
    //    val inTypes = types
    //    val outTypes = types.map(_.underlined)
    //    for {
    //      in <- inTypes
    //      out <- outTypes
    //    } yield in.funcGroupBy(out)
    //  }
  }

  def functionsCount: Seq[String] = types.map(_.funcCount)

  def functionsTake: Seq[String] = types.map(_.funcTake)

  def functionsTakeWhile: Seq[String] = types.map(_.funcTakeWhile)

  def functionsTakeRight: Seq[String] = types.map(_.funcTakeRight)

  def functionsDrop: Seq[String] = types.map(_.funcDrop)

  def functionsDropRight: Seq[String] = types.map(_.funcDropRight)

  def functionsDropWhile: Seq[String] = types.map(_.funcDropWhile)

  def functionsZipWithIndex: Seq[String] = types.map(_.funcZipWithIndex)

  def functionsZip: Seq[String] = {
    val inTypes = allowedBaseTypes.map(ListType.apply)
    val outTypes = inTypes ++ ArrayType.allowedBaseTypes.map(ArrayType.apply)
    for {
      in <- inTypes
      out <- outTypes
    } yield in.funcZip(out)
  }

  def functionsZipAll: Seq[String] = {
    val inTypes = allowedBaseTypes.map(ListType.apply)
    val outTypes = inTypes ++ ArrayType.allowedBaseTypes.map(ArrayType.apply)
    for {
      in <- inTypes
      out <- outTypes
    } yield in.funcZipAll(out)
  }
}
