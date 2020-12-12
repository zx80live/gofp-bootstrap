package com.zx80live.gofp.bootstrap.types

import com.zx80live.gofp.bootstrap.functions.FuncEquals

case class ArrayType(override val underlined: Type) extends MonadType with Traversable {

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

  override def funcTake: String =
    s"""
       |func (l $alias) Take(n int) $alias {
       |  size := len(l)
       |  Require(n >= 0, "index should be >= 0")
       |  if n >= size { n = size }
       |  acc := make($raw, n)
       |  copy(acc, l[0: n])
       |  return acc }""".stripMargin

  override def funcTakeRight: String =
    s"""
       |func (l $alias) TakeRight(n int) $alias {
       |  size := len(l)
       |  Require(n >= 0, "index should be >= 0")
       |  if n >= size { n = size }
       |  acc := make($raw, n)
       |  copy(acc, l[size - n: size])
       |  return acc
       |}""".stripMargin

  override def funcTakeWhile: String =
    s"""
       |func (l $alias) TakeWhile(p func(${underlined.raw}) bool) $alias {
       |  var n int
       |  size := len(l)
       |  for n = 0; n < size && p(l[n]); n ++ {}
       |  acc := make($raw, n)
       |  copy(acc, l)
       |  return acc
       |}""".stripMargin


  override def funcDrop: String =
    s"""
       |func (m $alias) Drop(i int) $raw {
       |  s := len(m)
       |  Require(Int(i).IsBetweenInclusive(0, s - 1), "index out of bound")
       |  if i < 0 || i >= s { panic ("index out of bound") }
       |  if s > 0 { return m[i:s-1] } else { return make([]${underlined.raw}, 0) } }""".stripMargin

  override def funcDropRight: String =
    s"""
       |func (l $alias) DropRight(n int) $alias {
       |  size := len(l)
       |  Require(n >= 0, "index should be >= 0")
       |  if n >= size { n = size }
       |  to := size - n
       |  acc := make($raw, to)
       |  copy(acc, l[0: to])
       |  return acc
       |}""".stripMargin

  override def funcDropWhile: String =
    s"""
       |func (l $alias) DropWhile(p func(${underlined.raw}) bool) $alias {
       |  size := len(l)
       |  var n int
       |  for n = 0; n < size && p(l[n]); n ++ {}
       |  acc := make($raw, size - n)
       |  copy(acc, l[n: size])
       |  return acc
       |}""".stripMargin

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

  def funcCount: String =
    s"""
       |func (l $alias) Count(p func(${underlined.raw}) bool) int {
       |  c := 0
       |  for _, e := range l {
       |    if p(e) { c++ }
       |  }
       |  return c } """.stripMargin

  override def funcZip(m: MonadType): String = {
    val a2 = ArrayType(Tuple2Type(underlined, BaseType.GoInt))
    m match {
      case _ : ArrayType =>
        s"""
           |func (a $alias) Zip${m.view}(a2 ${m.alias}) ${a2.alias} {
           |  minLen := int(Int(len(a)).Min(Int(len(a2))))
           |  zipped := make(${a2.raw}, minLen)
           |  for i := 0; i < minLen; i++ {
           |    zipped[i] = Tuple2 { a[i], a2[i] }
           |  }
           |  return zipped
           |}""".stripMargin
      case _ : ListType =>
        s"""
           |func (a $alias) Zip${m.view}(l2 ${m.alias}) ${a2.alias} {
           |  minLen := int(Int(len(a)).Min(Int(l2.Size())))
           |  zipped := make(${a2.raw}, minLen)
           |  xs := l2
           |  for i := 0; xs.NonEmpty() && i < minLen; i ++ {
           |    zipped[i] = Tuple2 { a[i], *xs.head }
           |    xs = *xs.tail
           |  }
           |  return zipped
           |}""".stripMargin
      case _ => ""
    }
  }

  override def funcZipAll(m: MonadType): String = {
    val a2 = ArrayType(Tuple2Type(underlined, BaseType.GoInt))
    m match {
      case _: ArrayType =>
        s"""
           |func (a $alias) ZipAll${m.view}(a2 ${m.alias}, thisDefault ${underlined.alias}, thatDefault ${m.underlined.alias}) ${a2.alias} {
           |  len1 := len(a); len2 := len(a2); maxLen := int(Int(len1).Max(Int(len2)))
           |  zipped := make(${a2.raw}, maxLen)
           |  var e1, e2 Any
           |  for i := 0; i < maxLen; i ++ {
           |    if i < len1 { e1 = a[i] } else { e1 = thisDefault }
           |    if i < len2 { e2 = a2[i] } else { e2 = thatDefault }
           |    zipped[i] = Tuple2 { e1, e2 }
           |  }
           |  return zipped
           |}""".stripMargin
      case _ : ListType =>
        s"""
           |func (a $alias) ZipAll${m.view}(l2 ${m.alias}, thisDefault ${underlined.alias}, thatDefault ${m.underlined.alias}) ${a2.alias} {
           |  len1 := len(a); maxLen := int(Int(len1).Max(Int(l2.Size())))
           |  zipped := make(${a2.raw}, maxLen)
           |  var e1, e2 Any
           |  xs := l2
           |  for i := 0; i < maxLen && xs.NonEmpty(); i ++ {
           |    if i < len1 { e1 = a[i] } else { e1 = thisDefault }
           |    if xs.NonEmpty() { e2 = *xs.head; xs = *xs.tail } else { e2 = thatDefault }
           |    zipped[i] = Tuple2 { e1, e2 }
           |  }
           |  return zipped
           |}""".stripMargin
      case _ => ""
    }
  }

  override def funcZipWithIndex: String = {
    val a2 = ArrayType(Tuple2Type(underlined, BaseType.GoInt))
    s"""
       |func(l $alias) ZipWithIndex() ${a2.alias} {
       |  zipped := make(${a2.raw}, len(l))
       |  for i, e := range l {
       |    zipped[i] = Tuple2 { e, i }
       |  }
       |  return zipped }""".stripMargin
  }

  override def iteratorName: String = s"${alias}Iterator"

  override def iteratorDeclaration: String =
    s"""
       |type $iteratorName struct {
       |  i int
       |  size int
       |  arr *$alias
       |}""".stripMargin

  override def funcHasNext: String =
    s"""
       |func (it *$iteratorName) HasNext() bool { return it.i < it.size }""".stripMargin

  override def funcNext: String =
    s"""
       |func (it *$iteratorName) Next() ${underlined.raw} {
       |  arr := *it.arr
       |  next := arr[it.i]
       |  it.i ++
       |  return next
       |}""".stripMargin

  override def funcIterator: String =
    s"""
      |func (a $alias) Iterator() $iteratorName { return $iteratorName { 0, len(a), &a } }""".stripMargin
}

object ArrayType {

  def allowedBaseTypes: Seq[Type] = BaseType.reducedTypes ++ Seq(Tuple2Type.defaultType)

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

  def functionsCount: Seq[String] = types.map(_.funcCount)

  def functionsTake: Seq[String] = types.map(_.funcTake)

  def functionsTakeWhile: Seq[String] = types.map(_.funcTakeWhile)

  def functionsTakeRight: Seq[String] = types.map(_.funcTakeRight)

  def functionsDropRight: Seq[String] = types.map(_.funcDropRight)

  def functionsDropWhile: Seq[String] = types.map(_.funcDropWhile)

  def functionsZipWithIndex: Seq[String] = types.map(_.funcZipWithIndex)

  def functionsZip: Seq[String] = {
    val inTypes = allowedBaseTypes.map(ArrayType.apply)
    val outTypes = inTypes ++ ListType.allowedBaseTypes.map(ListType.apply)
    for {
      in <- inTypes
      out <- outTypes
    } yield in.funcZip(out)
  }

  def functionsZipAll: Seq[String] = {
    val inTypes = allowedBaseTypes.map(ArrayType.apply)
    val outTypes = inTypes ++ ListType.allowedBaseTypes.map(ListType.apply)
    for {
      in <- inTypes
      out <- outTypes
    } yield in.funcZipAll(out)
  }

  def iterators: Seq[String] = types.map(_.iteratorDeclaration)
  def functionsIterator: Seq[String] = types.map(_.funcIterator)
  def functionsHasNext: Seq[String] = types.map(_.funcHasNext)
  def functionsNext: Seq[String] = types.map(_.funcNext)
}
