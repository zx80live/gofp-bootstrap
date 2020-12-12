package com.zx80live.gofp.bootstrap.types

case class LinkedListType(override val underlined: Type) extends MonadType with Traversable {
  override def setUnderlined(t: Type): MonadType = ???

  override def emptyName: String = s"Nil$view"

  override def emptyDeclaration: String =
    s"""
       |func $emptyName() *$raw {
       |	n := $raw{nil, nil, nil, nil, nil}
       |	n.head = &n // TODO clarify: how does gc utilize cyclic reference, if NOT then use condition in Add function instead
       |	n.end = &n
       |	return &n
       |}""".stripMargin

  override def consView: String = s"Mk${underlined.view}LikedList"

  override def funcCons: String =
    s"""
       |func $consView(elements ...${underlined.raw}) $raw {
       |}""".stripMargin

  def funcAppend: String =
    s"""
       |func (l *$raw) Append(e ${underlined.raw}) *$raw {
       |  nn := $raw{&e, l.head, nil, l, nil}
       |	nn.end = &nn
       |	l.next = &nn
       |	return &nn
       |}""".stripMargin

  override def funcFilter: String = ???

  override def funcMap(out: Type): String = ???

  override def funcToList: String = {
    val l2 = ListType(underlined)
    s"""
       |func (l $raw) ToList() ${l2.raw} {
       |  acc := ${l2.emptyName}
       |  l.ForeachRight(func (e ${underlined.raw}) { acc = acc.Cons(e) })
       |  return acc
       |}""".stripMargin
  }

  override def funcReduce: String = ???

  override def funcFlatMap(out: MonadType): String = ???

  override def funcFoldLeft(out: Type): String = ???

  override def funcFind: String = ???

  override def funcZip(m: MonadType): String = ???

  override def funcZipAll(m: MonadType): String = ???

  override def funcZipWithIndex: String = ???

  override def funcSize: String = ???

  override def funcForeach: String =
    s"""
       |func (n *$raw) Foreach(f func(${underlined.raw})) {
       |	h := n.head
       |	for {
       |		h = h.next
       |		if h == nil {
       |			return
       |		}
       |		f(*h.value)
       |	}
       |}""".stripMargin


  def funcForeachRight: String =
    s"""
       |func (n *$raw) ForeachRight(f func(${underlined.raw})) {
       |		e := n.end
       |    for e.value != nil {
       |       f(*e.value)
       |       e = e.prev
       |	}
       |}""".stripMargin


  override def funcTake: String = ???

  override def funcTakeWhile: String = ???

  override def funcTakeRight: String = ???

  override def funcDrop: String = ???

  override def funcDropRight: String = ???

  override def funcDropWhile: String = ???


  override def funcHead: String =
    s"""
       |func (l $raw) Head() ${underlined.raw} {
       |  if l.value != nil {
       |    return *l.value
       |  } else {
       |    panic("can't get head from empty list")
       |  }
       |}""".stripMargin

  override def funcHeadOption: String = ???

  override def funcTail: String =
    s"""
       |func (l $raw) Tail() $raw {
       |}""".stripMargin

  override def raw: String = s"${underlined.view}LinkedList"

  override def alias: String = raw

  override def view: String = raw


  override def declaration: String = {
    s"""
       |type $raw struct {
       |  value *${underlined.raw}
       |  head *$raw
       |  end *$raw
       |  prev *$raw
       |  next *$raw
       |}""".stripMargin
  }

  override def funcEquals: String = ???

  override def funcToString: String = ???

  override def iteratorName: String = ???

  override def iteratorDeclaration: String = ???

  override def funcIterator: String = ???

  override def funcHasNext: String = ???

  override def funcNext: String = ???
}

object LinkedListType {

  def allowedBaseTypes: Seq[Type] = BaseType.reducedTypes ++ Seq(Tuple2Type.defaultType)


  def types: Seq[LinkedListType] =
    allowedBaseTypes.map(LinkedListType.apply) ++ // List[T]
      allowedBaseTypes.map(ArrayType.apply).map(LinkedListType.apply) ++ // List[Array[T]]
      allowedBaseTypes.map(OptionType.apply).map(LinkedListType.apply) ++ // List[Option[T]]
      allowedBaseTypes.map(ListType.apply).map(LinkedListType.apply) // List[List[T]]

  def declarations: Seq[String] = types.map(_.declaration)

  def emptyDeclarations: Seq[String] = types.map(_.emptyDeclaration)

  def functionsHead: Seq[String] = types.map(_.funcHead)

  def functionsTail: Seq[String] = types.map(_.funcTail)

  def functionsForeach: Seq[String] = types.map(_.funcForeach)

  def functionsForeachRight: Seq[String] = types.map(_.funcForeachRight)

  def functionsAppend: Seq[String] = types.map(_.funcAppend)

  def functionsToList: Seq[String] = types.map(_.funcToList)
}
