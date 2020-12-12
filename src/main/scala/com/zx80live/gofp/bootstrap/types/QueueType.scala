package com.zx80live.gofp.bootstrap.types

case class QueueType(underlined: Type) extends MonadType with Traversable {
  override def setUnderlined(t: Type): MonadType = ???

  override def emptyName: String = s"Nil${underlined.view}Queue"

  override def emptyDeclaration: String =
    s"""
       |var $emptyName = $raw{ &${ListType(underlined).emptyName}, &${ListType(underlined).emptyName} }""".stripMargin

  override def consView: String = s"Mk${underlined.view}Queue"

  override def funcCons: String =
    s"""
       |func $consView(elements ...${underlined.raw}) $raw {
       |  q := $emptyName
       |  for _, e := range elements {
       |    q = q.Enqueue(e)
       |  }
       |  return q
       |}""".stripMargin

  override def funcFilter: String =
    s"""
       |func (q $raw) Filter(p ${Predicate(underlined).name}) $raw {
       |  in := (*q.in).Filter(p)
       |  out := (*q.out).Filter(p)
       |  return $raw { &in, &out  }
       |}""".stripMargin

  override def funcMap(out: Type): String = {
    val q = QueueType(out)
    s"""
       |func (q $raw) Map${out.view}(f func( ${underlined.raw} ) ${out.raw}) ${q.raw} {
       |  acc := ${q.emptyName}
       |  if q.IsEmpty() { return acc }
       |  xs := q
       |  for {
       |    h, t := xs.Dequeue()
       |    acc = acc.Enqueue(f(h))
       |    xs = t
       |
       |    if t.IsEmpty() { return acc }
       |  }
       |  return acc
       |}""".stripMargin
  }

  override def funcToList: String = {
    val l = ListType(underlined)
    s"""
       |func (q $raw) ToList() ${l.raw} {
       |  return *q.swap().out
       |}""".stripMargin
  }

  override def funcReduce: String =
    s"""
       |func (q $raw) Reduce(f func( ${underlined.raw}, ${underlined.raw} ) ${underlined.raw}) ${underlined.raw} {
       |  if q.IsEmpty() { panic("Can't reduce empty queue") } else {
       |    h, t := q.Dequeue()
       |    if t.IsEmpty() { return h }
       |    return f(h, t.Reduce(f))
       |  }
       |}""".stripMargin

  override def funcFlatMap(out: MonadType): String = ???

  override def funcFoldLeft(out: Type): String = ???

  override def funcFind: String = ???

  override def funcZip(m: MonadType): String = ???

  override def funcZipAll(m: MonadType): String = ???

  override def funcZipWithIndex: String = ???

  override def raw: String = s"${underlined.view}Queue"

  override def alias: String = raw

  override def view: String = alias

  override def declaration: String = {
    val l = ListType(underlined)
    s"""
       |type $raw struct {
       |  in *${l.raw}
       |  out *${l.raw}
       |}""".stripMargin
  }

  override def funcEquals: String = ???

  override def funcToString: String = ???

  def funcEnqueue: String =
    s"""
       |func (q $raw) Enqueue(e ${underlined.raw}) $raw {
       |	in := (*q.in).Cons(e)
       |	return $raw{&in, q.out}
       |}""".stripMargin

  def funcSwap: String =
    s"""
       |func (q $raw) swap() $raw {
       |	if (*q.out).NonEmpty() {
       |		return q
       |	} else {
       |		out := (*q.in).Reverse()
       |		return $raw{&${ListType(underlined).emptyName}, &out}
       |	}
       |}""".stripMargin

  def funcDequeue: String =
    s"""
       |func (q $raw) Dequeue() (${underlined.raw}, $raw) {
       |  if q.NonEmpty() {
       |	  swapped := q.swap()
       |	  h := (*swapped.out).Head()
       |	  t := (*swapped.out).Tail()
       |	  return h, $raw{swapped.in, &t}
       |  } else { panic("can't dequeue from empty queue ") }
       |}""".stripMargin

  def funcDequeueOption: String = {
    val o = OptionType(underlined)
    s"""
       |func (q $raw) DequeueOption() (${o.raw}, $raw) {
       |  if q.NonEmpty() {
       |	  swapped := q.swap()
       |	  h := (*swapped.out).Head()
       |	  t := (*swapped.out).Tail()
       |	  return ${o.consView}(h), $raw{swapped.in, &t}
       |  } else { return ${o.emptyName}, $emptyName }
       |}""".stripMargin
  }

  override def funcSize: String = ???

  override def funcForeach: String =
    s"""
       |func (q $raw) Foreach(f func(e ${underlined.raw})) {
       |	xs := q
       |	for xs.NonEmpty() {
       |		f(xs.Head())
       |		xs = xs.Tail()
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
       |func (q $raw) Head() ${underlined.raw} {
       |	h, _ := q.Dequeue()
       |	return h
       |}""".stripMargin

  override def funcHeadOption: String =
    s"""
       |func (q $raw) HeadOption() ${OptionType(underlined).raw} {
       |	h, _ := q.DequeueOption()
       |	return h
       |}""".stripMargin

  override def funcTail: String =
    s"""
       |func (q $raw) Tail() $raw {
       |	_, t := q.Dequeue()
       |	return t
       |}""".stripMargin

  def funcNonEmpty: String =
    s"""
       |func (q $raw) NonEmpty() bool {
       |	return (*q.in).NonEmpty() || (*q.out).NonEmpty()
       |}""".stripMargin

  def funcIsEmpty: String =
    s"""
       |func (q $raw) IsEmpty() bool {
       |	return (*q.in).IsEmpty() && (*q.out).IsEmpty()
       |}""".stripMargin
}

object QueueType {

  def allowedBaseTypes: Seq[Type] = BaseType.reducedTypes ++ Seq(Tuple2Type.defaultType)

  def types: Seq[QueueType] =
    allowedBaseTypes.map(QueueType.apply) ++ // Queue[T]
      allowedBaseTypes.map(ArrayType.apply).map(QueueType.apply) ++ // Queue[Array[T]]
      allowedBaseTypes.map(OptionType.apply).map(QueueType.apply) ++ // Queue[Option[T]]
      allowedBaseTypes.map(ListType.apply).map(QueueType.apply) // Queue[List[T]]

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
  def functionsEnqueue: Seq[String] = types.map(_.funcEnqueue)
  def functionsSwap: Seq[String] = types.map(_.funcSwap)
  def functionsDequeue: Seq[String] = types.map(_.funcDequeue)
  def functionsDequeueOption: Seq[String] = types.map(_.funcDequeueOption)
  def functionsHead: Seq[String] = types.map(_.funcHead)
  def functionsHeadOption: Seq[String] = types.map(_.funcHeadOption)
  def functionsTail: Seq[String] = types.map(_.funcTail)
  def functionsNonEmpty: Seq[String] = types.map(_.funcNonEmpty)
  def functionsIsEmpty: Seq[String] = types.map(_.funcIsEmpty)
  def functionsForeach: Seq[String] = types.map(_.funcForeach)
  def functionsToList: Seq[String] = types.map(_.funcToList)
  def functionsCons: Seq[String] = types.map(_.funcCons)
  def functionsFilter: Seq[String] = types.map(_.funcFilter)
  def functionsReduce: Seq[String] = types.map(_.funcReduce)
  def functionsMap: Seq[String] = transformers.map(t => QueueType(t.in).funcMap(t.out))
}