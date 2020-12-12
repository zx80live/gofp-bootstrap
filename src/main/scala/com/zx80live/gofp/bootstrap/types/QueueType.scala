package com.zx80live.gofp.bootstrap.types

import com.zx80live.gofp.bootstrap.functions.{FuncEquals, FuncToString}

case class QueueType(underlined: Type) extends MonadType with Traversable {
  override def setUnderlined(t: Type): MonadType = ???

  override def emptyName: String = s"Nil$view"

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

  override def funcFoldLeft(out: Type): String =
    s"""
       |func (q $raw) FoldLeft${out.view}(z ${out.raw}, f func(${out.raw}, ${underlined.raw}) ${out.raw}) ${out.raw} {
       |  acc := z
       |  q.Foreach(func (e ${underlined.raw}) { acc = f(acc, e) })
       |  return acc}""".stripMargin


  override def funcFind: String =
    s"""
       |func (q $raw) Find(p func(${underlined.raw}) bool) ${OptionType(underlined).raw} {
       |  xs := q
       |  for xs.NonEmpty() {
       |    h, t := xs.Dequeue()
       |    if p(h) { return ${OptionType(underlined).consView}(h) }
       |    xs = t
       |  }
       |  return ${OptionType(underlined).emptyName}}""".stripMargin


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

  override def funcEquals: String =
    s"""
       |func (a $raw) Equals(b $raw) bool {
       |  len1 := a.Size()
       |  len2 := b.Size()
       |  if len1 != len2 { return false }
       |  if len1 == 0 { return true }
       |  xs1 := a
       |  xs2 := b
       |  for {
       |    h1, t1 := xs1.Dequeue()
       |    h2, t2 := xs2.Dequeue()
       |
       |    if !${FuncEquals.name(underlined)}(${underlined.alias}(h1), ${underlined.alias}(h2)) { return false }
       |
       |    if t1.IsEmpty() { break }
       |    xs1 = t1
       |    xs2 = t2
       |  }
       |  return true }""".stripMargin


  def funcMkString: String =
    s"""
       |func (q $raw) MkString(start, sep, end string) String {
       |   if q.IsEmpty() { return String(fmt.Sprintf("Queue()")) }
       |   content := ""
       |   xs := q
       |   for {
       |     h, t := xs.Dequeue()
       |     content = fmt.Sprintf("%v%v%v", content, ${FuncToString.name(underlined)}(${underlined.alias}(h)), sep)
       |     if t.IsEmpty() { break }
       |     xs = t
       |   }
       |	 s := len(content)
       |	 if s > 0 {
       |		 content = content[:s-1]
       |	 }
       |	 return String(fmt.Sprintf("%v%v%v", start, content, end))
       |}""".stripMargin

  override def funcToString: String =
    s"""
       |func (q $raw) ToString() String { return q.MkString("Queue(", ",", ")") }""".stripMargin


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

  override def funcSize: String =
    s"""
       |func (q $raw) Size() int {
       |  count := 0
       |  xs := q
       |  for xs.NonEmpty() {
       |    count ++
       |    _, xs = xs.Dequeue()
       |  }
       |  return count
       |}""".stripMargin

  override def funcForeach: String =
    s"""
       |func (q $raw) Foreach(f func(e ${underlined.raw})) {
       |	xs := q
       |	for xs.NonEmpty() {
       |		f(xs.Head())
       |		xs = xs.Tail()
       |	}
       |}""".stripMargin

  override def funcTake: String =
    s"""
       |func (q $raw) Take(n int) $raw {
       |  acc := $emptyName
       |  xs := q
       |  for i := 0;  xs.NonEmpty() && i < n; i ++ {
       |    h, t := xs.Dequeue()
       |    acc = acc.Enqueue(h)
       |    xs = t
       |  }
       |  return acc
       |}""".stripMargin


  override def funcTakeWhile: String =
    s"""
       |func (q $raw) TakeWhile(p func(${underlined.raw}) bool) $raw {
       |  acc := $emptyName
       |  if q.IsEmpty() { return acc }
       |  xs := q
       |  for {
       |    h, t := xs.Dequeue()
       |    if p(h) && t.NonEmpty() { acc = acc.Enqueue(h); xs = t } else { break }
       |  }
       |  return acc
       |}""".stripMargin


  override def funcTakeRight: String =
    s"""
       |func (q $raw) TakeRight(n int) $raw {
       |  acc := ${ListType(underlined).emptyName}
       |  xs := q.swap().out.Reverse()
       |
       |  for i :=0; xs.NonEmpty() && i < n; i ++ {
       |    acc = acc.Cons(*xs.head)
       |    xs = *xs.tail
       |  }
       |  return $raw { &${ListType(underlined).emptyName}, &acc }
       |}""".stripMargin


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
  def functionsMkString: Seq[String] = types.map(_.funcMkString)
  def functionsToString: Seq[String] = types.map(_.funcToString)
  def functionsEquals: Seq[String] = types.map(_.funcEquals)
  def functionsSize: Seq[String] = types.map(_.funcSize)
  def functionsFind: Seq[String] = types.map(_.funcFind)

  def functionsFoldLeft: Seq[String] = {
    val inTypes = allowedBaseTypes.map(QueueType.apply) ++ allowedBaseTypes.map(OptionType.apply).map(QueueType.apply)
    val outTypes = allowedBaseTypes ++ allowedBaseTypes.map(QueueType.apply)
    for {
      t <- inTypes
      out <- outTypes
    } yield t.funcFoldLeft(out)
  }

  def functionsTake: Seq[String] = types.map(_.funcTake)
  def functionsTakeWhile: Seq[String] = types.map(_.funcTakeWhile)
  def functionsTakeRight: Seq[String] = types.map(_.funcTakeRight)
}