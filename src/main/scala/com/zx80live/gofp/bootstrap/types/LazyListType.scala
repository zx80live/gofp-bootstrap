package com.zx80live.gofp.bootstrap.types

import com.zx80live.gofp.bootstrap.types.BaseType.GoAny

case class LazyListType(underlined: Type) extends MonadType with Traversable {
  override def setUnderlined(t: Type): MonadType = ???

  override def emptyName: String = s"Nil$view"

  def emptyStateName: String = s"Empty$stateName"

  override def emptyDeclaration: String =
    s"""
       |var $emptyStateName = $stateName { nil, nil }
       |var $emptyName $raw = $raw { nil }
       |""".stripMargin

  override def consView: String = ???

  override def funcCons: String =
    s"""
       |func Mk$stateName(head ${LazyType(underlined).raw}, tail $raw) $stateName { return $stateName { &head, &tail } }
       |func Mk$view(state $lazyStateName) $raw { return $raw { &state } }
       |""".stripMargin

  def funcPrepend: String =
    s"""
       |func (l $raw) Cons(i ${LazyType(underlined).raw}) $raw {
       |	xs := $raw { l.state }
       |	state := func() $stateName { return $stateName{&i, &xs} }
       |
       |	return $raw {&state}
       |}""".stripMargin

  override def funcFilter: String = {
    s"""
       |func (l $raw) Filter(p func(e ${underlined.raw}) bool) $raw {
       |	newState := func() $stateName {
       |		var xs = l
       |		var h ${LazyType(underlined).raw}
       |		var found = false
       |
       |		for !found && xs.NonEmpty() {
       |			s := (*xs.state)()
       |			h = (*s.head).Eval()
       |			found = p(h.Value())
       |			xs = *s.tail
       |		}
       |
       |		if found {
       |			t := xs.Filter(p)
       |			return $stateName{&h, &t}
       |		} else {
       |			return $emptyStateName
       |		}
       |	}
       |
       |	return $raw{&newState}
       |}""".stripMargin
  }

  override def funcMap(out: Type): String = {
    val l = LazyListType(out)
    s"""
       |func (l $raw) Map${out.view}(f func(e ${underlined.raw}) ${out.raw}) ${l.raw} {
       |	newState := func() ${l.stateName} {
       |		state := (*l.state)()
       |		h := *state.head
       |		mappedValue := func() ${out.raw} {
       |			return f(h.Value())
       |		}
       |		mappedH := ${LazyType(out).consName}(mappedValue)
       |		t := state.tail.Map${out.view}(f)
       |
       |		return ${l.stateName}{&mappedH, &t}
       |	}
       |	return ${l.raw}{&newState}
       |}""".stripMargin
  }

  override def funcToList: String = ???

  override def funcReduce: String = ???

  override def funcFlatMap(out: MonadType): String = ???

  override def funcFoldLeft(out: Type): String = ???

  override def funcFind: String = ???

  override def funcZip(m: MonadType): String = ???

  override def funcZipAll(m: MonadType): String = ???

  override def funcZipWithIndex: String = ???

  override def funcSize: String = ???

  override def funcForeach: String = ???

  override def funcTake: String =
    s"""
       |func (l $raw) Take(n int) $raw {
       |	if n <= 0 {
       |		return $emptyName
       |	}
       |	newState := func() $stateName {
       |		state := (*l.state)()
       |		t := state.tail.Take(n - 1)
       |		return $stateName{state.head, &t}
       |	}
       |	return $raw{&newState}
       |}""".stripMargin

  override def funcTakeWhile: String = ???

  override def funcTakeRight: String = ???

  override def funcDrop: String = ???

  override def funcDropRight: String = ???

  override def funcDropWhile: String = ???

  override def funcHead: String =
    s"""
       |func (l $raw) Head() (${underlined.raw}, $raw) {
       |	if l.IsEmpty() {
       |		panic("can't get head from empty list")
       |	} else {
       |		s := (*l.state)()
       |		return s.head.Eval().Value(), *s.tail
       |	}
       |}""".stripMargin

  override def funcHeadOption: String =
    s"""
       |func (l $raw) HeadOption() (${OptionType(underlined).raw}, $raw) {
       |	if l.IsEmpty() {
       |		return ${OptionType(underlined).emptyName}, $emptyName
       |	} else {
       |		s := (*l.state)()
       |		return ${OptionType(underlined).consView}(s.head.Eval().Value()), *s.tail
       |	}
       |}
       |""".stripMargin

  override def funcTail: String = ???

  override def iteratorName: String = ???

  override def iteratorDeclaration: String = ???

  override def funcIterator: String = ???

  override def funcHasNext: String = ???

  override def funcNext: String = ???

  override def raw: String = s"${underlined.view}LazyList"

  override def alias: String = raw

  override def view: String = raw

  def lazyStateName: String = s"${underlined.view}LazyState"

  def stateName: String = s"${underlined.view}State"

  override def declaration: String =
    s"""
       |type $lazyStateName = func() $stateName
       |type $stateName struct {	head *${LazyType(underlined).raw};	tail *$raw }
       |type $raw struct{ state *$lazyStateName }
       |""".stripMargin

  def funcIsEmpty: String =
    s"""
       |func (l $raw) IsEmpty() bool { return l == $emptyName }""".stripMargin

  def funcNonEmpty: String =
    s"""
       |func (l $raw) NonEmpty() bool { return !l.IsEmpty() }""".stripMargin

  override def funcEquals: String = ???

  override def funcToString: String =
    s"""
       |func (l $raw) ToString() string { return "$view(?)" }""".stripMargin
}

object LazyListType {
  def allowedBaseTypes: Seq[Type] = BaseType.reducedTypes ++ Seq(Tuple2Type.defaultType)

  def types: Seq[LazyListType] = LazyType.types.map(_.t).map(LazyListType.apply)

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

  def functionsFilter: Seq[String] = types.map(_.funcFilter)

  def functionsMap: Seq[String] = transformers.map(t => LazyListType(t.in).funcMap(t.out))

  def functionsTake: Seq[String] = types.map(_.funcTake)

  def functionsToString: Seq[String] = types.map(_.funcToString)
}
