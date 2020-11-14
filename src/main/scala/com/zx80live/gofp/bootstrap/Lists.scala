package com.zx80live.gofp.bootstrap

object Lists {
  val types : Seq[String] = GoTypes.allTypes
  val names: Seq[(String, String)] = types.map(t => (t, toName(t)))

  def toName(goType: String): String = s"${GoTypes.toName(goType)}List"

  def toNilName(goType: String): String = s"Nil${GoTypes.toName(goType)}"

  def listDeclaration(goType: String): String =
    s"""
       |type ${toName(goType)} struct {
       |	head    *$goType
       |	tail    *${toName(goType)}
       |}
       |""".stripMargin

  val lists: Seq[String] = types map listDeclaration

  val listsNil: Seq[String] = types.map { t =>
    s"""
       |var ${toNilName(t)} ${Lists.toName(t)} = ${Lists.toName(t)} {nil, nil}""".stripMargin
  }

  val listsMake: Seq[String] = types.map { t =>
    s"""
       |func Make${toName(t)}(elements ...$t) ${toName(t)} {
       |	l := ${toNilName(t)}
       |	for i := len(elements) - 1; i >= 0; i-- {
       |		l = l.Cons(elements[i])
       |	}
       |	return l
       |}""".stripMargin
  }

  val listsEmptyNonEmpty: Seq[String] = types.map { t =>
    s"""
       |func (l ${toName(t)}) IsEmpty() bool { return l.head == nil && l.tail == nil }
       |func (l ${toName(t)}) IsNotEmpty() bool { return !l.IsEmpty() }""".stripMargin
  }

  val listsCopy: Seq[String] = types.map { t =>
    s"""
       |func (l ${toName(t)}) Copy() ${toName(t)} {
       |	if l.IsEmpty() {
       |		return l
       |	} else {
       |		tail := l.tail.Copy()
       |		return ${toName(t)}{
       |			head:    l.head,
       |			tail:    &tail,
       |		}
       |	}
       |}""".stripMargin
  }

  val listsCons: Seq[String] = {
    types.map { t =>
      s"""
         |func (l ${toName(t)}) Cons(e $t) ${toName(t)} {
         |	tail := l.Copy()
         |	xs := ${toName(t)}{
         |		head:    &e,
         |		tail:    &tail,
         |	}
         |	return xs
         |}""".stripMargin
    }
  }

  val listsHead: Seq[String] = types.map { t =>
    s"""
       |func (l ${toName(t)}) Head() $t { return *l.head }""".stripMargin
  }

  val listsTail: Seq[String] = types.map { t =>
    s"""
       |func (l ${toName(t)}) Tail() ${toName(t)} { return l.tail.Copy() }""".stripMargin
  }

  val listsForeach: Seq[String] = types.map { t =>
    s"""
       |func (l ${toName(t)}) Foreach(f func($t)) {
       |	if l.IsNotEmpty() {
       |    f(*l.head)
       |    l.tail.Foreach(f)
       |  }
       |}""".stripMargin
  }

  val listsReverse: Seq[String] = types.map { t =>
    s"""
       |func (l ${toName(t)}) Reverse() ${toName(t)} {
       |	xs := ${toNilName(t)}
       |	l.Foreach(func(e $t) {
       |		xs = xs.Cons(e)
       |	})
       |	return xs
       |}
       |""".stripMargin
  }

  //TODO optimize
  val listsFilter: Seq[String] = types.map { t =>
    s"""
       |func (l ${toName(t)}) Filter(p ${Predicates.toName(t)}) ${toName(t)} {
       |  acc := ${toNilName(t)}
       |  xs := &l
       |  for xs.IsNotEmpty() {
       |    if p(*xs.head) {
       |      acc = acc.Cons(*xs.head)
       |    }
       |    xs = xs.tail
       |  }
       |  return acc.Reverse()
       |}""".stripMargin
  }

  //TODO optimize
  val listsMap: Seq[String] = for {
    t1 <- types
    t2 <- types
  } yield {
    s"""
       |func (l ${toName(t1)}) Map${GoTypes.toName(t2)}(f ${Functors.toName(t1, t2)}) ${toName(t2)} {
       |  acc := ${toNilName(t2)}
       |  xs := &l
       |  for xs.IsNotEmpty() {
       |    acc = acc.Cons(f(*xs.head))
       |    xs = xs.tail
       |  }
       |  return acc.Reverse()
       |}""".stripMargin
  }

  val listsSize: Seq[String] = types.map { t =>
    s"""
       |func (l ${toName(t)}) Size() int {
       |  count := 0
       |  xs := l
       |  for xs.IsNotEmpty() {
       |    count ++
       |    xs = *xs.tail
       |  }
       |  return count
       |}
       |""".stripMargin
  }
}
