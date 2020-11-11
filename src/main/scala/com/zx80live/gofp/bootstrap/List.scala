package com.zx80live.gofp.bootstrap

object List {

  def toName(goType: String): String = s"List${GoTypes.toName(goType)}"
  def toNilName(goType: String): String = s"Nil${GoTypes.toName(goType)}"

  def listDeclaration(goType: String): String =
    s"""
      |type ${toName(goType)} struct {
      |	head    $goType
      |	tail    *${toName(goType)}
      |	functor ${Functors.toName(goType, goType)}
      |}
      |""".stripMargin

  val lists: Seq[String] = GoTypes.allTypes map listDeclaration

  val listsMake: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
       |func Make${toName(t)}(elements ...$t) ${toName(t)} {
       |	l := ${toNilName(t)}
       |	for i := len(elements) - 1; i >= 0; i-- {
       |		l = l.Cons(elements[i])
       |	}
       |	return l
       |}""".stripMargin
  }

  val listsNil: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
      |var ${toNilName(t)} ${toName(t)} = ${toName(t)}{nil, nil, ${Functors.toEmptyFunctorName(t)}}""".stripMargin
  }

  val listsEmptyNonEmpty: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
      |func (l ${toName(t)}) IsEmpty() bool { return reflect.DeepEqual(l, ${toNilName(t)}) }
      |func (l ${toName(t)}) IsNotEmpty() bool { return !l.IsEmpty() }""".stripMargin
  }

  val listsCopy: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
      |func (l ${toName(t)}) Copy() ${toName(t)} {
      |	if l.IsEmpty() {
      |		return l
      |	} else {
      |		tail := l.tail.Copy()
      |		return ${toName(t)}{
      |			head:    l.head,
      |			tail:    &tail,
      |			functor: ${Functors.toEmptyFunctorName(t)},
      |		}
      |	}
      |}""".stripMargin
  }

  val listsCons: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
      |func (l ${toName(t)}) Cons(e $t) ${toName(t)} {
      |	tail := l.Copy()
      |	xs := ${toName(t)}{
      |		head:    e,
      |		tail:    &tail,
      |		functor: ${Functors.toEmptyFunctorName(t)},
      |	}
      |	return xs
      |}""".stripMargin
  }

  val listsHead: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
      |func (l ${toName(t)}) Head() $t { return l.head }""".stripMargin
  }

  val listsTail: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
       |func (l ${toName(t)}) Tail() ${toName(t)} { return l.tail.Copy() }""".stripMargin
  }

  val listsForeach: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
       |func (l ${toName(t)}) Foreach(f func($t)) {
       |	if l.head != nil {
       |		processed := l.functor(l.head)
       |		if processed != nil {
       |			f(processed)
       |		}
       |	}
       |
       |	if l.tail != nil {
       |		l.tail.mapHead(l.functor).Foreach(f)
       |	}
       |}""".stripMargin
  }

  val listsReverse: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
       |func (l ${toName(t)}) Reverse() ${toName(t)} {
       |	xs := Nil
       |	l.Foreach(func(e $t) {
       |		xs = xs.Cons(e)
       |	})
       |	return xs
       |}
       |""".stripMargin
  }

  val lists_mapHead: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
      |func (l ${toName(t)}) mapHead(f ${Functors.toName(t, t)}) ${toName(t)} {
      |	return ${toName(t)}{
      |		head: l.head,
      |		tail: l.tail,
      |		functor: func(e $t) $t {
      |			processed := l.functor(e)
      |			if processed == nil {
      |				return nil
      |			} else {
      |				return f(processed)
      |			}
      |		},
      |	}
      |}
      |""".stripMargin
  }
}
