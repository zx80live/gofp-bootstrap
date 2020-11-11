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
}
