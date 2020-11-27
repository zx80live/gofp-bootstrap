package com.zx80live.gofp.bootstrap.types

import scala.annotation.tailrec

trait MonadType extends Type {
  def underlined: Type

  def emptyName: String

  def emptyDeclaration: String

  def consView: String

  def nestedLevel: Int = {
    @tailrec
    def loop(t: Type, level: Int): Int = t match {
      case ct: MonadType => loop(ct.underlined, level + 1)
      case _ => level
    }

    loop(this, 0)
  }

  def core: Type = underlined match {
    case ct: MonadType => ct.core
    case _ => this
  }

  def funcCons: String

  def funcHead: String

  def funcHeadOption: String

  def funcTail: String

  def funcSize: String

  def funcForeach: String

  def funcFilter: String

  def funcDrop: String

  def funcMap(out: Type): String

  def funcToList: String

  //  def flatMapDeclaration(out: MonadType): String = s"func (m $raw) FlatMap${out.view}(f func(${underlined.raw}) ${out.raw}) ${rawFrom(out.underlined)}"
  //
  //  def funcFlatMap(out: MonadType): String = (this, out) match {
  //    case (_ : OptionType, o2: OptionType) =>
  //      s"""
  //         |${flatMapDeclaration(out)} { if m.IsDefined() { return f(*m.value) } else { return ${o2.emptyName} } }""".stripMargin
  //    case (_ : ListType, l2: ListType) =>
  //      s"""
  //         |${flatMapDeclaration(out)} { if m.IsEmpty() { return ${l2.emptyName} } else { acc := ${l2.emptyName}; xs := m; for xs.NonEmpty() { exs := f(*xs.head); for exs.NonEmpty() { acc = acc.Cons(*exs.head); exs = *exs.tail }; xs = *xs.tail }; return acc.Reverse() } }""".stripMargin
  //    case _ =>
  //      s"""
  //         |${flatMapDeclaration(out)} { panic("does not supported: $raw.FlatMap(${out.raw})") } }
  //         |""".stripMargin
  //  }
}
