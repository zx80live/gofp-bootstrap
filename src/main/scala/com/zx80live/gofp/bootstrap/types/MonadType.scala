package com.zx80live.gofp.bootstrap.types

import com.zx80live.gofp.bootstrap.functions.{FuncFilter, FuncMap}

trait MonadType extends TraversableType {

  def funcCons: String

  def funcForeach: String

  def funcFilter: String =
    s"""
       |func (m $raw) Filter(p ${Predicate.name(underlined)}) $raw { return ${FuncFilter.name(this)}(m, p) }""".stripMargin

  def funcMap(out: Type): String =
    s"""
       |func (m $raw) Map${out.view}(f func(${underlined.raw}) ${out.raw}) ${rawFrom(out)} { return ${FuncMap.name(this, out)}(m, f) }""".stripMargin


  def funcFlatMap(out: MonadType): String = (this, out) match {
    case (_ : OptionType, o2: OptionType) =>
      s"""
         |func (m $raw) FlatMap${out.underlined.view}(f func(${underlined.raw}) ${rawFrom(out.underlined)}) ${rawFrom(out.underlined)} { if m.IsDefined() { return f(*m.value) } else { return ${o2.noneName} } }""".stripMargin
    case (l1 : ListType, l2: ListType) =>
      s"""
         |func (m $raw) FlatMap${out.underlined.view}(f func(${underlined.raw}) ${rawFrom(out.underlined)}) ${rawFrom(out.underlined)} { if m.IsEmpty() { return ${l2.nilName} } else { acc := ${l2.nilName}; xs := m; for xs.NonEmpty() { exs := f(*xs.head); for exs.NonEmpty() { acc = acc.Cons(*exs.head); exs = *exs.tail }; xs = *xs.tail }; return acc.Reverse() } }""".stripMargin
    case _ =>
      s"""
         |func (m $raw) FlatMap${out.underlined.view}(f func(${underlined.raw}) ${rawFrom(out.underlined)}) ${rawFrom(out.underlined)} { panic("does not supported: $raw.FlatMap(${out.raw})") } }
         |""".stripMargin
  }


}
