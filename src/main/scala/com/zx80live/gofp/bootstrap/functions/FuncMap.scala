package com.zx80live.gofp.bootstrap.functions

import com.zx80live.gofp.bootstrap.types._

@deprecated
object FuncMap {

  //  def transformer(t: MonadType, out: Type): Transformer = Transformer(t.underlined, out)
  //
  //  def name(t: MonadType, out: Type): String = s"Map${out.view}To${t.view}"
  //
  //  def contract(t: MonadType, out: Type): String = s"/* ${t.raw}: ${t.underlined.raw}->${out.raw} */ func ${name(t, out)}(m ${t.raw}, f func(${t.underlined.raw}) ${out.raw}) ${t.rawFrom(out)}"
  //
  //  def body(t: MonadType, out: Type): String = t match {
  //    case _ : SliceType =>
  //      s"""acc := make([]${out.raw}, len(m)); for i, e := range m { acc[i] = f(e) }; return acc"""
  //    case _: OptionType =>s""" if m.IsDefined() { return ${OptionType(out).consView}(f(*m.value)) } else { return ${OptionType(out).emptyName} } """
  //    case l : ListType =>
  //      s"""acc := ${l.emptyNameFrom(out)}; xs := m; for xs.NonEmpty() { acc = acc.Cons(f(*xs.head)); xs = *xs.tail }; return acc.Reverse()"""
  //    case _ =>s"""  panic("Map() is unsupported for ${t.raw}->${out.raw}")"""
  //  }
  //
  //  def func(t: MonadType, out: Type): String =
  //    s"""
  //       |${contract(t, out)} { ${body(t, out)} }""".stripMargin
  //
  //
  //  def functionsOption: Seq[String] = {
  //    val underlinedTypes = OptionType.types
  //
  //    for {
  //      t <- underlinedTypes
  //      Transformer(in, out) <- Transformer.types
  //      if t.underlined == in
  //    } yield func(t, out)
  //  }
  //
  //  def functionsArray: Seq[String] = {
  //    val underlinedTypes = SliceType.types
  //
  //    for {
  //      t <- underlinedTypes
  //      Transformer(in, out) <- Transformer.types
  //      if t.underlined == in
  //    } yield func(t, out)
  //  }
  //
  //  def functionsList: Seq[String] = {
  //    val underlinedTypes = ListType.types
  //
  //    for {
  //      t <- underlinedTypes
  //      Transformer(in, out) <- Transformer.types
  //      if t.underlined == in
  //    } yield func(t, out)
  //  }
}
