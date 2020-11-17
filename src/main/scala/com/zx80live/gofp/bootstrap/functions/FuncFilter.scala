package com.zx80live.gofp.bootstrap.functions

import com.zx80live.gofp.bootstrap.types.{ArrayType, ListType, OptionType, Predicate, TraversableType}

object FuncFilter {

  def name(t: TraversableType): String = s"Filter${t.view}"

  def contract(t: TraversableType): String = s"func ${name(t)}(l ${t.raw}, p ${Predicate.name(t.underlined)}) ${t.raw}"

  def body(t: TraversableType): String = t match {
    case a : ArrayType =>
      s"""
         |  acc := make(${t.raw}, len(l))
         |  i := 0
         |  for _, e := range l {
         |    if p(e) {
         |      acc[i] = e
         |      i ++
         |    }
         |  }
         |  return acc""".stripMargin
    case l : ListType =>
      s"""
         |  acc := ${l.nilName}
         |  xs := l
         |  for xs.NonEmpty() {
         |    if p(*xs.head) {
         |      acc = acc.Cons(*xs.head)
         |    }
         |    xs = *xs.tail
         |  }
         |  return acc.Reverse()""".stripMargin
    case o : OptionType =>
      s"""
         |  if l.IsDefined() {
         |    if p(*l.value) {
         |      return ${o.consView}(*l.value)
         |    } else { return ${o.noneName} }
         |  } else { return ${o.noneName} }""".stripMargin
    case _ => s"""  panic("Filter() is not supported for ${t.raw}")"""
  }

  def func(t: TraversableType): String =
    s"""
       |${contract(t)} {
       |  ${body(t)}
       |}""".stripMargin

  def functions: Seq[String] = (ArrayType.types ++ OptionType.types ++ ListType.types).map(func)
}
