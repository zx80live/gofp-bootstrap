package com.zx80live.gofp.bootstrap.functions

import com.zx80live.gofp.bootstrap.types.{ArrayType, BaseType, ListType, OptionType, Type}

object FuncEquals {
  def name(t: Type): String = s"${t.view}Equals"

  def contract(t: Type): String = s"func ${name(t)}(a, b ${t.raw}) bool"

  def body(t: Type): String = t match {
    case _: OptionType =>
      s"""
         |  if a.IsDefined() {
         |    if b.IsDefined() {
         |      return ${name(t.underlined)}(*a.value, *b.value)
         |    } else { return false }
         |  } else if b.IsDefined() {
         |    return false
         |  } else { return true }
         |""".stripMargin

    case _: ArrayType =>
      s"""
         |  len1 := len(a)
         |  if len1 != len(b) { return false }
         |
         |  for i := 0; i < len1; i ++ {
         |    if !${name(t.underlined)}(a[i], b[i]) { return false }
         |  }
         |  return true
         |""".stripMargin
    case _ =>
      s"""
         |return a == b""".stripMargin
  }

  def func(t: Type): String =
    s"""
       |${contract(t)} {
       |  ${body(t)}
       |}""".stripMargin

  def functions: Seq[String] = (BaseType.types ++ OptionType.types ++ ArrayType.types ++ ListType.types).map(func)
}
