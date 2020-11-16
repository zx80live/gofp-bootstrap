package com.zx80live.gofp.bootstrap.refactored.functions

import com.zx80live.gofp.bootstrap.refactored.types.{ArrayType, BaseType, OptionType, Type}

object FuncEquals {
  def name(t: Type): String = s"${t.view}Equals"

  def declaration(t: Type): String = s"func ${name(t)}(a, b ${t.raw}) bool"

  def func(t: Type): String = t match {
    case _: BaseType =>
      s"""
         |${declaration(t)} { return a == b }
         |""".stripMargin
    case _: OptionType =>
      s"""
         |${declaration(t)} {
         |  if a.IsDefined() {
         |    if b.IsDefined() {
         |      return ${name(t.underlined)}(*a.value, *b.value)
         |    } else { return false }
         |  } else if b.IsDefined() {
         |    return false
         |  } else { return true }
         |}
         |""".stripMargin

    case _: ArrayType =>
      s"""
         |${declaration(t)} {
         |  len1 := len(a)
         |  if len1 != len(b) { return false }
         |
         |  for i := 0; i < len1; i ++ {
         |    if !${name(t.underlined)}(a[i], b[i]) { return false }
         |  }
         |  return true
         |}
         |""".stripMargin
  }

  val functions: Seq[String] = (BaseType.types ++ OptionType.types ++ ArrayType.types).map(func)
}
