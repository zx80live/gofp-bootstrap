package com.zx80live.gofp.bootstrap.functions

import com.zx80live.gofp.bootstrap.types.{ArrayType, BaseType, ListType, OptionType, Type}

object FuncEquals {
  def name(t: Type): String = s"${t.view}Equals"

  def contract(t: Type): String = s"func ${name(t)}(a, b ${t.raw}) bool"

  def body(t: Type): String = t match {
    case _: BaseType =>
      s""" return a == b """
    case a: ArrayType =>
      s"""
         |  len1 := len(a)
         |  if len1 != len(b) { return false }
         |  for i, e := range a {
         |    if ${FuncEquals.name(a.underlined)}(e, b[i]) { return false }
         |  }
         |  return true""".stripMargin
    case _ =>
      s""" return a.Equals(b) """
  }

  def func(t: Type): String =
    s"""
       |${contract(t)} { ${body(t)} }""".stripMargin

  def functions: Seq[String] = (BaseType.types ++ OptionType.types ++ ArrayType.types ++ ListType.types).map(func)
}
