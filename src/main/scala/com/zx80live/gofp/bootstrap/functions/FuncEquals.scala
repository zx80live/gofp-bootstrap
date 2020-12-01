package com.zx80live.gofp.bootstrap.functions

import com.zx80live.gofp.bootstrap.types.{ArrayType, BaseType, ListType, OptionType, Type}

object FuncEquals {
  def name(t: Type): String = s"${t.view}Equals"

  def contract(t: Type): String = s"func ${name(t)}(a, b ${t.alias}) bool"

  def body(t: Type): String = t match {
    case _: BaseType =>
      s""" return a == b """
    case _ =>
      s""" return a.Equals(b) """
  }

  def func(t: Type): String =
    s"""
       |${contract(t)} { ${body(t)} }""".stripMargin

  def functions: Seq[String] = (BaseType.types ++ OptionType.types ++ ArrayType.types ++ ListType.types).map(func)
}
