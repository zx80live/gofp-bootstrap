package com.zx80live.gofp.bootstrap.functions

import com.zx80live.gofp.bootstrap.types._

object FuncToString {

  def name(t: Type): String = s"${t.view}ToString"

  def contract(t: Type): String = s"func ${name(t)}(o ${t.alias}) String"

  def body(t: Type): String = t match {
    case _: BaseType  => s""" return String(fmt.Sprintf("%v", o)) """
    case _ => s""" return o.ToString() """
  }

  def func(t: Type): String =
    s"""
       |${contract(t)} {
       |  ${body(t)}
       |}""".stripMargin

  def functions: Seq[String] = (BaseType.types ++ OptionType.types ++ ArrayType.types ++ ListType.types).map(func)
}
