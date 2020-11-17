package com.zx80live.gofp.bootstrap.functions

import com.zx80live.gofp.bootstrap.types.{ArrayType, BaseType, ListType, OptionType, TraversableType, Type}

object FuncToString {

  def name(t: Type): String = s"${t.view}ToString"

  def contract(t: Type): String = s"func ${name(t)}(o ${t.raw}) string"

  def body(t: Type): String = t match {
    case t: ArrayType =>
      s"""  return ${FuncMkString.name(t)}(o, "[", ",", "]")"""
    case t: ListType =>
      s"""  return ${FuncMkString.name(t)}(o, "List(", ",", ")")"""
    case t: OptionType =>
      s"""
         |  if o.IsDefined() { return fmt.Sprintf("%v%v%v", "Some(", ${name(t.underlined)}(*o.value), ")")
         |  } else { return "None" }""".stripMargin
    case _  => s"""  return fmt.Sprintf("%v", o)"""
  }

  def func(t: Type): String =
    s"""
       |${contract(t)} {
       |  ${body(t)}
       |}""".stripMargin

  def functions: Seq[String] = (BaseType.types ++ OptionType.types ++ ArrayType.types ++ ListType.types).map(func)
}
