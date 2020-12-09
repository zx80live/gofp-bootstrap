package com.zx80live.gofp.bootstrap.functions

import com.zx80live.gofp.bootstrap.types._

object FuncToString {

  def name(t: Type): String = s"${t.view}ToString"

  def contract(t: Type): String = s"func ${name(t)}(o ${t.alias}) String"

  def body(t: Type): String = t match {
    case BaseType.GoAny => toStringAny
    case _: BaseType => s""" return String(fmt.Sprintf("%v", o)) """
    case _ => s""" return o.ToString() """
  }

  def func(t: Type): String =
    s"""
       |${contract(t)} {
       |  ${body(t)}
       |}""".stripMargin

  def functions: Seq[String] = (BaseType.types ++ OptionType.types ++ ArrayType.types ++ ListType.types ++ Tuple2Type.types).map(func)

  private def toStringAny: String = {
    val conditions = FuncPredef.underlinedTypes.map { t =>
      s"""
         |if reflect.TypeOf(o) == ${FuncPredef.reflectTypeName(t)} { s1 = ${FuncToString.name(t)}(o.(${t.alias})) }""".stripMargin
    }

    val conditionsFull = conditions.mkString("", " else ", s""" else { s1 = String(fmt.Sprintf("%v", o))  }""")

    s"""
       |var s1 String
       |$conditionsFull
       |return String(s1)
       |""".stripMargin
  }
}
