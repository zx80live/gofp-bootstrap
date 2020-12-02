package com.zx80live.gofp.bootstrap.functions

import com.zx80live.gofp.bootstrap.types.{ArrayType, BaseType, ListType, MonadType, OptionType, Type}

object FuncPrintln {

  def funcPrintln(t: Type): String = t match {
    case _: BaseType =>
      s"""
         |func Println${t.view}(e ${t.raw}) { fmt.Println(${FuncToString.name(t)}(${t.alias}(e))) }""".stripMargin

    case _ =>
      s"""
         |func Println${t.view}(e ${t.raw}) { fmt.Println(${FuncToString.name(t)}(e)) } """.stripMargin
  }

  def functionsPrintln: Seq[String] = (BaseType.reducedTypes ++ OptionType.types ++ ArrayType.types ++ ListType.types).map(funcPrintln)
}
