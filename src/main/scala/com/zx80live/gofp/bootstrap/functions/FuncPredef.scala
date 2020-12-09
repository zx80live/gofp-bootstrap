package com.zx80live.gofp.bootstrap.functions

import com.zx80live.gofp.bootstrap.types.{ArrayType, BaseType, FutureType, ListType, OptionType, Tuple2Type, Type}

object FuncPredef {
  def funcRequire: String =
    s"""
       |func Require(e bool, msg string) {
       |  if !e { panic(fmt.Sprintf("Assertion error: %v", msg)) }
       |}
       |""".stripMargin

  def underlinedTypes: Seq[Type] = BaseType.reducedTypes ++ OptionType.types ++ ListType.types ++ ArrayType.types ++ Tuple2Type.types

  def reflectTypeName(t: Type): String = s"${t.view}ReflectType"

  def funcReflectTypes: Seq[String] = underlinedTypes.map { t =>
    s"""
       |var ${reflectTypeName(t)} reflect.Type = reflect.TypeOf((*${t.alias})(nil)).Elem()""".stripMargin
  }
}
