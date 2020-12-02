package com.zx80live.gofp.bootstrap.types

import com.zx80live.gofp.bootstrap.types.BaseType.GoString

//TODO refactoring
case class Transformer(in: Type, out: Type) {
  def raw: String = Transformer.name(in, out)

  def name: String = raw

  def declaration: String =
    s"""
       |type $name func(in ${in.raw}) ${out.raw}""".stripMargin

  def emptyName: String = s"Empty${in.view}To${out.view}"

  def identityName: String = s"${in.view}Identity"

  def emptyDeclaration: String =
    s"""
       |var $emptyName func(${in.raw}) ${out.raw} = func(in ${in.raw}) ${out.raw} { return in }""".stripMargin

  def identity: String =
    s"""
       |var $identityName func(${in.raw}) ${in.raw} = func(in ${in.raw}) ${in.raw} { return in }""".stripMargin

  def funcToRegexGroups: String =
    s"""
       |func RegexGroups(r *regexp.Regexp) func(string) ${ArrayType(GoString).raw} {
       |  return func(s string) ${ArrayType(GoString).raw} {
       |     return (r.FindStringSubmatch(s))
       |  }
       |}""".stripMargin

  def funcToStringRegexGroups: String =
    s"""
       |func StringRegexGroups(s string) func(string) ${ArrayType(GoString).raw} {
       |  return RegexGroups(regexp.MustCompile(s))
       |}""".stripMargin
}

object Transformer {
  def name(in: Type, out: Type): String = s"${in.view}To${out.view}Transformer"

  def emptyDeclarations: Seq[String] = (BaseType.types ++ OptionType.types ++ ArrayType.types ++ ListType.types).map(t => Transformer(t, t).emptyDeclaration)

  def types: Seq[Transformer] = for {
    in <- inTypes
    out <- outTypes
  } yield Transformer(in, out)

  def declarations: Seq[String] = types.map(_.declaration)

  private def allowedTypes: Seq[Type] =
    BaseType.types
//    Seq(
//      BaseType.GoBool,
//      BaseType.GoString,
//      BaseType.GoInt,
//      BaseType.GoInt64,
//      BaseType.GoUInt,
//      BaseType.GoUInt64,
//      BaseType.GoByte,
//      BaseType.GoRune,
//      BaseType.GoFloat32,
//      BaseType.GoFloat64,
//      BaseType.GoAny
//    )

  private def inTypes: Seq[Type] = allowedTypes ++ allowedTypes.map(OptionType.apply) ++ allowedTypes.map(ArrayType.apply) ++ allowedTypes.map(ListType.apply)

  private def outTypes: Seq[Type] = inTypes


  def identities: Seq[String] = {
    val types = BaseType.reducedTypes ++ BaseType.reducedTypes.map(OptionType.apply) ++ BaseType.reducedTypes.map(ListType.apply)
    types.map(t => Transformer(t, t)).map(_.identity)
  }

  def stringTransformers: Seq[String] = Seq(Transformer(GoString, GoString).funcToRegexGroups) ++ Seq(Transformer(GoString, GoString).funcToStringRegexGroups)
}