package com.zx80live.gofp.bootstrap.refactored.types

case class Transformer(in: Type, out: Type) {
  def raw: String = Transformer.name(in, out)

  def name: String = raw

  def declaration: String =
    s"""
       |type $name func(in ${in.raw}) ${out.raw}""".stripMargin

  def emptyName: String = s"${in.view}EmptyTransformer"

  def emptyDeclaration: String =
    s"""
       |var $emptyName func(${in.raw}) ${in.raw} = func(in ${in.raw}) ${in.raw} { return in }""".stripMargin
}

object Transformer {
  def name(in: Type, out: Type): String = s"${in.view}${out.view}Transformer"

  val emptyDeclarations: Seq[String] = (BaseType.types ++ OptionType.types ++ ArrayType.types ++ ListType.types).map(t => Transformer(t, t).emptyDeclaration)

  val types: Seq[Transformer] = for {
    in <- inTypes
    out <- outTypes
  } yield Transformer(in, out)

  val declarations: Seq[String] = types.map(_.declaration)

  private def allowedTypes: Seq[BaseType] =
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

  private def inTypes: Seq[Type] = allowedTypes ++ allowedTypes.map(OptionType.apply) ++ allowedTypes.map(ListType.apply)

  private def outTypes: Seq[Type] = inTypes
}