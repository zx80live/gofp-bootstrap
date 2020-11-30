package com.zx80live.gofp.bootstrap.types

case class Transformer(in: Type, out: Type) {
  @deprecated
  def raw: String = Transformer.name(in, out)

  @deprecated
  def name: String = raw

  @deprecated
  def declaration: String =
    s"""
       |type $name func(in ${in.raw}) ${out.raw}""".stripMargin

  @deprecated
  def emptyName: String = s"Empty${in.view}To${out.view}"

  @deprecated
  def emptyDeclaration: String =
    s"""
       |var $emptyName func(${in.raw}) ${out.raw} = func(in ${in.raw}) ${out.raw} { return in }""".stripMargin
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
}