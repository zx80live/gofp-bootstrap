package com.zx80live.gofp.bootstrap.types

import com.zx80live.gofp.bootstrap.types.BaseType.{GoAny, GoString}

case class BaseType(value: String) extends Type {

  override def raw: String = value

  override def view: String = raw.capitalize

  override def alias: String = view

  override def declaration: String = ""

  override def funcEquals: String = ""

  override def funcToString: String = ""

  def boxedRaw: String = view

  def boxedView: String = boxedRaw

  def boxedDeclaration: String = if (this != GoAny) {
    s"""
       |type $boxedRaw $raw""".stripMargin
  } else ""

  def funcUnderlined: String = this match {
    case GoAny => ""
    case _ =>
      s"""
         |func (e $boxedRaw) Underlined() $raw { return $raw(e) }""".stripMargin
  }

  def funcToInt: String = this match {
    case GoString =>
      s"""
         |func (s $boxedRaw) ToInt() Int {
         |  i, err := strconv.Atoi(s.Underlined())
         |  if err != nil { panic(fmt.Sprintf("%v to Int parse error", s)) } else { return Int(i) } }""".stripMargin
    case _ => ""
  }

  def funcToIntOption: String = this match {
    case GoString =>
      s"""
         |func (s $boxedRaw) ToIntOption() IntOption {
         |  i, err := strconv.Atoi(s.Underlined())
         |  if err != nil { return NoneInt } else { return IntOpt(i) } }""".stripMargin
    case _ => ""
  }
}

object BaseType {
  val GoBool: BaseType = BaseType("bool")
  val GoString: BaseType = BaseType("string")
  val GoInt: BaseType = BaseType("int")
  val GoInt8: BaseType = BaseType("int8")
  val GoInt16: BaseType = BaseType("int16")
  val GoInt32: BaseType = BaseType("int32")
  val GoInt64: BaseType = BaseType("int64")
  val GoUInt: BaseType = BaseType("uint")
  val GoUInt8: BaseType = BaseType("uint8")
  val GoUInt16: BaseType = BaseType("uint16")
  val GoUInt32: BaseType = BaseType("uint32")
  val GoUInt64: BaseType = BaseType("uint64")
  val GoUIntPtr: BaseType = BaseType("uintptr")
  val GoByte: BaseType = BaseType("byte")
  val GoRune: BaseType = BaseType("rune")
  val GoFloat32: BaseType = BaseType("float32")
  val GoFloat64: BaseType = BaseType("float64")
  val GoComplex64: BaseType = BaseType("complex64")
  val GoComplex128: BaseType = BaseType("complex128")
  val GoAny: BaseType = BaseType("Any")

  def types: Seq[BaseType] = Seq(
    GoBool,
    GoString,
    GoInt,
    GoInt8,
    GoInt16,
    GoInt32,
    GoInt64,
    GoUInt,
    GoUInt8,
    GoUInt16,
    GoUInt32,
    GoUInt64,
    GoUIntPtr,
    GoByte,
    GoRune,
    GoFloat32,
    GoFloat64,
    GoComplex64,
    GoComplex128,
    GoAny)

  def reducedTypes: Seq[BaseType] = Seq(
    GoBool,
    GoString,
    GoInt,
    GoInt64,
    GoByte,
    GoRune,
    GoFloat32,
    GoFloat64,
    GoAny
  )

  def boxedDeclarations: Seq[String] = types.map(_.boxedDeclaration)

  def functionsUnderlined: Seq[String] = types.map(_.funcUnderlined)

  def functionsConverters: Seq[String] = types.map(_.funcToInt) ++ types.map(_.funcToIntOption)
}
