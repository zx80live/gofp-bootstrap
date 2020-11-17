package com.zx80live.gofp.bootstrap.types

/*
 raw:
   int, string, bool

 view:
   Int, String, Bool

 usage:
   func <view>ToString(e <this.raw>) <TO.raw>
   func IntToString(e int) string

   func <view>Equals(a, b <raw>) bool
   func IntEquals(a, b int) bool
 */
case class BaseType(value: String) extends Type {

  override def underlined: Type = SuperType

  override def raw: String = value

  override def view: String = raw.capitalize
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
}
