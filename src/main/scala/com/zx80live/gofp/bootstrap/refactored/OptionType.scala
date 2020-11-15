package com.zx80live.gofp.bootstrap.refactored

/*
 raw:
   OptionInt
   OptionOptionInt

 declare:
   type OptionInt struct { value *int }
   type OptionOptionInt struct { value *OptionInt }


 cons:
   var e IntOption = Int(1)
   var e IntOptionOption = IntInt(1)

 usage:
   func IntOptionToString(e IntOption) string
   func IntOptionOptionToString(e IntOptionOption) string

 extension:
   func (e IntOption) ToString() string
   func (e IntOptionOption) ToString() string
   func (e IntOption) MapInt(f IntToInt) OptionInt
   func (e IntOption) FlatMapIntOption(f IntToIntOption) OptionInt

 */
case class OptionType(underlined: Type) extends Type {
  override def raw: String = s"${underlined.view}Option"

  override def view: String = raw

  override def declare: String = s"type $raw struct { value *${underlined.raw} }"

  override def consView: String = underlined match {
    case _: BaseType => s"${underlined.view}"
    case _: OptionType => s"${underlined.consView}${underlined.core.view}"
  }
}

object OptionType {
  val underlinedTypes: Seq[Type] = BaseType.types ++ BaseType.types.map(OptionType.apply)
  val types : Seq[Type] = underlinedTypes.map(OptionType.apply)
}