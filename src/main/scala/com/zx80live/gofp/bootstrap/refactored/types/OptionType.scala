package com.zx80live.gofp.bootstrap.refactored.types

import com.zx80live.gofp.bootstrap.refactored.functions.FuncEquals

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
case class OptionType(underlined: Type) extends MonadType {
  override def raw: String = s"${underlined.view}Option"

  override def view: String = raw

  override def declaration: String =
    s"""
       |type $raw struct { value *${underlined.raw} }""".stripMargin

  override def consView: String = underlined match {
    case _: BaseType => s"${underlined.view}"
    case _: OptionType => s"${underlined.consView}${underlined.core.view}"
  }

  def noneName: String = s"None$view"

  def noneDeclaration: String =
    s"""
       |var $noneName $raw = $raw { nil }""".stripMargin

  def funcIsDefined: String =
    s"""
       |func (o $raw) IsDefined() bool { return o == $noneName }
       |""".stripMargin

  def funcIsEmpty: String =
    s"""
       |func (o $raw) IsEmpty() bool { return !o.IsDefined() }
       |""".stripMargin

  def funcEquals: String =
    s"""
       |func (o1 $raw) Equals(o2 $raw) bool { return ${FuncEquals.name(this)}(o1, o2) }
       |""".stripMargin
}

object OptionType {
  val underlinedTypes: Seq[Type] = BaseType.types ++ BaseType.types.map(OptionType.apply)
  val types: Seq[OptionType] = underlinedTypes.map(OptionType.apply)
  val declarations: Seq[String] = types.map(_.declaration)
  val noneDeclarations: Seq[String] = types.map(_.noneDeclaration)
  val functionsIsDefined: Seq[String] = types.map(_.funcIsDefined)
  val functionsIsEmpty: Seq[String] = types.map(_.funcIsEmpty)
  val functionsEquals: Seq[String] = types.map(_.funcEquals)
}