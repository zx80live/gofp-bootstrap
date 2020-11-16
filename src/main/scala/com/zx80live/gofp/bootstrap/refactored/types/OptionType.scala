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

  override def rawFrom(t: Type): String = s"${t.view}Option"

  override def nilNameFrom(t: Type): String = s"None${t.view}"

  override def view: String = raw

  override def declaration: String =
    s"""
       |type $raw struct { value *${underlined.raw} }""".stripMargin

  override def consView: String = underlined match {
    case t: BaseType if t ==BaseType.GoAny => s"AnyOpt"
    case _: BaseType => s"${underlined.view}"
    case _: OptionType => s"${underlined.consView}${underlined.core.view}"
    case _ => s"${underlined.view}Opt"
  }

  def noneName: String = s"None$view"

  def noneDeclaration: String =
    s"""
       |var $noneName $raw = $raw { nil }""".stripMargin

  override def funcCons: String =
    s"""
       |func $consView(e ${underlined.raw}) $raw { return $raw { &e } }""".stripMargin

  def funcIsDefined: String =
    s"""
       |func (o $raw) IsDefined() bool { return o == $noneName }
       |""".stripMargin

  def funcIsEmpty: String =
    s"""
       |func (o $raw) IsEmpty() bool { return !o.IsDefined() }
       |""".stripMargin

  override def funcForeach: String =
    s"""
       |func (o $raw) Foreach(f func(${underlined.raw})) { if o.IsDefined() { f(*o.value) } }""".stripMargin
}

object OptionType {
  // TODO reduce option types
  def underlinedTypes: Seq[Type] = BaseType.types ++ ArrayType.types ++ BaseType.types.map(ListType.apply)
  def types: Seq[OptionType] = (underlinedTypes ++ underlinedTypes.map(OptionType.apply)).map(OptionType.apply)

  def declarations: Seq[String] = types.map(_.declaration)
  def noneDeclarations: Seq[String] = types.map(_.noneDeclaration)

  def functionsCons: Seq[String] = types.map(_.funcCons)
  def functionsIsDefined: Seq[String] = types.map(_.funcIsDefined)
  def functionsIsEmpty: Seq[String] = types.map(_.funcIsEmpty)
  def functionsEquals: Seq[String] = types.map(_.funcEquals)
  def functionsForeach: Seq[String] = types.map(_.funcForeach)
  def functionsFilter: Seq[String] = types.map(_.funcFilter)
  def functionsMap: Seq[String] = for {
    o <- types
    t <- Transformer.types
    if o.underlined == t.in
  } yield o.funcMap(t.out)

  def functionsToString: Seq[String] = types.map(_.funcToString)
}