package com.zx80live.gofp.bootstrap.types

import com.zx80live.gofp.bootstrap.functions.{FuncEquals, FuncToString}

case class OptionType(override val underlined: Type) extends MonadType {

  override def raw: String = s"${underlined.view}Option"

  override def emptyName: String = s"None${underlined.view}"

  override def alias: String = view

  override def view: String = raw

  override def declaration: String =
    s"""
       |type $raw struct { value *${underlined.raw} }""".stripMargin

  override def consView: String = underlined match {
    case t: BaseType if t == BaseType.GoAny => s"AnyOpt"
    case _: BaseType => s"${underlined.view}"
    case u: OptionType => s"${u.consView}${u.core.view}"
    case _ => s"${underlined.view}Opt"
  }

  override def emptyDeclaration: String =
    s"""
       |var $emptyName $raw = $raw { nil }""".stripMargin

  override def funcCons: String =
    s"""
       |func $consView(e ${underlined.raw}) $raw { return $raw { &e } }""".stripMargin

  def funcIsDefined: String =
    s"""
       |func (o $raw) IsDefined() bool { return o != $emptyName }
       |""".stripMargin

  def funcIsEmpty: String =
    s"""
       |func (o $raw) IsEmpty() bool { return o == $emptyName }
       |""".stripMargin

  override def funcForeach: String =
    s"""
       |func (o $raw) Foreach(f func(${underlined.raw})) { if o.IsDefined() { f(*o.value) } }""".stripMargin

  override def funcHead: String =
    s"""
       |func (o $raw) Head() ${underlined.raw} { if o.IsDefined() { return *o.head } else { panic("can't get head from None") } }""".stripMargin

  override def funcHeadOption: String =
    s"""
       |func (o $raw) HeadOption() $raw { return o }""".stripMargin

  override def funcTail: String =
    s"""
       |func (m $raw) ${ListType(underlined)} { return m.ToList().Tail() }""".stripMargin

  override def funcSize: String = ""

  override def funcFilter: String =
    s"""
       |func (m $raw) Filter(p ${Predicate.name(underlined)}) $raw { if m.IsDefined() && p(*m.value) { return m } else { return $emptyName } }""".stripMargin

  override def funcMap(out: Type): String = {
    val o = OptionType(out)
    s"""
       |func (m $raw) Map${out.view}(f ${Transformer.name(underlined, out)}) ${o.raw} {
       |  if m.IsDefined() { return ${o.consView}(f(*m.value)) } else { return ${o.emptyName} } }""".stripMargin
  }

  override def funcDrop: String =
    s"""
       |func (m $raw) Drop(i int) ${ListType(underlined)} { return m.ToList().Drop(i) }""".stripMargin

  override def funcToList: String = {
    val l = ListType(underlined)
    s"""
       |func (m $raw) ToList() ${l.raw} { if m.IsDefined() { return ${l.consView}(*m.head) } else { return ${l.emptyName} } }""".stripMargin
  }

  override def funcEquals: String =
    s"""
       |func (a $raw) Equals(b $raw) bool {
       |  if a.IsDefined() {
       |    if b.IsDefined() {
       |      return ${FuncEquals.name(underlined)}(*a.value, *b.value)
       |    } else { return false}
       |  } else if b.IsDefined() { return false } else { return true } }""".stripMargin


  override def funcToString: String =
    s"""
       |func (o $raw) ToString() string { if o.IsDefined() { return fmt.Sprintf("Some(%v)", ${FuncToString.name(underlined)}) } else { return "None" } }""".stripMargin

  override def setUnderlined(t: Type): MonadType = OptionType(t)
}

object OptionType {
  def underlinedTypes: Seq[Type] = BaseType.types ++ ArrayType.types ++ BaseType.types.map(ListType.apply)

  def types: Seq[OptionType] = (underlinedTypes ++ underlinedTypes.map(OptionType.apply) ++ underlinedTypes.map(OptionType.apply).map(OptionType.apply)).map(OptionType.apply)

  def declarations: Seq[String] = types.map(_.declaration)

  def emptyDeclarations: Seq[String] = types.map(_.emptyDeclaration)

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

  //  def functionsFlatMap: Seq[String] = {
  //    val baseTypes = Seq(
  //      BaseType.GoBool,
  //      BaseType.GoAny,
  //      BaseType.GoByte,
  //      BaseType.GoInt,
  //      BaseType.GoInt32,
  //      BaseType.GoInt64,
  //      BaseType.GoUInt,
  //      BaseType.GoUInt64,
  //      BaseType.GoUIntPtr,
  //      BaseType.GoFloat32,
  //      BaseType.GoFloat64,
  //      BaseType.GoRune,
  //      BaseType.GoString
  //    )
  //
  //    val inTypes = (baseTypes ++ baseTypes.map(SliceType.apply) ++ baseTypes.map(ListType.apply) ++ baseTypes.map(OptionType.apply)).map(OptionType.apply)
  //    val outTypes = inTypes
  //
  //    _functionsFlatMap(
  //      inTypes = inTypes,
  //      outTypes = outTypes)
  //  }
  //
  //  private def _functionsFlatMap(inTypes: Seq[MonadType], outTypes: Seq[MonadType]) = {
  //    for {
  //      o1 <- inTypes
  //      o2 <- outTypes
  //    } yield o1.funcFlatMap(o2)
  //  }

}