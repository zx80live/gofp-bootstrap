package com.zx80live.gofp.bootstrap.types

case class LazyType(t: Type) extends Type {
  override def raw: String = s"Lazy${t.view}"

  override def alias: String = raw

  override def view: String = raw

  override def declaration: String =
    s"""
       |type $raw struct {
       |	eval   func() ${t.raw}
       |	cached *${t.raw}
       |}""".stripMargin

  override def funcEquals: String = ""

  override def funcToString: String =
    s"""
       |func (n $raw) ToString() string {
       |  if n.cached != nil {
       |    return fmt.Sprintf("${raw}(%v)", *n.cached)
       |  } else {
       |    return fmt.Sprintf("${raw}(?)")
       |  }
       |}""".stripMargin

  def funcValue: String =
    s"""
       |func (n $raw) Value() ${t.raw} {
       |	if n.cached != nil {
       |		//fmt.Println(" ${t.alias}.cached", *n.cached)
       |		return *n.cached
       |	} else {
       |		//fmt.Println("*${t.alias}.eval", n.eval())
       |		return n.eval()
       |	}
       |}""".stripMargin

  def funcEval: String =
    s"""
       |func (n $raw) Eval() $raw {
       |	if n.cached != nil {
       |		//fmt.Println(" ${t.alias}.cached", *n.cached)
       |		return n
       |	} else {
       |		cached := n.eval()
       |		//fmt.Println("*${t.alias}.eval", cached)
       |		return $raw{n.eval, &cached}
       |	}
       |}""".stripMargin

  def funcCached: String =
    s"""
       |func (n $raw) Cached() ${t.raw} { return *n.cached }
       |""".stripMargin

  def consName: String = s"Mk$view"

  def funcCons: String =
    s"""
       |func $consName(f func() ${t.raw}) $raw { return $raw { f , nil }}
       |""".stripMargin
}

object LazyType {
  def allowedBaseTypes: Seq[Type] = BaseType.reducedTypes ++ Seq(Tuple2Type.defaultType)

  def types: Seq[LazyType] =
    allowedBaseTypes.map(LazyType.apply) ++ // List[T]
      allowedBaseTypes.map(ArrayType.apply).map(LazyType.apply) ++ // List[Array[T]]
      allowedBaseTypes.map(OptionType.apply).map(LazyType.apply) ++ // List[Option[T]]
      allowedBaseTypes.map(ListType.apply).map(LazyType.apply) // List[List[T]]

  def declarations: Seq[String] = types.map(_.declaration)

  def functionsValue: Seq[String] = types.map(_.funcValue)

  def functionsEval: Seq[String] = types.map(_.funcEval)

  def functionsToString: Seq[String] = types.map(_.funcToString)

  def functionsCons: Seq[String] = types.map(_.funcCons)

  def functionsCached: Seq[String] = types.map(_.funcCached)
}
