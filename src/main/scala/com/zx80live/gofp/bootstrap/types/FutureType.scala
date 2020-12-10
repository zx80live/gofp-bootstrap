package com.zx80live.gofp.bootstrap.types

case class FutureType(override val underlined: Type) extends MonadType {
  override def raw: String = s"""${underlined.view}Future"""

  override def alias: String = raw

  override def view: String = alias

  override def declaration: String =
    s"""
       |type $raw struct {
       |  ch chan ${underlined.alias}
       |}""".stripMargin

  override def setUnderlined(t: Type): MonadType = ???

  override def emptyName: String = ???

  override def emptyDeclaration: String = ???

  override def consView: String = s"""Mk$view"""

  override def funcCons: String =
    s"""
       |func $consView(f func() ${underlined.alias}) $raw {
       | 	fut := $raw { make(chan ${underlined.alias}, 1) }
       |	go func() { defer close(fut.ch); fut.ch <- f() }()
       |	return fut
       |}""".stripMargin

  override def funcFilter: String = ???

  override def funcMap(out: Type): String =
    s"""
       |func (f $alias) Map${out.view}(t func(${underlined.alias}) ${out.alias}) ${FutureType(out).raw} { return ${FutureType(out).successName}(t(<-f.ch)) }""".stripMargin

  override def funcToList: String = ???

  override def funcReduce: String = ???

  override def funcFlatMap(out: MonadType): String =
    s"""
       |func (f $alias) FlatMap${out.underlined.view}(t func(${underlined.alias}) ${out.raw}) ${out.raw} {
       |	return ${out.consView}(func() ${out.underlined.alias} {
       |		return <- t( <- f.ch).ch
       |	})
       |}""".stripMargin

  override def funcFoldLeft(out: Type): String = ???

  override def funcFind: String = ???


  override def funcEquals: String = ???

  override def funcToString: String = ???

  def successName: String = s"Success$view"

  def funcSuccessFuture: String =
    s"""
       |func $successName(v ${underlined.alias}) $raw {
       |	ch := make(chan ${underlined.alias}, 1)
       |	go func() { defer close(ch); ch <- v }()
       |	return $raw { ch }
       |}""".stripMargin

  def funcResult: String =
    s"""
       |func (f $alias) Result() ${underlined.alias} { return <-f.ch }""".stripMargin

  override def funcZipWithIndex: String = ???

  override def funcZipAll(m: MonadType): String = ???

  override def funcZip(m: MonadType): String = ???
}

object FutureType {

  def types: Seq[FutureType] = (
    BaseType.reducedTypes ++ // Future[T]
      BaseType.reducedTypes.map(OptionType.apply) ++ // Future[Option[T]]
      BaseType.reducedTypes.map(ListType.apply).map(OptionType.apply) ++ // Future[Option[List[T]]]
      BaseType.reducedTypes.map(ArrayType.apply).map(OptionType.apply) ++ // Future[Option[Array[T]]]
      BaseType.reducedTypes.map(ListType.apply) ++ // Future[List[T]]
      BaseType.reducedTypes.map(ArrayType.apply) // Future[Array[T]]
    ).map(FutureType.apply)

  def declarations: Seq[String] = types.map(_.declaration)

  def functionsCons: Seq[String] = types.map(_.funcCons)

  def functionsSuccess: Seq[String] = types.map(_.funcSuccessFuture)

  def functionsMap: Seq[String] = {
    val inTypes = types
    val outTypes = types.map(_.underlined)
    for {
      in <- inTypes
      out <- outTypes
    } yield in.funcMap(out)
  }

  def functionsFlatMap: Seq[String] = {
    val inTypes = types
    val outTypes = types
    for {
      in <- inTypes
      out <- outTypes
    } yield in.funcFlatMap(out)
  }

  def functionsResult: Seq[String] = types.map(_.funcResult)
}