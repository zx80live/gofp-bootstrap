package com.zx80live.gofp.bootstrap

@deprecated("use com.zx80live.gofp.bootstrap.refactored")
object Transformers {
  def toName(t1: String, t2: String): String = s"${GoTypes.toName(t1)}${GoTypes.toName(t2)}Transformer"
  def toEmptyTransformerName(t1: String): String = s"EmptyTransformer${GoTypes.toName(t1)}"

  def transformerTypeDeclaration(t1: String, t2: String): String =
    s"""
       |type ${toName(t1, t2)} func(e $t1) $t2""".stripMargin

  val transformerTypeDeclarations: Seq[String] = for {
    t1 <- GoTypes.types
    t2 <- GoTypes.types
  } yield transformerTypeDeclaration(t1, t2)

  type TransformerName = String
  type In = String
  type Out = String

  val transformers: Seq[(TransformerName, In, Out)] = {
    val supportedTypes = Seq(
      GoTypes.GoBool,
      GoTypes.GoString,
      GoTypes.GoInt,
      GoTypes.GoInt64,
      GoTypes.GoUInt,
      GoTypes.GoUInt64,
      GoTypes.GoByte,
      GoTypes.GoRune,
      GoTypes.GoFloat32,
      GoTypes.GoFloat64,
      GoTypes.GoAny,
    )

    for {
      t1 <- GoTypes.baseTypes.filter(t => supportedTypes.contains(t))
      t2 <- GoTypes.baseTypes.filter(t => supportedTypes.contains(t))
    } yield (toName(t1, t2), t1, t2)
  }

  val transformerMaps: Seq[String] = for {
    (f1, in1, out1) <- transformers
    (f2, in2, out2) <- transformers
    if out1 == in2
  } yield {
    s"""
       |func (f1 $f1) Map${GoTypes.toName(out2)}(f2 $f2) ${toName(in1, out2)} { return func(e $in1) $out2 { return f2(f1(e)) } }""".stripMargin
  }

  val emptyTransformers: Seq[String] = GoTypes.types.map { t =>
    s"""
      |var ${toEmptyTransformerName(t)} ${toName(t, t)} = func(e $t) $t { return e }""".stripMargin
  }
}
