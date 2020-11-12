package com.zx80live.gofp.bootstrap

object Functors {
  def toName(t1: String, t2: String): String = s"Functor${GoTypes.toName(t1)}${GoTypes.toName(t2)}"
  def toEmptyFunctorName(t1: String): String = s"EmptyFunctor${GoTypes.toName(t1)}"

  def functorTypeDeclaration(t1: String, t2: String): String =
    s"""
       |type ${toName(t1, t2)} func(e $t1) $t2""".stripMargin

  val functorTypeDeclarations: Seq[String] = for {
    t1 <- GoTypes.allTypes
    t2 <- GoTypes.allTypes
  } yield functorTypeDeclaration(t1, t2)

  type FunctorName = String
  type In = String
  type Out = String

  val functors: Seq[(FunctorName, In, Out)] = {
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

  val functorMaps: Seq[String] = for {
    (f1, in1, out1) <- functors
    (f2, in2, out2) <- functors
    if out1 == in2
  } yield {
    s"""
       |func (f1 $f1) Map${GoTypes.toName(out2)}(f2 $f2) ${toName(in1, out2)} { return func(e $in1) $out2 { return f2(f1(e)) } }""".stripMargin
  }

  val emptyFunctors: Seq[String] = GoTypes.allTypes.map { t =>
    s"""
      |var ${toEmptyFunctorName(t)} ${toName(t, t)} = func(e $t) $t { return e }""".stripMargin
  }
}
