package com.zx80live.gofp.bootstrap

object Functors {
  def toName(t1: String, t2: String): String = s"Functor${GoTypes.toName(t1)}${GoTypes.toName(t2)}"

  def functorTypeDeclaration(t1: String, t2: String): String = s"type ${toName(t1, t2)} func(e $t1) $t2"

  val functorTypeDeclarations: Seq[String] = for {
    t1 <- GoTypes.allTypes
    t2 <- GoTypes.allTypes
  } yield functorTypeDeclaration(t1, t2)
}
