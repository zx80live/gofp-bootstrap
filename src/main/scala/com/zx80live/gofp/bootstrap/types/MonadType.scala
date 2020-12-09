package com.zx80live.gofp.bootstrap.types

import scala.annotation.tailrec

trait MonadType extends Type {

  def setUnderlined(t: Type): MonadType

  def underlined: Type

  def emptyName: String

  def emptyDeclaration: String

  def consView: String

  def nestedLevel: Int = {
    @tailrec
    def loop(t: Type, level: Int): Int = t match {
      case ct: MonadType => loop(ct.underlined, level + 1)
      case _ => level
    }

    loop(this, 0)
  }

  def core: Type = underlined match {
    case ct: MonadType => ct.core
    case _ => this
  }

  def funcCons: String

  def funcFilter: String

  def funcMap(out: Type): String

  def funcToList: String

  def funcReduce: String

  // Monad[A].flatMap(A => Monad[B]): Monad[B]
  def funcFlatMap(out: MonadType): String

  def funcFlatten: String = underlined match {
    case u: MonadType =>
      s"""
         |func (m $raw) Flatten() ${underlined.raw} {
         |  return m.FlatMap${u.underlined.view}(func(e ${u.raw}) ${u.raw} { return e }) }""".stripMargin
    case _ => ""
  }

  def funcFoldLeft(out: Type): String

  def funcFind: String

  def funcZipWithIndex: String
  def funcZipWith: String
  def funcZip: String
}
