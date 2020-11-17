package com.zx80live.gofp.bootstrap.types

trait Type {
  def underlined: Type

  def raw: String

  def alias: String = raw

  def view: String = alias

  def declaration: String = ""

  def consView: String = ""

  def core: Type = underlined match {
    case SuperType => this
    case _ => underlined.core
  }

  override def toString: String =
    s"""
       |raw:        $raw
       |underlined: ${underlined.raw}
       |core:       ${core.raw}
       |alias:      $alias
       |view:       $view        // func ${view}ToString() string
       |consView:   $consView    ${if (consView.nonEmpty) " \t\t\t // func " + consView + "(...) " + raw else ""}
       |declare:    $declaration
       |""".stripMargin
}

