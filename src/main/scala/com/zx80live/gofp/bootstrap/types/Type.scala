package com.zx80live.gofp.bootstrap.types

import com.zx80live.gofp.bootstrap.functions.{FuncEquals, FuncToString}

trait Type {

  // int, []int, IntOption, IntList
  def raw: String

  // type <IntArray> = []int
  def alias: String

  // Int, IntArray, IntOption, IntList
  def view: String

  // type IntOption struct {}
  def declaration: String

  def funcEquals: String

  def funcToString: String

  override def toString: String =
    s"""
       |raw:          $raw
       |alias:        $alias
       |view:         $view
       |declaration:  $declaration
       |funcEquals:   $funcEquals
       |funcToString: $funcToString
       |""".stripMargin
}

