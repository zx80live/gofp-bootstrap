package com.zx80live.gofp.bootstrap

import java.io._

object Bootstrap extends App {
  val root = "/home/work/gofp/fp/"

  def fileHeader(name: String, pack: String = "fp", imports: Seq[String] = Nil) =
    println(
      s"""// $name.go
         |// DO NOT EDIT THIS FILE WAS GENERATED AUTOMATICALLY BY gofp-bootstrap
         |
         |package $pack
         |
         |${imports.map(i => "import \"" + i + "\"").mkString("", "\n", "")}
         |""".stripMargin)


  def toFile(filename: String,
             pack: String = "fp",
             imports: Seq[String] = Nil,
             content: Seq[String] = Nil): Unit = {
    val writer = new BufferedWriter(new FileWriter(new File(root + "/" + filename)))

    try {
      writer.write(
        s"""// $filename
           |// DO NOT EDIT THIS FILE WAS GENERATED AUTOMATICALLY BY gofp-bootstrap
           |
           |package $pack
           |
           |${imports.map(i => "import \"" + i + "\"").mkString("", "\n", "")}
           |""".stripMargin
      )
      content foreach writer.write
    } finally {
      writer.close()
    }
  }


  toFile("fpredicate.go", content = Predicates.predicateTypeDeclarations)
  toFile("fequal.go", content = Equals.allEquals)
  toFile("fpredicate_eq.go", content = Predicates.predicateEq)
  toFile("fpredicate_logic.go", content = Predicates.predicateAnd ++  Predicates.predicateOr ++  Predicates.predicateXor ++  Predicates.predicateNeg)
  toFile("fmkstring.go", imports = Seq("fmt"), content = MkStrings.arraysMkString)
  toFile("ftostring.go", imports = Seq("fmt"), content = ToStrings.allToStrings)
  toFile("ffunctor.go", content =
    Functors.functorTypeDeclarations ++ Functors.emptyFunctors
  )
  toFile("ffunctor_map.go", content = Functors.functorMaps)
  toFile("flist.go", content = Lists.lists ++ Lists.listsNil)
  toFile("flist_isempty.go", content = Lists.listsEmptyNonEmpty)
  toFile("flist_make.go", content = Lists.listsMake)
  toFile("flist_head.go", content = Lists.listsHead)
  toFile("flist_tail.go", content = Lists.listsTail)
  toFile("flist_copy.go", content = Lists.listsCopy)
  toFile("flist_cons.go", content = Lists.listsCons)
  toFile("flist_foreach.go", content = Lists.listsForeach)
  toFile("flist_reverse.go", content = Lists.listsReverse)
  toFile("flist_filter.go", content = Lists.listsFilter)
  toFile("flist_map.go", content = Lists.listsMap)

  toFile("ftuple.go", content = Tuples.tuples)
  toFile("foptional.go", content = Optional.optionalDeclarations ++ Optional.optionalNones ++ Optional.optionalCons)
  toFile("foptional_tostring.go", imports = Seq("fmt"), content = Optional.optionalToString)
}







