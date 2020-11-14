package com.zx80live.gofp.bootstrap

import java.io._

object Bootstrap extends App {
  val root = "/home/work/gofp/fp/"

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
  toFile("fmkstring.go", imports = Seq("fmt"), content = MkStrings.arraysMkString ++ MkStrings.nestedArraysMkString)
  toFile("ftostring.go", imports = Seq("fmt"), content = ToStrings.allToStrings)
  toFile("ftransformer.go", content =
    Transformers.transformerTypeDeclarations ++ Transformers.emptyTransformers
  )
  toFile("ftransformer_map.go", content = Transformers.transformerMaps)
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
  toFile("flist_size.go", content = Lists.listsSize)
  toFile("flist_equals.go", content = Lists.listsEquals)
  toFile("flist_toarray.go", content = Lists.toArrays)

  toFile("ftuple.go", content = Tuples.tuples)
  toFile("foptional.go", content = Optional.optionalDeclarations ++ Optional.optionalNones ++ Optional.optionalCons)
  toFile("foptional_tostring.go", content = Optional.optionalToString)
  toFile("foptional_isdefined.go", content = Optional.optionalIsDefined)
  toFile("foptional_isempty.go", content = Optional.optionalIsEmpty)
  toFile("foptional_filter.go", content = Optional.optionalFilter)
  toFile("foptional_map.go", content = Optional.optionalMap)
  toFile("foptional_equals.go", content = Optional.optionalEquals)
}







