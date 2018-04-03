import org.scalatest.{FlatSpec, Matchers}


class ChromosomeUtilsTest extends FlatSpec with Matchers {

  "Legal chromosomes that all has the same length" should "be re-assembled correctly" in {
    val chromosomes = Seq("ATTAGACCTG", "CCTGCCGGAA", "AGACCTGCCG", "GCCGGAATAC")
    val result = ChromosomeUtils.reassembleStrict(chromosomes)
    result should equal ("ATTAGACCTGCCGGAATAC")
  }

  "Legal chromosomes that one of them has a short length" should "be re-assembled correctly and the short one is at the end" in {
    val chromosomes = Seq("ATTAGACCTG", "CCTGCCGGAA", "AGACCTGCCG", "GCCGGAATC")
    val result = ChromosomeUtils.reassembleStrict(chromosomes)
    result should equal ("ATTAGACCTGCCGGAATC")
  }

  "No chromosome " should "return empty string" in {
    val chromosomes = Seq()
    val result = ChromosomeUtils.reassembleStrict(chromosomes)
    result should equal ("")
  }

  "Illegal chromosomes that one of them has a longer length" should "cause IllegalChromosome" in {
    val chromosomes = Seq("ATTAGACCTG", "CCTGCCGGAA", "AGACCTGCCG", "GCCGGAATCCCCC")
    an [IllegalChromosome] should be thrownBy ChromosomeUtils.reassembleStrict(chromosomes)
  }

  "Illegal chromosomes that one of them has different length" should "cause IllegalChromosome" in {
    val chromosomes = Seq("ATTAGACCTG", "CCTGCCGGA", "AGACCTGCCG", "GCCGGATC")
    an [IllegalChromosome] should be thrownBy ChromosomeUtils.reassembleStrict(chromosomes)
  }

  "Illegal chromosomes that contains characters other than A/T/C/G" should "cause IllegalChromosome" in {
    val chromosomes = Seq("ATUAGACCTG", "CCTGCCGGAA", "AGACCTGCCG", "GCCGGAATAC")
    an [IllegalChromosome] should be thrownBy ChromosomeUtils.reassembleStrict(chromosomes)
  }

  "Legal chromosomes that contains lower case of A/T/C/G" should "not cause any exception and return in upper case" in {
    val chromosomes = Seq("ATTAGACCTG".toLowerCase, "CCTGCCGGAA", "AGACCTGCCG", "GCCGGAATAC")
    val result = ChromosomeUtils.reassembleStrict(chromosomes)
    result should equal ("ATTAGACCTGCCGGAATAC")
  }

  "Legal chromosomes with more than one re-assembly way" should "cause a TooManyReassembleWays" in {
    val chromosomes = Seq("AAAACCCC", "CCCCTTTT", "TTTTGGGG", "GGGGAAAA")
    an [TooManyReassembleWays] should be thrownBy ChromosomeUtils.reassembleStrict(chromosomes)
  }

  "Legal chromosomes without any re-assembly way" should "cause a NoReassembleWay" in {
    val chromosomes = Seq("AAAACCCC", "CCCCTTTT", "TTTTGGAG", "GGGGAATA")
    an [NoReassembleWay] should be thrownBy ChromosomeUtils.reassembleStrict(chromosomes)
  }

  "Legal chromosomes with repeated items" should "not affect the correct result" in {
    val chromosomes = Seq("AAAACCC", "ACCCTTT", "ACCCTTT", "CTTTGAG", "CTTTGAG")
    val result = ChromosomeUtils.reassembleStrict(chromosomes)
    result should equal ("AAAACCCTTTGAG")
  }

  // TODO Performance Test
//  "Legal chromosomes with the max size of 50 and max length of 1000" should "re-assemble the correct result" in {
//    val characters = Seq('A', 'C', 'T','G')
//    val halfMaxLength = 4
//    val itemList = (0 until halfMaxLength)
//      .map(_ => characters(Random.nextInt(4)))
//      .toList
//      .permutations
//      .toList
//      .map(_.mkString(""))
//
//    val chromosomes = (0 until itemList.size - 1).map(i => itemList(i) + itemList(i + 1))
//    println(chromosomes)
//    val result = ChromosomeUtils.reassembleStrict(chromosomes)
//    result should equal (itemList.mkString(""))
//  }

}
