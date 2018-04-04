import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable
import scala.util.Random


class StrictChromosomeSpec() extends FlatSpec with Matchers {

  "Legal fragments that all has the same length" should "be re-assembled correctly" in {
    val fragments = Seq("ATTAGACCTG", "CCTGCCGGAA", "AGACCTGCCG", "GCCGGAATAC")
    val result = new StrictChromosome(fragments).reassemble()
    result should equal ("ATTAGACCTGCCGGAATAC")
  }

  "Legal fragments that one of them has a short length" should "be re-assembled correctly and the short one is at the end" in {
    val fragments = Seq("ATTAGACCTG", "CCTGCCGGAA", "AGACCTGCCG", "GCCGGAATC")
    val result = new StrictChromosome(fragments).reassemble()
    result should equal ("ATTAGACCTGCCGGAATC")
  }

  "No chromosome " should "return empty string" in {
    val fragments = Seq()
    val result = new StrictChromosome(fragments).reassemble()
    result should equal ("")
  }

  "Illegal fragments that one of them has a longer length" should "cause IllegalChromosome" in {
    val fragments = Seq("ATTAGACCTG", "CCTGCCGGAA", "AGACCTGCCG", "GCCGGAATCCCCC")
    an [IllegalChromosome] should be thrownBy new StrictChromosome(fragments).reassemble()
  }

  "Illegal fragments that one of them has different length" should "cause IllegalChromosome" in {
    val fragments = Seq("ATTAGACCTG", "CCTGCCGGA", "AGACCTGCCG", "GCCGGATC")
    an [IllegalChromosome] should be thrownBy new StrictChromosome(fragments).reassemble()
  }

  "Illegal fragments that contains characters other than A/T/C/G" should "cause IllegalChromosome" in {
    val fragments = Seq("ATUAGACCTG", "CCTGCCGGAA", "AGACCTGCCG", "GCCGGAATAC")
    an [IllegalChromosome] should be thrownBy new StrictChromosome(fragments).reassemble()
  }

  "Legal fragments that contains lower case of A/T/C/G" should "not cause any exception and return in upper case" in {
    val fragments = Seq("ATTAGACCTG".toLowerCase, "CCTGCCGGAA", "AGACCTGCCG", "GCCGGAATAC")
    val result = new StrictChromosome(fragments).reassemble()
    result should equal ("ATTAGACCTGCCGGAATAC")
  }

  "Legal fragments without any re-assembly way" should "cause a NoReassembleWay" in {
    val fragments = Seq("AAAACCCC", "CCCCTTTT", "TTTTGGAG", "GGGGAATA")
    an [NoReassembleWay] should be thrownBy new StrictChromosome(fragments).reassemble()
  }

  "Legal fragments with repeated items" should "not affect the correct result" in {
    val fragments = Seq("AAAACCC", "ACCCTTT", "ACCCTTT", "CTTTGAG", "CTTTGAG")
    val result = new StrictChromosome(fragments).reassemble()
    result should equal ("AAAACCCTTTGAG")
  }

  "Legal fragments with the max size of 50 and max length of 1000" should "re-assemble the correct result" in {
    val characters = Seq('A', 'C', 'T','G')
    val halfMaxLength = 500
    val maxSize = 50

    def randomLegalString(length: Int): String = (0 until length).map(_ => characters(Random.nextInt(4))).mkString("")

    val itemList = mutable.Buffer[String]()
    while(itemList.size <= maxSize) {
      itemList ++= (0 to maxSize).map(_ => randomLegalString(halfMaxLength))
    }

    val fragments = (0 until maxSize).map(i => itemList(i) + itemList(i + 1))
    val result = new StrictChromosome(fragments).reassemble()
    result should equal (itemList.mkString(""))
  }

}
