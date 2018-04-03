import scala.collection.mutable

object ChromosomeUtils {

  val CHROMOSOME_CHARACTERS = Set('A', 'C', 'G', 'T')

  private def getMaxMatchLength(s: String, t: String, minMatchLength: Int): Int = {
    if (minMatchLength > s.length || minMatchLength > t.length)
      return 0
    val minSubStr = t.slice(0, minMatchLength)
    val matched = minSubStr.r.findAllMatchIn(s).map(_.start).filter(i => {
      val matchLength = s.length - i
      if (matchLength < minMatchLength)
        false
      else if (s.slice(i, s.length) == t.slice(0, matchLength))
        true
      else
        false
    }).map(s.length - _)
    if (matched.isEmpty) 0 else matched.max
  }

  private def getMatchStrings(target: String, minOverlapLength: Int, availableChromosomes: Set[String]): Set[(String, Int)] = {
    if (availableChromosomes.isEmpty || target.isEmpty)
      return Set.empty
    val result = mutable.Buffer[(String, Int)]()
    availableChromosomes.foreach(c => {
      if (target.contains(c))
        result.append((c, c.length))
      else {
        val overlapLength = getMaxMatchLength(c, target.slice(0, c.length), minOverlapLength)
        if (overlapLength >= minOverlapLength) {
          result.append((c, overlapLength))
        }
      }
    })
    result.toSet
  }

  // Not check the head chromosome's overlap length percentage
  private def mkChromosomeStrings(
                                   iResult: String, iMinOverlapLength: Int, availableChromosomes: Set[String]
                                 ): List[(String, Int, Set[String])] = {
    if (availableChromosomes.isEmpty)
      return List((iResult, iMinOverlapLength, availableChromosomes))
    val minOverlapLength = if (iMinOverlapLength < 1) 1 else iMinOverlapLength
    val validPreChromosomes = getMatchStrings(iResult, minOverlapLength, availableChromosomes)
    val result = mutable.Buffer[(String, Int, Set[String])]()
    validPreChromosomes.map(c => {
      if (c._2 == c._1.length)
        result ++= mkChromosomeStrings(iResult, minOverlapLength, availableChromosomes - c._1)
      else
        result ++= mkChromosomeStrings(
          c._1.slice(0, c._1.length - c._2) + iResult,
          ((c._1.length / 2.0).round - c._2).toInt,
          availableChromosomes - c._1
        )
    })
    result.toSet.toList.sortBy[Int](_._2)(Ordering[Int].reverse)
  }

  def isLegalChromosomes(chromosomes: Seq[String]): Boolean =
    chromosomes != null && chromosomes.toSet.forall(_.toUpperCase.toSet.subsetOf(CHROMOSOME_CHARACTERS))

  def reassembleStrict(iChromosomes: Seq[String]): String = {
    if (!isLegalChromosomes(iChromosomes))
      throw IllegalChromosome()
    if (iChromosomes.isEmpty)
      return ""
    val chromosomes = iChromosomes.map(_.toUpperCase).toSet
    val commonLength = chromosomes.map(_.length).max
    val tails = chromosomes.filter(_.length != commonLength)
    val tail = tails.size match {
      case 0 => None
      case 1 => Option(tails.head)
      case _ => throw IllegalChromosome("Strict chromosome set should have at most one chromosome which is shorter than the others.")
    }

    val result = tail match {
      case Some(x) => mkChromosomeStrings(x, (x.length / 2.0).round.toInt, chromosomes - x)
      case None => chromosomes.map(c =>{
        mkChromosomeStrings(c, (commonLength / 2.0).round.toInt, chromosomes - c)
      }).fold(List()){(l, r) => l ++ r}
    }

    // check head here
    val headCheckedResult = result.filter(_._2 <= 0)

    headCheckedResult.size match {
      case 0 => throw NoReassembleWay()
      case 1 => headCheckedResult.head._1
      case _ =>
        println(result.map(_._1))
        throw TooManyReassembleWays(s"More than one unique way found. result=${result.map(_._1)}")
    }
  }
}
