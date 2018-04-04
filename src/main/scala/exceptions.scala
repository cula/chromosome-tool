class ChromosomeException(message: String = "") extends Exception(message)

case class IllegalChromosome(message: String = "Chromosome input is illegal") extends ChromosomeException(message)

case class NoReassembleWay(message: String = "No way found to reassemble the given chromosomes")
  extends ChromosomeException(message)
