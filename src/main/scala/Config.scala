import scopt.OptionParser

case class Config(in: String, out: String)

object Config {

  val parser: OptionParser[Config] = new scopt.OptionParser[Config]("java -jar <path-to-your-jar>") {
    head("Chromosome tools", "0.1")

    opt[String]('i', "in").required()
      .valueName("<value>")
      .action( (x, c) => c.copy(in = x))
      .text("in is the absolute path of the input FASTA file.")

    opt[String]('o', "out").required()
      .valueName("<value>")
      .action( (x, c) => c.copy(out = x))
      .text("out is the absolute path of the output file including the result.")

    help("help").text("prints this usage text")
  }

}
