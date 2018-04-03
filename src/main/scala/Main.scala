import java.io.PrintWriter

import com.typesafe.scalalogging.Logger


object Main extends App {
  val logger = Logger("default")

  Config.parser.parse(args, Config("", "")) match {
    case None => logger.warn("Illegal parameters. Please use '--help' to see the usage.")
    case Some(config) =>
      val entries =
        try
          Fasta.fromFile(config.in)
        catch {
          case ex: Throwable =>
            logger.error("Loading data from file failed")
            throw ex
        }
      val chromosomes = entries.map(_.sequence)
      val result = ChromosomeUtils.reassembleStrict(chromosomes)
      new PrintWriter(config.out) {
        try
          write(result)
        catch {
          case ex: Throwable =>
            logger.error("Dumping result to file failed")
            throw ex
        } finally
          close()
      }
  }
}
