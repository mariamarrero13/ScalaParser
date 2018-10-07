import scala.io._

object console {
      def main(args: Array[String]): Unit = {
      var strToParse = scala.io.Source.fromFile("testprogram.txt").mkString
      val parser = new CICOMparser
      parser.apply(strToParse)
        }
  
}
