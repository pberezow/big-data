import io.Source
import java.io.PrintWriter
import java.io.File

object Main extends App {
    val data = "pan_tadeusz.txt"
    // val data = "witcher_the_last_wish.txt"
    val words = Source.fromFile(data, "UTF-8").mkString("").toLowerCase().split("\\s+")

    val stopwords = "stopwords_pl.txt"
    // val stopwords = "stopwords_en.txt"
    val stopwordsSet = Source.fromFile(stopwords, "UTF-8").mkString("").split("\\s+").toSet
    val punctation = "[.,\"'-?!—:;<>]"

    val withoutPunctation = words.map(x => x.replaceAll(punctation, ""))
    val withoutStopwords = withoutPunctation.filterNot(stopwordsSet.contains(_)).filter(v => v != "")
    val groupped = withoutStopwords.groupBy(x => x)
    val reduced = groupped.mapValues(x => x.length)
    val sorted = reduced.toSeq.sortWith((x, y) => x._2 > y._2)

    val n = 75
    val outfile = new File("result_pl.txt")
    // val outfile = new File("result_en.txt")
    val printer = new PrintWriter(outfile)
    sorted.take(n).foreach(v => printer.write(v._2 + "," + v._1 + "\n"))
    printer.close()
}