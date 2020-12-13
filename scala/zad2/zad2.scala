import io.Source
import java.io.PrintWriter
import java.io.File


object Main extends App {
    def asDouble(b: Boolean) : Double = if (b) 1.0 else 0.0
    
    def tf(t: String, d: Array[String]) : Int = {
        return d.filter(w => w == t).length
    }
    
    def idf(t: String, D: Array[Array[String]]) : Double = {
        val numOfDocs = D.map(ch => ch.toSet.contains(t)).map(v => asDouble(v)).fold(0.0)(_ + _)
        return math.log(D.length / 1 + numOfDocs)
    }
    
    def tf_idf(t: String, d: Array[String], D: Array[Array[String]]): Double = {
        return tf(t, d) * idf(t, D)
    }
    
    def tf_idfs(D: Array[Array[String]]) : Array[Array[(String, Double)]] = {
        val wordsTF = D.map(ch => ch.groupBy(x => x).mapValues(x => x.length).toSeq)
        val wordsIDF = D.flatten.toSet.toArray.map(w => (w, idf(w, D))).toMap
        val TF_IDFS = wordsTF.map(ch => ch.map(v => (v._1, v._2 * wordsIDF(v._1))).toArray.sortWith((x, y) => x._2 > y._2))
        return TF_IDFS
    }
    // val data = "pan_tadeusz.txt"
    val data = "witcher_blood_of_elves.txt"
    val chapterRegex = "CHAPTER [A-Z ]+\n"

    // val stopwords = "stopwords_pl.txt"
    val stopwords = "stopwords_en.txt"
    val stopwordsSet = Source.fromFile(stopwords, "UTF-8").mkString("").split("\\s+").toSet
    val punctation = "[.,\"'-?!—:;<>]"
    
    val chapters = Source.fromFile(data, "UTF-8").mkString("").split(chapterRegex)
    val wordsForChapters = chapters.map(ch => ch.toLowerCase().split("\\s+"))

    val withoutPunctation = wordsForChapters.map(ch => ch.map(w => w.replaceAll(punctation, "")))
    val withoutStopwords = withoutPunctation.map(ch => ch.filterNot(stopwordsSet.contains(_)).filter(v => v != ""))


    val tfIdfs = tf_idfs(withoutStopwords)

    val n = 75
    // val outfile_prefix = new File("result_pl_")
    val outfile_prefix = new File("result_en_")
    val outfile_postfix = new File(".txt")

    tfIdfs.zipWithIndex.map(v => {
        val printer = new PrintWriter(outfile_prefix + v._2.toString + outfile_postfix)
        v._1.take(n).foreach(e => printer.write(e._2 + "," + e._1 + "\n"))
        printer.close()
    })
}