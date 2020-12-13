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

    def f(w: String, D: Array[Array[String]]) : List[Int] = {
        return D.zipWithIndex.map(v => (v._2, tf_idf(w, v._1, D))).sortWith((x, y) => x._2 > y._2).map(v => v._1).toList
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


    val w = "gerald"
    println(f(w, withoutStopwords))
}