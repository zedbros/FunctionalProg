import org.jsoup.*

import scala.concurrent.*
import duration.{Duration, MILLISECONDS}
import ExecutionContext.Implicits.global
import java.util.Scanner
import scala.::
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer

@main def Main(): Unit =
  val scanner = new Scanner(System.in)
  val website_url = "https://en.wikipedia.org/wiki/Main_Page"
  //  val website_url = "https://isc.hevs.ch/learn/"
  val max_links = 3
  val min_chars = 3
  //  val website_url, max_links, min_chars = scanner.nextLine()

  @tailrec
  def get_stuff(website_url: String, max_links: Int, min_chars: Int, l: scala.collection.mutable.Map[String, Int]): scala.collection.mutable.Map[String, Int] =
    val doc = Jsoup.connect(website_url).get()
    val urls = ArrayBuffer[String]()
    val text = doc.text.split(" ")
    for (i <- text) {
      if (i.length >= min_chars) {
        if (i.substring(0, 3) == "http") {
          urls.append(i)
        }
        else if (l.contains(i))
          l(i) += 1
        else
          l += (i -> 1)
      }
    }
    println("URLS : ")
    for (i <- urls)
      print(i)
    println("END OF URLS")
    if (max_links == 0 || urls.isEmpty)
      l
    else
      get_stuff(urls(0), max_links - 1, min_chars, l)
      //      for (i <- urls) {
      //        val d = Future {get_stuff(i, max_links - 1, min_chars, l)}
      //        // concat l::d

  val l = scala.collection.mutable.Map[String, Int]()
  val min_occ = 4
  val his = ListMap(get_stuff(website_url, max_links, min_chars, l).toSeq.sortWith(_._2>_._2):_*)
  for (i <- his)
    if (i(1) > min_occ)
      println(i)

Main()



def conc(): Unit =
  def text(url: String): String =
    val d = Jsoup.connect("https://en.wikipedia.org/").get()
    d.select("p").text

  val d0 = Future {text("https://en.wikipedia.org/")}
  val d1 = Future {text("https://rts.ch/")}
  val dx = for x <- d0; y <- d1 yield x ++ y
  println(Await.result(dx, Duration(1000, MILLISECONDS)))
