import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.Directives._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}
import scala.io.StdIn


trait CrawlerJsonProtocol extends DefaultJsonProtocol {
  case class UriTitle (uri: String, title: String)
  implicit val uriTitleFormat: RootJsonFormat[UriTitle] = jsonFormat2(UriTitle)
}

object SimpleCrawler extends App with CrawlerJsonProtocol with SprayJsonSupport {

  implicit val system: ActorSystem = ActorSystem("AssWeCan")
  import system.dispatcher

  //метод, достающий тайтл из урла
  def getTitle(url: String): String = scala.io.Source.fromURL(url).mkString
    .split("</title>").head.split("<title>").tail.mkString

  //метод, безопасно выполняющий ГетТайтл и возвращающий экземпляр UriTitle
  def safeUriTitle(uri: String): UriTitle = {
    try {
      UriTitle(uri = uri, title = getTitle(uri))
    } catch {
      case e: Exception => println(s"Bad request with $uri: $e")
        UriTitle(uri, "no title for: " + uri)
    }
  }

  //оверлоад предыдущего, но для списка урлов
  def safeUriTitle(uriList: List[String]): List[UriTitle] = {
    if (uriList.isEmpty) List()
    else safeUriTitle(uriList.head) +: safeUriTitle(uriList.tail)
  }

  val getTitleRoute = {
    path("gimmeTitles") {
      post {//принимаем список УРЛ-ов (просто список, каждый УРЛ с новой строки)
        extractDataBytes { data =>
          val text = data.runFold[String]("") { (string, i) => string + i.utf8String}
          onSuccess(text) { t =>
            val rawTextAsList = t.mkString.split("\\n").map(_.trim).toList
            val uRiTitleList: List[UriTitle] = safeUriTitle(rawTextAsList)
            complete(uRiTitleList)
          }
        }
      }
    }
  }

  val bindingFuture = Http().newServerAt("localhost", 8088).bind(getTitleRoute)

  println(s"server now online. hit http://localhost:8088/gimmeTitles with POST request \nPress RETURN to stop it")
  StdIn.readLine()
  bindingFuture.flatMap(_.unbind()).onComplete(_ => system.terminate())
}


