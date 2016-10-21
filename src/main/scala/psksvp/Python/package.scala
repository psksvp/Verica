package psksvp

/**
  * Created by psksvp on 21/10/16.
  */
package object Python
{
  def execute(code:String,
              host:String="localhost",
              port:Int = 5000): String =
  {
    import scalaj.http.{Http, HttpResponse}
    val respond:HttpResponse[String] = Http(s"http://$host:$port/exec").postForm(Seq("code" -> code, "quit" -> "False")).asString
    respond.body.trim
  }
}
