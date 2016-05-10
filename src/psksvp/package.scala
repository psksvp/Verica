/**
  * Created by psksvp on 19/03/2016.
  */
package object psksvp
{
  def extractYoutubePlayListURLs(targetURL:String,
                                 startMarker:String = "watch?v=",
                                 endMarker:String = ";"): List[String] =
  {
    import scala.io.Source

    val result = new scala.collection.mutable.ListBuffer[String]()
    val src = Source.fromURL(targetURL).mkString

    val vdoCodeList = extractString(src, startMarker, endMarker)
    for(code <- vdoCodeList)
    {
      val url = "\"http://www.youtube.com/"+code+"\""
      if(result.indexOf(url) < 0)
      {
        result.append(url)
      }
    }
    result.toList
  }


  /**
    *
    * @param src
    * @param startMarker
    * @param endMarker
    * @return List[String]
    */
  def extractString(src:String,
                    startMarker:String,
                    endMarker:String):List[String]=
  {
    var ls:List[String] = Nil
    var start = src.indexOf(startMarker)
    if(0 <= start)
    {
      val end = src.indexOf(endMarker, start)
      if (end >= start)
        ls = List(src.substring(start + startMarker.length, end)) ::: extractString(src.substring(end),
                                                                                    startMarker,
                                                                                    endMarker)
    }

    ls
  }

  def uuidString(prefix:String="L"):String=
  {
    prefix + java.util.UUID.randomUUID().toString().replace("-", "")
  }

  def evalPython(src:String, python:String="/usr/bin/python"):String=
  {
    import sys.process._
    val code = src.replaceAll("\n", ";").trim
    (Seq(python.trim, "-c", code).!!).trim
  }

  def removeDuplicate[T](ls:List[T]):List[T] =
  {
    var result:List[T] = Nil
    for(e <- ls)
    {
      if(false == result.contains(e))
        result = result ::: List(e)
    }
    result
  }
}

