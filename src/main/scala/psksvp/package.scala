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
    prefix + java.util.UUID.randomUUID().toString.replace("-", "")
  }

  def evalPython(src:String, python:String="/usr/bin/python"):String=
  {
    //println("-----------------------")
    //println(src)
    //println("------------------------")
    import sys.process._
    val code = src.replaceAll("\n", ";").replaceAll(";;", ";").trim
    (Seq(python.trim, "-c", code).!!).trim
  }

  def removeDuplicate[T](ls:List[T]):List[T] =
  {
    var result:List[T] = Nil
    for(e <- ls)
    {
      if(false == result.contains(e))
        result = result :+ e
    }
    result
  }

  import scala.reflect.ClassTag
  def removeElement[T:ClassTag, E <: T:ClassTag](ls:List[T]):List[T] = ls match
  {
    case Nil            => Nil
    case (_:E) :: rest  => removeElement[T, E](rest)
    case a :: rest      => a :: removeElement[T, E](rest)
  }

  def booleanVector(i:Int, bits:Int): Vector[Boolean] =
  {
    require(i <= Integer.parseInt("1"*bits, 2), "psksvp.booleanArray(i, bits) bits is too small for i ")
    var result = scala.collection.immutable.Vector.empty[Boolean]
    val format = "%" + bits + "s"
    val binStr = String.format(format, Integer.toBinaryString(i)).replace(" ", "0")
    for((c, i) <- binStr zip (0 until bits))
    {
      result = result :+ (if('1' == c) true else false)
    }
    result
  }

}

