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
    import psksvp.FileSystem.SimpleFileIO
    val tmpDir = System.getProperty("java.io.tmpdir")
    val srcFileName = s"$tmpDir${psksvp.gensym()}.py"
    SimpleFileIO.writeStringToTextFile(src, srcFileName)
    import sys.process._

    (Seq(python, srcFileName).!!).trim
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


  def overflow(n:Int, bits:Int) = !(n <= Integer.parseInt("1"*bits, 2))

  def maxValue(bits:Int) = Integer.parseInt("1"*bits, 2)

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

  def octal(s:String):Int = Integer.parseInt(s, 8)

  var counter = 0
  def gensym(prefix:String="symbol"):String =
  {
    counter = counter + 1
    s"$prefix$counter"
  }


  val RG = new java.util.Random()
  def random(n:Int):Int = RG.nextInt(n)


  /**
    * http://stackoverflow.com/questions/5955797/scala-method-to-combine-each-element-of-an-iterable-with-each-element-of-another
    * code modified from above
    * @param xx
    * @return
    */
  def crossProduct[T](xx: List[List[T]]): List[List[T]] =
  {
    def count[T](xx: List[List[T]]): Int = (1 /: xx) (_ * _.length)

    def combination[T](xx: List[List[T]], i: Int): List[T] = xx match
    {
      case Nil => Nil
      case x :: xs => x(i % x.length) :: combination (xs, i / x.length)
    }

    (0 until count(xx)).toList.map(i => combination(xx, i))
  }
}

