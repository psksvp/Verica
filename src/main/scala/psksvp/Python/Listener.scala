package psksvp.Python

/**
  * Created by psksvp on 21/10/16.
  */

/**
  *
  * @param python must have flask installed e.g pip install flask
  * @param host
  * @param port
  */
class Listener(python:String="/usr/bin/python",
               host:String="127.0.0.1",
               port:Int = 5000) extends Runnable
{
  private var listening = false

  def execute(code:String):String = psksvp.Python.execute(code, host, port)

  def start:Unit=
  {
    if(!listening)
    {
      new Thread(this).start()
      listening = true
    }
  }

  def stop:Unit=
  {
    if(listening)
    {
      import scalaj.http.Http
      try
      {
        Http(s"http://$host:$port/exec").postForm(Seq("code" -> "", "quit" -> "True")).asString
      }
      catch
      {
        case _:Throwable => listening = false
      }
    }
  }

  override def run:Unit=
  {
    val listener =
      s"""
        |from flask import Flask
        |from flask import request
        |import sys
        |app = Flask(__name__)
        |
        |@app.route('/exec', methods = ['POST', 'GET'])
        |def main():
        |  quit = request.form['quit']
        |  if 'True' == quit:
        |    print('listener is exiting')
        |    sys.exit(0)
        |    return 'quitting'
        |  else:
        |    code = request.form['code']
        |    return execCode(code)
        |
        |def execCode(code):
        |  from cStringIO import StringIO
        |  old_stdout = sys.stdout
        |  redirected_output = sys.stdout = StringIO()
        |  exec(code)
        |  sys.stdout = old_stdout
        |  return redirected_output.getvalue()
        |
        |if __name__ == '__main__':
        |  app.run(host = '$host', port = $port)
      """.stripMargin

      import psksvp.FileSystem.SimpleFileIO
      val tmpProp = System.getProperty("java.io.tmpdir")
      val tmpDir = if(tmpProp.last != '/') tmpProp + '/' else tmpProp
      val srcFileName = s"$tmpDir${psksvp.gensym()}.py"
      SimpleFileIO.writeStringToTextFile(listener, srcFileName)
      import sys.process._

      Seq(python, srcFileName).!
  }
}
