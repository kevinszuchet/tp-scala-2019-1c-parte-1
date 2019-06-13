import java.io.{PushbackReader, StringReader}
import Musica._
import scala.collection.mutable.ListBuffer

case class Note(name: String)

class MusicParser(input: String) {
  protected val inputStream = new PushbackReader(new StringReader(input))

  protected def parseChar(): Char = {
    val parsed = inputStream.read()
    if (parsed == -1) throw new EOIParserException
    return parsed.toChar
  }
  protected def parseNote(): List[Nota] = {
    var next: Char = ' '
    do next = parseChar() while (next == ' ')

    if (next.isDigit) {
      var repeticiones: Int = 0
      do {
        repeticiones = (repeticiones * 10) + next.asDigit
        next = parseChar()
        if (!next.isDigit && next != 'x') throw new NotAValidPatronException(next)
      } while (next.toLower != 'x')

      next = parseChar()
      if (next != '(') throw new NotAValidPatronException(next)

      val pattern = this.parse()
      return List.fill(repeticiones)(pattern).flatten
    }

    if (next == ')') {
      throw new EOIParserException()
    }

    List[Nota](Nota.notas.find(_.toString == next.toString()).getOrElse(throw new NotANoteException(next)))
  }

  def parse(): List[Nota] = {
    var result: List[Nota] = List()
    try while (true) {
      val notas: List[Nota] = parseNote()
      result = result ::: notas
    }
    catch {
      case _: EOIParserException =>
    }
    return result
  }
}

class ParserException(reason: String) extends Exception(reason)
class EOIParserException extends ParserException("reached end of input")
class NotANoteException(val read: Char) extends ParserException(s"Expected [A|B|C|D|E|F|G] but got $read")
class NotAValidPatronException(val read: Char) extends ParserException(s"Expected NUMx([A|B|C|D|E|F|G]+) but got $read")