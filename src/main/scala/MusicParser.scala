import java.io.{PushbackReader, StringReader}
import Musica._
import scala.collection.mutable.ListBuffer

case class Note(name: String)

class MusicParser(input: String) {
  protected val inputStream = new PushbackReader(new StringReader(input))
  private var patronParser: PatronParser = null

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

      patronParser = new PatronParser(repeticiones)

      next = parseChar()
      if (next != '(') throw new NotAValidPatronException(next)

      val currentPatron = patronParser.searchPatron(this)
      return currentPatron.parse()
    }

    if (next == ')') {
      throw new EndOfPatronException()
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
      case _: EndOfPatronException =>
    }
    return result
  }
}

class PatronParser(repeticiones: Int) {
  protected var patron: List[Nota] = List[Nota]()

  def searchPatron(musicParser: MusicParser): PatronParser = {
    patron = patron ::: musicParser.parse()
    return this
  }

  def parse(): List[Nota] = {
    val patronRepetido: List[Nota] = List.fill(repeticiones)(patron).flatten
    return patronRepetido
  }
}

class ParserException(reason: String) extends Exception(reason)
class EOIParserException extends ParserException("reached end of input")
class NotANoteException(val read: Char) extends ParserException(s"Expected [A|B|C|D|E|F|G] but got $read")
class NotAValidPatronException(val read: Char) extends ParserException(s"Expected NUMx([A|B|C|D|E|F|G]+) but got $read")
class EndOfPatronException() extends ParserException("End of the Patron")