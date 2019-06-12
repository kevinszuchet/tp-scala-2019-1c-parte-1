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

      println("Entro al patron", patronParser)
      patronParser.searchPatron(this)
      return patronParser.parse()
    }

    if (next == ')') {
      return List.empty
    }

    List[Nota](Nota.notas.find(_.toString == next.toString()).getOrElse(throw new NotANoteException(next)))
  }

  def parse(): List[Nota] = {
    var result: List[Nota] = List()
    try while (true) {
      val notas: List[Nota] = parseNote()
      println("Las notas", notas)
      result = result ::: notas
      println("El resultado", result)
    }
    catch {
      case _: EOIParserException =>
    }
    return result
  }
}

class PatronParser(repeticiones: Int) {
  protected var patron: List[Nota] = List[Nota]()

  def searchPatron(musicParser: MusicParser): Unit = {
    patron = patron ::: musicParser.parse()
    println("Patron completo", patron)
  }

  def parse(): List[Nota] = {
    println("Sale del patron", patron)
    val patronRepetido: List[Nota] = List.fill(repeticiones)(patron).flatten
    patron = List[Nota]()
    return patronRepetido
  }
}

class ParserException(reason: String) extends Exception(reason)
class EOIParserException extends ParserException("reached end of input")
class NotANoteException(val read: Char) extends ParserException(s"Expected [A|B|C|D|E|F|G] but got $read")
class NotAValidPatronException(val read: Char) extends ParserException(s"Expected NUMx([A|B|C|D|E|F|G]+) but got $read")