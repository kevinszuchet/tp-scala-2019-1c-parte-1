object PlayAudio extends App {
  //val sariaSong = "F A 2x(C D 2x(C D E)) B B"
  val sariaSong = "F A 2x(C D 3x(G E)) B B"
  // val sariaSong = "F A B B F A B B F A B E D D B C B G E E D D E G E F A B B F A B B F A B E D D B C E B G G D E G E"

  AudioPlayer.reproducir(sariaSong)
}
