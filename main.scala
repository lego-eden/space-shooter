//> using scala 3.8.3
//> using dep io.github.lego-eden::bearlyb:0.2.5
//> using option -deprecation
// //> using javaOptions -agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005

package spacegame

import bearlyb as bl
import bl.*

import scala.language.experimental.multiSpreads

@main
def main(): Unit =
  bl.init(Init.Video, Init.Events)
  try
    Game.run()
  finally
    bl.quit()
