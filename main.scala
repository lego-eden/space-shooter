//> using scala 3.8.3
//> using dep io.github.lego-eden::bearlyb::0.2.4
//> using option -Wall

package spacegame

import bearlyb as bl
import bl.*

@main
def main(): Unit =
  bl.init(Init.Video, Init.Events)
  try
    run()
  finally
    bl.quit()
