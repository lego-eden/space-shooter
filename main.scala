//> using dep io.github.lego-eden::bearlyb::0.2.2
//> using option -Wall

import bearlyb as bl
import bl.*

@main
def main(): Unit =
  bl.init(Init.Video, Init.Events)
  try
    run()
  finally
    bl.quit()
