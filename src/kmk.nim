import os
import system
import std/[parseopt, strformat]

import parser

proc printHelp =
  echo fmt"Usage:"
  echo fmt"{paramStr(0)} [file] [options]"

proc main: int =
  var
    optParser = initOptParser(commandLineParams())
    filename: string

  for kind, key, val in optParser.getopt():
    case kind
    of cmdArgument:
      filename = key
    of cmdLongOption, cmdShortOption:
      case key
      of "help", "h":
        printHelp()
    of cmdEnd:
      assert(false)

  if filename == "":
    writeLine(stderr, "Error: no input file provided")
    return 1

  let file = open(filename, fmRead)
  defer: close(file)
 
  let text = readAll(file)
  let tokens = tokenize(text)
  echo tokens

  return 0

when isMainModule:
  quit(main())
