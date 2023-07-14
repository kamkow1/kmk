import std/strutils

type
  TokenKind = enum
    Eof,
    Fnc,
    Endfnc,
    Ident,
    OParen,
    CParen,

  Token = object
    kind: TokenKind
    text: string

  UnknownTokenError = object of CatchableError

proc tokenize*(text: string): seq[Token] =
  var tokens = newSeq[Token](0)
  var i = 0
  while i < len(text):
    var c = text[i]
    if isSpaceAscii(c):
      inc i
      continue

    # possible identifier or keyword
    if isAlphaAscii(c):
      var buf: string
      while isAlphaNumeric(c):
        buf = buf & $c
        inc i
        c = text[i]
     
      var kind: TokenKind
      case buf:
      of "fnc":
        kind = TokenKind.Fnc
      of "endfnc":
        kind = TokenKind.Endfnc
      else:
        kind = TokenKind.Ident

      tokens.add(Token(
        kind: kind,
        text: buf
      ))
      continue

    case c
    of '(':
      tokens.add(Token(
        kind: TokenKind.OParen,
        text: "("
      ))
    of ')':
      tokens.add(Token(
        kind: TokenKind.CParen,
        text: ")"
      ))
    else:
      raise newException(UnknownTokenError, "Found an unknown token `" & $c & "`")
    inc i
    continue

  return tokens

