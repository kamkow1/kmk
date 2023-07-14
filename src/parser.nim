import std/strutils

type
  TokenKind = enum
    Eof,
    Fnc,
    Endfnc,
    Ident,
    OParen,
    CParen,
    String,
    SemiColon,

  Token = object
    kind: TokenKind
    text: string

  UnknownTokenError = object of CatchableError

proc tokenize*(text: string): seq[Token] =
  var tokens = newSeq[Token](0)
  var i = 0
  while i < len(text):
    if isSpaceAscii(text[i]):
      inc i
      continue

    # possible identifier or keyword
    if isAlphaAscii(text[i]):
      var buf: string
      while isAlphaNumeric(text[i]):
        buf = buf & $text[i]
        inc i
     
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
        text: buf,
      ))
      continue

    case text[i]
    of '(':
      tokens.add(Token(
        kind: TokenKind.OParen,
        text: "(",
      ))
    of ')':
      tokens.add(Token(
        kind: TokenKind.CParen,
        text: ")",
      ))
    of ';':
      tokens.add(Token(
        kind: TokenKind.SemiColon,
        text: ";",
      ))
    of '"':
      inc i
      var buf: string
      while text[i] != '"':
        buf = buf & $text[i]
        inc i

      tokens.add(Token(
        kind: TokenKind.String,
        text: buf,
      ))
    else:
      raise newException(UnknownTokenError, "Found an unknown token `" & $text[i] & "`")
    inc i
    continue

  return tokens

