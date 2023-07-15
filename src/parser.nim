import std/strutils
import print

type
  TokenKind = enum
    Eof,
    Func,
    Begin,
    End,
    Ident,
    OParen,
    CParen,
    String,
    SemiColon,
    Comma,

  Token = object
    kind: TokenKind
    text: string

  UnclassifiedTokenError = object of CatchableError
  UnhandledTokenError = object of CatchableError
  UnexpectedTokenError = object of CatchableError

  Parser* = object
    tokens: seq[Token]
    current: int

  ExprKind = enum
    StringLiteral,
    FunctionCall,

  Expr = ref object of RootObj
    kind: ExprKind
  
  Statement = ref object of RootObj 
    expression: Expr

  FunctionCallExpr = ref object of Expr
    name: string
    arguments: seq[Expr]

  StringLiteralExpr = ref object of Expr
    text: string

  Function = object
    name: string
    arguments: seq[string]
    body: seq[Statement]

func newParser*(tokens: seq[Token]): Parser =
  return Parser(tokens: tokens, current: 0)

func consume(self: var Parser): Token {.discardable.} =
  let token = self.tokens[self.current]
  inc self.current
  return token

func current(self: Parser): Token =
  return self.tokens[self.current]

func previous(self: Parser): Token =
  assert(self.current - 1 >= 0)
  return self.tokens[self.current - 1]

proc parseExpr(self: var Parser): Expr =
  var current = self.current()

  if current.kind == TokenKind.Comma:
    self.consume() # consume `,`
    current = self.consume()

  case current.kind
  of TokenKind.String: # string literal
    return StringLiteralExpr(
      kind: ExprKind.StringLiteral,
      text: current.text,
    )
  of TokenKind.Ident: # function call
    var functionCallExpr = FunctionCallExpr(
      kind: ExprKind.FunctionCall,
      name: current.text,
      arguments: newSeq[Expr](),
    )
    self.consume() # omit `(`
    while self.consume().kind != TokenKind.CParen:
      let expression = self.parseExpr()
      functionCallExpr.arguments.add(expression)
    return functionCallExpr
  else:
    raise newException(
      UnexpectedTokenError,
      "Unhandled token " & $current.kind,
    )

proc parseStatement(self: var Parser): Statement =
  let expression = self.parseExpr()
  if self.current().kind != SemiColon:
    raise newException(
      UnexpectedTokenError,
      "Expected `;` but found " & $self.current(),
    )
  return Statement(expression: expression)

proc parseBlock(self: var Parser): seq[Statement] =
  var statements = newSeq[Statement]()
  while self.current().kind != TokenKind.End:
    let statement = self.parseStatement()
    statements.add(statement)
    self.consume()

  if self.current().kind != TokenKind.End:
    raise newException(
      UnexpectedTokenError,
      "Unclosed block. Expected `end`",
    )

  return statements

proc parseFunctionArguments(self: var Parser): seq[string] =
  var
    arguments = newSeq[string]()
    token = self.consume()

  while token.kind != TokenKind.CParen:
    if token.kind == TokenKind.Comma:
      token = self.consume()
      continue

    if token.kind != TokenKind.Ident:
      raise newException(
        UnexpectedTokenError,
        "Expected `" & $TokenKind.Ident & "` but got `" & $token.kind & "`",
      )

    arguments.add(token.text)
    token = self.consume()

  if token.kind != TokenKind.CParen:
    raise newException(
      UnexpectedTokenError,
      "Unclosed parenthesis. Expected `)`",
    )

  return arguments

proc parseFunction(self: var Parser): Function =
  var
    function: Function
    token = self.consume()

  while token.kind != TokenKind.End:
    case token.kind
    of TokenKind.Ident:
      function.name = token.text
    of TokenKind.OParen:
      function.arguments = self.parseFunctionArguments()
    of TokenKind.Begin:
      function.body = self.parseBlock()
      break # nothing left to parse
    else: discard
    token = self.consume()
  return function

proc parse*(self: var Parser) =
  let token = self.consume()

  case token.kind
  of TokenKind.Func:
    let function = self.parseFunction()
    print(function)
  else:
    raise newException(
      UnhandledTokenError,
      "Found an unhandled token `" & $token.kind & "`",
    )

proc tokenize*(text: string): seq[Token] =
  var
    tokens = newSeq[Token](0)
    i = 0

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
      of "function":
        kind = TokenKind.Func
      of "end":
        kind = TokenKind.End
      of "begin":
        kind = TokenKind.Begin
      else:
        kind = TokenKind.Ident

      tokens.add(Token(kind: kind, text: buf))
      continue

    case text[i]
    of '(':
      tokens.add(Token(kind: TokenKind.OParen, text: "("))
    of ')':
      tokens.add(Token(kind: TokenKind.CParen, text: ")"))
    of ';':
      tokens.add(Token(kind: TokenKind.SemiColon, text: ";"))
    of ',':
      tokens.add(Token(kind: TokenKind.Comma, text: ","))
    of '"':
      inc i
      var buf: string
      while text[i] != '"':
        buf = buf & $text[i]
        inc i

      tokens.add(Token(kind: TokenKind.String, text: buf))
    else:
      raise newException(
        UnclassifiedTokenError,
        "Found an unclassified token `" & $text[i] & "`",
      )
    inc i
    continue

  return tokens
