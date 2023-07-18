import std/strutils

type
  TokenKind = enum
    tkEof,
    tkFunc,
    tkBegin,
    tkEnd,
    tkIdent,
    tkOParen,
    tkCParen,
    tkString,
    tkSemiColon,
    tkComma,
    tkVariableRef

  Token = object
    kind: TokenKind
    text: string

  UnclassifiedTokenError = object of CatchableError
  UnhandledTokenError = object of CatchableError
  UnexpectedTokenError = object of CatchableError

  Parser* = object
    tokens: seq[Token]
    current: int
  
  NodeKind* = enum
    nkFunction,
    nkStatement,
    nkExpression,

  Node* = ref object of RootObj
    nodeKind*: NodeKind

  ExprKind* = enum
    ekStringLiteral,
    ekFunctionCall,
    ekVariableRef

  Expr* = ref object of Node
    exprKind*: ExprKind
  
  Statement* = ref object of Node
    expression*: Expr

  FunctionCallExpr* = ref object of Expr
    name*: string
    arguments*: seq[Expr]

  StringLiteralExpr* = ref object of Expr
    text*: string

  VariableRefExpr* = ref object of Expr
    name*: string

  Function* = ref object of Node
    name*: string
    arguments*: seq[string]
    body*: seq[Statement]

func newParser*(tokens: seq[Token]): Parser =
  return Parser(tokens: tokens, current: -1)

proc consume(self: var Parser): Token {.discardable.} =
  inc self.current
  return self.tokens[self.current]

func current(self: Parser): Token =
  return self.tokens[self.current]

proc parseExpr(self: var Parser): Expr =
  var current = self.current()

  if current.kind == TokenKind.tkComma:
    current = self.consume() # consume `,`

  case current.kind
  of tkString: # string literal
    return StringLiteralExpr(
      nodeKind: nkExpression,
      exprKind: ekStringLiteral,
      text: current.text,
    )
  of tkIdent: # function call
    var functionCallExpr = FunctionCallExpr(
      nodeKind: nkExpression,
      exprKind: ekFunctionCall,
      name: current.text,
      arguments: newSeq[Expr](),
    )
    self.consume() # consume `(`
    var token = self.consume()
    while token.kind != tkCParen:
      let expression = self.parseExpr()
      functionCallExpr.arguments.add(expression)
      token = self.consume()
    self.consume()
    return functionCallExpr
  of tkVariableRef: # variable reference
    return VariableRefExpr(
      nodeKind: nkExpression,
      exprKind: ekVariableRef,
      name: current.text,
    )
  else:
    raise newException(
      UnexpectedTokenError,
      "parseExpr(): Unhandled token " & $current,
    )

proc parseStatement(self: var Parser): Statement =
  let expression = self.parseExpr()
  if self.current().kind != tkSemiColon:
    raise newException(
      UnexpectedTokenError,
      "parseStatement(): Expected `;` but found " & $self.current(),
    )
  return Statement(
    nodeKind: nkStatement,
    expression: expression,
  )

proc parseBlock(self: var Parser): seq[Statement] =
  var statements = newSeq[Statement]()

  while self.consume().kind != tkEnd:
    let statement = self.parseStatement()
    statements.add(statement)

  if self.current().kind != tkEnd:
    raise newException(
      UnexpectedTokenError,
      "parseBlock(): Unclosed block. Expected `end`",
    )
  return statements

proc parseFunctionArguments(self: var Parser): seq[string] =
  var
    arguments = newSeq[string]()
    token = self.consume()

  while token.kind != tkCParen:
    if token.kind == tkComma:
      token = self.consume()
      continue

    if token.kind != tkIdent:
      raise newException(
        UnexpectedTokenError,
        "parseFunctionArguments() Expected `" &
        $TokenKind.tkIdent &
        "` but got `" & $token.kind & "`",
      )

    arguments.add(token.text)
    token = self.consume()

  if token.kind != tkCParen:
    raise newException(
      UnexpectedTokenError,
      "parseFunctionArguments(): Unclosed parenthesis. Expected `)`",
    )

  return arguments

proc parseFunction(self: var Parser): Function =
  var
    token = self.consume()
    name: string
    arguments: seq[string]
    body: seq[Statement]

  while token.kind != tkEnd:
    case token.kind
    of tkIdent:
      name = token.text
    of tkOParen:
      arguments = self.parseFunctionArguments()
    of tkBegin:
      body = self.parseBlock()
      break # nothing left to parse
    else: discard
    token = self.consume()
  return Function(
    nodeKind: nkFunction,
    name: name,
    arguments: arguments,
    body: body
  )

proc parse*(self: var Parser): seq[Node] =
  var
    token = self.consume()
    nodes = newSeq[Node]()

  while token.kind != tkEof:
    case token.kind
    of tkFunc: # function declaration
      let function = self.parseFunction()
      nodes.add(function)
    of tkIdent: # function call
      let statement = self.parseStatement()
      nodes.add(statement)
    else:
      raise newException(
        UnhandledTokenError,
        "parse(): Found an unhandled token `" & $token.kind & "`",
      )
    token = self.consume()
  return nodes

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
        kind = tkFunc
      of "end":
        kind = tkEnd
      of "begin":
        kind = tkBegin
      else:
        kind = tkIdent

      tokens.add(Token(kind: kind, text: buf))
      continue

    case text[i]
    of '(':
      tokens.add(Token(kind: tkOParen, text: "("))
    of ')':
      tokens.add(Token(kind: tkCParen, text: ")"))
    of ';':
      tokens.add(Token(kind: tkSemiColon, text: ";"))
    of ',':
      tokens.add(Token(kind: tkComma, text: ","))
    of '"':
      inc i
      var buf: string
      while text[i] != '"':
        buf &= $text[i]
        inc i
      tokens.add(Token(kind: tkString, text: buf))
    of '$':
      inc i
      var buf: string
      while isAlphaNumeric(text[i]):
        buf &= $text[i]
        inc i
      tokens.add(Token(kind: tkVariableRef, text: buf))
      dec i # magic trick
    else:
      raise newException(
        UnclassifiedTokenError,
        "tokenize(): Found an unclassified token `" & $text[i] & "`",
      )
    inc i
    continue

  tokens.add(Token(kind: tkEof, text: ""))
  return tokens
