import std/[tables, sequtils]
import parser
import print

type
  Visitor* = ref object
    nodes: seq[Node]

  RTResultKind = enum
    rtrInt
    rtrFloat
    rtrString
    rtrNone

  RTResult = ref object
    case kind: RTResultKind
    of rtrInt: intValue: int
    of rtrFloat: floatValue: float
    of rtrString: stringValue: string
    of rtrNone: noneValue: string
  
  RTFunction = ref object
    name*: string
    arguments*: seq[string]
    body*: seq[Statement]
  
  BuiltinFunction = proc (args: seq[RTResult]): RTResult

  CallFrame = ref object
    variables: Table[string, RTResult]

var callStack = newSeq[CallFrame]()

var rtfunctions = initTable[string, RTFunction]()

var builtin_functions = {
  "print": proc (args: seq[RTResult]): RTResult =
    for a in args:
      case a.kind
      of rtrString:
        echo a.stringValue
      of rtrInt:
        echo a.intValue
      of rtrFloat:
        echo a.floatValue
      of rtrNone:
        echo a.noneValue
}.toTable()

proc visitExpr(self: Visitor, node: Expr): RTResult 
proc visitStatement(self: Visitor, node: Statement): RTResult

func newCallFrame(callArgs: Table[string, RTResult]): CallFrame =
  return CallFrame(variables: callArgs)

func rtResultNone(): RTResult =
  return RTResult(kind: rtrNone, noneValue: "NONE")

func newVisitor*(nodes: seq[Node]): Visitor =
  return Visitor(nodes: nodes)

proc visitFunction(self: Visitor, node: Function): RTResult = 
  let rtf = RTFunction(
    name: node.name,
    arguments: node.arguments,
    body: node.body,
  )
  rtfunctions[node.name] = rtf
  return rtResultNone()

proc visitStringLiteralExpr(self: Visitor, node: StringLiteralExpr): RTResult =
  return RTResult(kind: rtrString, stringValue: node.text)

proc visitFunctionCallExpr(self: Visitor, node: FunctionCallExpr): RTResult =  
  let name = node.name
  var arguments = newSeq[RTResult]()

  for argument in node.arguments:
    let a = self.visitExpr(argument) 
    arguments.add(a)

  if builtin_functions.hasKey(name):
    return builtin_functions[name](arguments)
  elif rtfunctions.hasKey(name):
    let function = rtfunctions[name]
    var
      returnValue: RTResult
      callArgs: Table[string, RTResult]

    for i, argument in function.arguments:
      callArgs[argument] = arguments[i]

    callStack.add(newCallFrame(callArgs))
    for statement in function.body:
      returnValue = self.visitStatement(statement)
    return returnValue
  return rtResultNone()

proc visitExpr(self: Visitor, node: Expr): RTResult =
  case node.exprKind:
  of ExprKind.ekFunctionCall:
    return self.visitFunctionCallExpr(FunctionCallExpr(node))
  of ExprKind.ekStringLiteral:
    return self.visitStringLiteralExpr(StringLiteralExpr(node))
  else:
    return rtResultNone()

proc visitStatement(self: Visitor, node: Statement): RTResult =
  return self.visitExpr(node.expression)

proc visit*(self: Visitor) =
  for node in self.nodes:
    case node.nodeKind:
    of NodeKind.nkFunction:
      discard self.visitFunction(Function(node))
    of NodeKind.nkStatement:
      discard self.visitStatement(Statement(node))
    else: discard
