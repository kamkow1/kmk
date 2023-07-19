import std/tables
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
    name: string
    arguments: seq[string]
    body: seq[Statement]
  
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

func topCallFrame(callStack: seq[CallFrame]): CallFrame =
  return callStack[len(callStack) - 1]

func pushCallFrame(callStack: var seq[CallFrame], frame: CallFrame) =
  callStack.add(frame)

func popCallFrame(callStack: var seq[CallFrame]) =
  let l = Natural(len(callStack) - 1)
  callStack.delete(l)

func newObjectInCallFrame(callStack: var seq[CallFrame], name: string, value: RTREsult) =
  callStack[len(callStack) - 1].variables[name] = value

func getObjectFromCallFrame(callStack: var seq[CallFrame], name: string): RTResult =
  return callStack.topCallFrame().variables[name]

func removeObjectFromCallFrame(callStack: var seq[CallFrame], name: string): RTResult {.discardable.} =
  let value = callStack.getObjectFromCallFrame(name)
  callStack[len(callStack) - 1].variables.del(name)
  return value

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

    callStack.pushCallFrame(newCallFrame(callArgs))
    for statement in function.body:
      returnValue = self.visitStatement(statement)
    callStack.popCallFrame()
    return returnValue
  return rtResultNone()

proc visitVariableRefExpr(self: Visitor, node: VariableRefExpr): RTResult =
  return callStack.getObjectFromCallFrame(node.name)

proc visitVariableAssignmentExpr(self: Visitor, node: VariableAssignmentExpr): RTResult =
  let
    name = node.name
    value = self.visitExpr(node.value)

  callStack.newObjectInCallFrame(name, value)
  return value

proc visitVariableUnsetExpr(self: Visitor, node: VariableUnsetExpr): RTResult =
  let name = node.name
  return callStack.removeObjectFromCallFrame(name)

proc visitExpr(self: Visitor, node: Expr): RTResult =
  case node.exprKind:
  of ekFunctionCall:
    return self.visitFunctionCallExpr(FunctionCallExpr(node))
  of ekStringLiteral:
    return self.visitStringLiteralExpr(StringLiteralExpr(node))
  of ekVariableRef:
    return self.visitVariableRefExpr(VariableRefExpr(node))
  of ekAssignment:
    return self.visitVariableAssignmentExpr(VariableAssignmentExpr(node))
  of ekUnsetVariable:
    return self.visitVariableUnsetExpr(VariableUnsetExpr(node))

proc visitStatement(self: Visitor, node: Statement): RTResult =
  return self.visitExpr(node.expression)

proc visit*(self: Visitor) =
  for node in self.nodes:
    case node.nodeKind:
    of NodeKind.nkFunction:
      discard self.visitFunction(Function(node))
    of NodeKind.nkStatement:
      discard self.visitStatement(Statement(node))
    of NodeKind.nkExpression:
      discard self.visitExpr(Expr(node))
