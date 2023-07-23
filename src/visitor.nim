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
    rtrBoolean
    rtrNone

  RTResult = ref object
    case kind: RTResultKind
    of rtrInt: intValue: int
    of rtrFloat: floatValue: float
    of rtrString: stringValue: string
    of rtrNone: noneValue: string
    of rtrBoolean: boolValue: bool
  
  RTFunction = ref object
    name: string
    arguments: seq[string]
    returnName: string
    body: seq[Statement]
  
  BuiltinFunction = proc (args: seq[RTResult]): RTResult

  CallFrame = ref object
    variables: Table[string, RTResult]

var callStack = newSeq[CallFrame]()

var rtfunctions = initTable[string, RTFunction]()

var builtin_functions = {
  "print": proc (args: seq[RTResult]): RTResult =
    for i, a in args:
      case a.kind
      of rtrString:
        stdout.write(a.stringValue)
      of rtrInt:
        stdout.write(a.intValue)
      of rtrFloat:
        stdout.write(a.floatValue)
      of rtrBoolean:
        stdout.write(a.boolValue)
      of rtrNone:
        stdout.write(a.noneValue)

      if i < len(args) - 1:
        stdout.write(" ")
    stdout.write("\n")
    stdout.flushFile()
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
    returnName: node.returnName,
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
      callArgs: Table[string, RTResult]
      returnValue: RTResult

    for i, argument in function.arguments:
      callArgs[argument] = arguments[i]

    callStack.pushCallFrame(newCallFrame(callArgs))
    for statement in function.body:
      discard self.visitStatement(statement)

    if function.returnName != "":
      returnValue = callStack.getObjectFromCallFrame(function.returnName)
    else:
      returnValue = rtResultNone()
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

proc visitBooleanExpr(self: Visitor, node: BooleanExpr): RTResult =
  return RTResult(kind: rtrBoolean, boolValue: node.value)

proc visitWhenExpr(self: Visitor, node: WhenExpr): RTResult =
  let conditionExpr = self.visitExpr(node.condition)

  var condition: bool
  case conditionExpr.kind
  of rtrBoolean:
    condition = conditionExpr.boolValue
  of rtrInt:
    condition = conditionExpr.intValue == 1
  of rtrFloat:
    condition = conditionExpr.floatValue == 1.0
  of rtrString:
    condition = conditionExpr.stringValue == "true"
  of rtrNone:
    condition = false

  var returnValue: RTResult
  if condition:
    for statement in node.body:
      returnValue = self.visitStatement(statement)
  return returnValue

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
  of ekBoolean:
    return self.visitBooleanExpr(BooleanExpr(node))
  of ekWhen:
    return self.visitWhenExpr(WhenExpr(node))

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
