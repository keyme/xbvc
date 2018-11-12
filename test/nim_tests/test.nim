import xbvc
import options
import threadpool

let
  fluffAry = [1.uint8, 2.uint8, 3.uint8, 4.uint8, 5.uint8,
              6.uint8, 7.uint8, 8.uint8, 9.uint8, 0.uint8]

var
  getCommandTriggered = false

proc getCommandCallback(msg: XBVCMessage) =
  echo "Got command callback"
  assert msg.get_command.target == 0xdeadbeef.uint32
  assert msg.get_command.fluff == fluffAry
  getCommandTriggered = true

# Test basic serialization

let rsp = GetResponseMessage(error: 123.uint32,
                             target: 456.uint32,
                             index: -32.int16,
                             foo: 128.uint8,
                             result: -45.int32,
                             bar: 5.uint8,
                             version: -0.07.float32,
                             floatList: [-1.1.float32,
                                         0.07.float32,
                                         3.3.float32,
                                         4.4.float32,
                                         5.5.float32])

let encoded = rsp.serialize(0xdeadbeef.uint32, 0xbeefcafe.uint32)
let decoded = deserializeGetResponse(encoded[1..^1])
if decoded.isSome():
  var (_, decInternal) = decoded.get()
  # clear random id and response id so equality checks on
  # everything else
  decInternal.randomId = 0
  decInternal.responseTo = 0
  echo rsp
  echo decInternal

  assert decInternal == rsp
  echo "decoded GetResponse successfully"
else:
  echo "Not able to decode"

echo rsp

var ep = newDispatcher()
ep.registerCallback(xmGetCommand, getCommandCallback)

var cmd = GetCommandMessage()
cmd.target = 0xdeadbeef.uint32
cmd.fluff = fluffAry

assert gtRPM == 0.GetTarget
assert gtTPS == 1.GetTarget
assert gtMAP == 2.GetTarget
assert gtIAT == 3.GetTarget

let (_, data) = ep.encode(cmd)
discard ep.dispatch(data)

for _ in 0..1_000_000:
  if getCommandTriggered:
    echo "Test was successful"
    quit(0)

echo "Never got get command!"
quit(-1)
