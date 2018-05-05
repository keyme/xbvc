import xbvc
import options
import threadpool

let
  fluffAry = [1.uint8, 2.uint8, 3.uint8, 4.uint8, 5.uint8,
              6.uint8, 7.uint8, 8.uint8, 9.uint8, 0.uint8]

var
  getCommandTriggered = false

proc feedbackLoop(rx, tx: ptr Channel[Option[byte]]) =
  while true:
    let data = tx[].recv()
    rx[].send(data)

proc getCommandCallback(msg: XBVCMessage) =
  echo "Got command callback"
  assert msg.get_command.target == 0xdeadbeef.uint32
  assert msg.get_command.fluff == fluffAry
  getCommandTriggered = true

let ep = newEdgePoint()
ep.registerCallback(xmGetCommand, getCommandCallback)
ep.start()
spawn feedbackLoop(ep.rxChan.addr, ep.txChan.addr)

let cmd = GetCommandMessage()
cmd.target = 0xdeadbeef.uint32
cmd.fluff = fluffAry

ep.send(cmd)

for _ in 0..1_000_000:
  if getCommandTriggered:
    echo "Test was successful"
    quit(0)

echo "Never got get command!"
quit(-1)

