//https://adventofcode.com/2022/day/10
val commands = generateSequence(::readLine).map { Pair(it.substringBefore(" "), it.substringAfter(" ")) }.toList()

var xRegister =  1
val frameBuffer = List(6) { MutableList(40) { false } }
val cmdIter = commands.iterator()
var signalStrength = 0
var addxNum: Int? = null
var addxRunning = false

for(cycle in 0 until 240){
    if ((cycle + 1) % 40 == 20) signalStrength += (cycle + 1) * xRegister

    if(addxNum == null && cmdIter.hasNext()){
        val cmd = cmdIter.next()
        if (cmd.first == "addx"){
            addxNum = cmd.second.toInt()
            addxRunning = true
        }
    }

    val row: Int = cycle / 40
    val col = cycle % 40
    frameBuffer[row][col] = col in xRegister - 1.. xRegister + 1

    if(addxNum != null) {
        if(!addxRunning){
            xRegister += addxNum!!
            addxNum = null
        } else addxRunning = false
    }
}

println("Part1: Signal strength = $signalStrength")
println("Part2:")
frameBuffer.forEach{ row ->
    row.forEach { print(if(it) "#" else ".") }
    print("\n")
}