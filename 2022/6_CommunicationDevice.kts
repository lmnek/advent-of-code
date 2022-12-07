//https://adventofcode.com/2022/day/6#part2

println(betterSolution(14))

fun betterSolution(windowSize: Int) = readln().withIndex().windowed(windowSize, 1).first{
        window -> window.map{ it.value }.toSet().size == windowSize }.last().index + 1

fun firstSolution(bufferLength: Int): Int {
    val buffer = arrayOfNulls<Char>(bufferLength)
    var counter = 0
    for(char in readLine()!!){
        buffer[counter % bufferLength] = char
        if(counter >= bufferLength - 1 && buffer.toSet().size == bufferLength)
            break
        counter+=1
    }
    return counter + 1
}

