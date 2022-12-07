// https://adventofcode.com/2022/day/7

readlnOrNull() // $ cd /
var resultPart1 = 0
var sizes = mutableListOf<Int>()
val usedSpace = recursion()

println("Part1: $resultPart1")

val neededSpace = 30000000 - (70000000 - usedSpace)
val resultPart2 = sizes.fold(null){ accumulator: Int?, num ->
    if(num >= neededSpace && (accumulator == null || num < accumulator)) num else accumulator
}
println("Part2: $resultPart2")

fun recursion(): Int{
    var size = 0
    var line = readlnOrNull()
    while(line != null) {
        val words = line.split(' ')
        size += when (words[0]) {
            "$" -> if (words[1] == "cd") when (words[2]) {
                    ".." -> break
                    else -> recursion() } else 0
            "dir" -> 0
            else -> words[0].toInt()
        }
        line = readlnOrNull()
    }
    // part 1
    if(size <= 100000)
        resultPart1+= size
    // part 2
    sizes.add(size)
    return size
}