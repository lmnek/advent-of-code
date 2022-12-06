//https://adventofcode.com/2022/day/5

val stacks = Array(9) { ArrayDeque<Char>() }

generateSequence { readLine() }.forEach { line ->
    if('[' in line){
        line.forEachIndexed() { i, char -> if (char == '[') stacks[i / 4].addFirst(line[i + 1]) }
    } else if( line.startsWith("move")){
        val words = line.split(' ')
        part2(stacks, )
    }
}
stacks.forEach { print(it.last()) }

data class Command(val moveTimes: Int, val from: Int, val to: Int)

Object CommandFactory {
    fun fromLine(line: String): Command = {
        val words = line.split(' ')
        return Command(words[1].toInt(), words[3].toInt(), words[5].toInt())
    }
}

fun part1(stacks: Array<ArrayDeque<Char>>, cmd: Command) {
    for (i in 0 until moveTimes)
        stacks[to - 1].addLast(stacks[from - 1].removeLast())
}


fun part2(stacks: Array<ArrayDeque<Char>>, cmd: Command) {
    val tmpStack = ArrayDeque<Char>()
    for (i in 0 until moveTimes)
        tmpStack.addLast(stacks[from - 1].removeLast())
    for(i in 0 until tmpStack.count())
        stacks[to - 1].addLast(tmpStack.removeLast())
}