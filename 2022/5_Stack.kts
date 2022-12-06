//https://adventofcode.com/2022/day/5

val stacks = Array(9) { ArrayDeque<Char>() }

generateSequence { readLine() }.forEach { line ->
    if('[' in line)
        line.forEachIndexed { i, char -> if (char == '[') stacks[i / 4].addFirst(line[i + 1]) }
    else if( line.startsWith("move"))
        part1(Command.fromLine(line, stacks))
}
stacks.forEach { print(it.last()) }

fun part1(cmd: Command) = repeat(cmd.moveTimes){ cmd.stackTo.addLast(cmd.stackFrom.removeLast()) }

fun part2(cmd: Command) {
    val tmpStack = ArrayDeque<Char>()
    repeat(cmd.moveTimes){ tmpStack.addLast(cmd.stackFrom.removeLast()) }
    while(!tmpStack.isEmpty()) { cmd.stackTo.addLast(tmpStack.removeLast()) }
}

data class Command(val moveTimes: Int, val stackFrom: ArrayDeque<Char>, val stackTo: ArrayDeque<Char>){
    companion object {
        fun fromLine(line: String, stacks: Array<ArrayDeque<Char>>): Command {
            val words = line.split(' ')
            return Command(words[1].toInt(), stacks[words[3].toInt()-1], stacks[words[5].toInt()-1])
        }
    }
}
