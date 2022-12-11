// https://adventofcode.com/2022/day/11

typealias WorryLevel = Long
class Monkey(val divisibleBy: Int, private val items: ArrayDeque<WorryLevel>, val operation: (WorryLevel) -> WorryLevel, private val monkeyWhenTrue: Int, private val monkeyWhenFalse: Int) {
    var inspectionsCount = 0L
    fun throwItems(divisor: Int?) {
        while(!items.isEmpty()) {
            inspectionsCount++
            var itemWorryLevel = items.removeFirst()
            itemWorryLevel = operation(itemWorryLevel)
            if (divisor == null) { itemWorryLevel /= 3 }
            else { itemWorryLevel %= divisor }
            monkeys[if (itemWorryLevel % divisibleBy == 0L) monkeyWhenTrue else monkeyWhenFalse]?.items?.addLast(itemWorryLevel)
        }
    }
}

fun String.lastNumber() = this.substringAfterLast(" ").toInt()

// parse input
val monkeys = generateSequence(::readLine).joinToString("\n").split("Monkey ").drop(1).associate {
    val lines = it.split("\n")
    val operation = lines[2].substringAfter("= ").split(" ").map { a -> a.trim() }
    lines[0].substringBefore(":").toInt() to Monkey(lines[3].lastNumber(),
        ArrayDeque(lines[1].substringAfter(":").split(",").map { num -> num.trim().toLong() }.toList()),
        { old: WorryLevel ->
            val second = operation[2].toLongOrNull() ?: old
            if (operation[1] == "+") old + second else old * second
        },
        lines[4].lastNumber(),
        lines[5].lastNumber()
    )
}

// modulo WorryLevels by this number -> monkeys see it the same and it stops the numbers from overflowing
// even better would be to count least common divisor (lcm)
val divisor = monkeys.map { (_, monkey) -> monkey.divisibleBy }.reduce{ a, b -> a * b}

//println("Part1: ${figureOutMonkeys(20, null)}")
println("Part2: ${figureOutMonkeys(10_000, divisor)}")

// solve
fun figureOutMonkeys(roundCount: Int, divisor: Int?): WorryLevel{
    repeat(roundCount){ monkeys.forEach { (_, monkey) -> monkey.throwItems(divisor) } }
    return monkeys.map { (_, monkey) -> monkey.inspectionsCount }.sortedDescending().take(2).reduce {a, b -> a * b}
}