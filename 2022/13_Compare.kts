// https://adventofcode.com/2022/day/13

val components: List<Component> = generateSequence(::readLine)
    .filter { it.isNotEmpty() }.map{ it.toComponent() }.toList()

println("Part1: ${part1(components)}")
println("Part2: ${part2(components)}")

fun part1(components: List<Component>): Int = components
    .chunked(2)
    .mapIndexedNotNull{ i, (o1, o2) -> if(o1 <= o2) i + 1 else null }
    .sum()

fun part2(components: List<Component>): Int{
    val dividers = arrayListOf("[[2]]".toComponent(), "[[6]]".toComponent())
    val newComponents = components.toMutableList()
    newComponents.addAll(dividers)
    newComponents.sort()
    return (newComponents.indexOf(dividers[0]) + 1) * (newComponents.indexOf(dividers[1]) + 1)
}

// composite design pattern + comparable interface
sealed interface Component : Comparable<Component>{
    override fun compareTo(other: Component): Int
}

class Composite(private val components: List<Component>) : Component {
    override fun compareTo(other: Component): Int {
        return when (other) {
            is Number -> this.compareTo(Composite(listOf(other)))
            is Composite -> {
                var result: Int?
                val iter1 = this.components.iterator()
                val iter2 = other.components.iterator()
                while (iter1.hasNext() && iter2.hasNext()) {
                    result = iter1.next().compareTo(iter2.next())
                    if (result != 0) return result
                }
                if (!iter1.hasNext() && !iter2.hasNext()) 0
                else if (iter1.hasNext()) 1
                else if (iter2.hasNext()) -1
                else throw Error()
            }
            else -> throw Error()
        }
    }
}

class Number(private val value: Int) : Component {
    override fun compareTo(other: Component): Int = when(other) {
        is Number -> value.compareTo(other.value)
        is Composite -> Composite(listOf(this)).compareTo(other)
        else -> throw Error()
    }
}

// input parser
fun String.toComponent(): Component {
    val str = (if(startsWith(',')) substring(1) else this).trim()
    return if(str == "[]") Composite(listOf())
    else if(str.startsWith('[')) Composite(buildList(0) {
        var openBracketCount = 0
        val iter = str.substring(1).iterator()
        var char = iter.next()
        var strComponent = ""
        while (openBracketCount != 0 || char != ']') {
            when (char) {
                '[' -> openBracketCount++
                ']' -> openBracketCount--
                ',' -> if (openBracketCount == 0) {
                    add(strComponent.toComponent())
                    strComponent = ""
                }
            }
            strComponent += char
            char = iter.next()
        }
        add(strComponent.toComponent())
    })
    else Number(str.toIntOrNull()!!)
}