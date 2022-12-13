// https://adventofcode.com/2022/day/13
import kotlin.math.min

// composite design pattern + comparable interface
sealed interface Component : Comparable<Component>{
    override fun compareTo(other: Component): Int
}

class Composite(private val components: List<Component>) : Component {
    override fun compareTo(other: Component): Int = when(other) {
        is Number -> this.compareTo(Composite(listOf(other)))
        is Composite -> {
            var res = 0
            for(i in 0 until min(this.components.size, other.components.size)){
                res = this.components[i].compareTo(other.components[i])
                if(res != 0) break
            }
            res
        }
        else -> 0
    }
}

class Number(private val value: Int) : Component {
    override fun compareTo(other: Component): Int = when(other) {
        is Number -> value.compareTo(other.value)
        is Composite -> Composite(listOf(this)).compareTo(other)
        else -> 0
    }
}

fun String.toComponent(): Component {
    return if (startsWith('[')) {
        println(substringBeforeLast(']').substringAfter('[').split(','))
        Composite(substringBeforeLast(']').substringAfter('[')
            .split(',')
            .map { it.trim().toComponent() })
    }
    else {
        println(this)
        Number(toIntOrNull()!!)
    }
}

val pairs: List<List<Component>> = generateSequence(::readLine)
    .filter { it.isNotEmpty() }
    .map{ it.toComponent() }
    .chunked(2).toList()

println("AAA")

val result = 0//pairs.filter { (o1, o2) -> o1 < o2 }.mapIndexed{ i, _ -> i + 1 }
println("Part1: $result")