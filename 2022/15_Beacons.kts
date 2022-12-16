// https://adventofcode.com/2022/day/15
import kotlin.math.abs

val positions = generateSequence(::readLine).map { it.toSensor() to it.toBeacon() }.toSet()

println("Part1 ${part1()}")
println("Part2 ${part2()}")

fun part1(result_y: Int = 2_000_000) = positions.mapNotNull { (sensor, beacon) ->
    val yDiff = manhattanDistance(sensor, beacon) - abs(result_y - sensor.y)
    if(yDiff  >= 0) (sensor.x - yDiff)..(sensor.x + yDiff) else null
}.merge().sumOf { it.last - it.first }

// idea from https://www.youtube.com/watch?v=w7m48_uCvWI
fun part2(): Int {
    // intersection of the two lines is the distress beacon
    linesBetween { sensor: Pos -> sensor.x - sensor.y }.distinct().forEach { posLine ->
        linesBetween { sensor: Pos -> sensor.x + sensor.y }.distinct().forEach { negLine ->
            val x = (posLine + negLine) / 2
            val y = (negLine - posLine) / 2
            println("$x, $y")
            return (4000000 * x) + y
        }
    }
    throw Error()
}

fun linesBetween(getLine: (Pos) -> Int) = sequence {
    val down = mutableListOf<Int>(); val up = mutableListOf<Int>()
    positions.forEach { (sensor, beacon) ->
        val d = manhattanDistance(sensor, beacon)
        down.add(getLine(sensor) - d); up.add(getLine(sensor) + d)
    }

    down.forEachIndexed { i, d -> up.forEachIndexed { j, u ->
        if(i != j && abs(d - u) == 2)
            yield(d + 1)
    } }
}

fun manhattanDistance(a: Pos, b: Pos) = abs(a.x - b.x) + abs(a.y - b.y)

// props to: https://todd.ginsberg.com/post/advent-of-code/2022/day15/
fun List<IntRange>.merge(): List<IntRange>  = if(this.size <= 1) this
else {
    val sorted = this.sortedBy { it.first }
    sorted.drop(1).fold(mutableListOf(sorted.first())) { reduced, range ->
        val lastRange = reduced.last()
        if (range.first <= lastRange.last) reduced[reduced.lastIndex] = (lastRange.first..maxOf(lastRange.last, range.last))
        else reduced.add(range)
        reduced
    }
}

fun String.toBeacon() = Pos(substringAfterLast("x=").substringBefore(",").toInt(), substringAfterLast("y=").trim().toInt())
fun String.toSensor() = Pos(substringAfter("x=").substringBefore(",").toInt(), substringAfter("y=").substringBefore(":").toInt())

data class Pos(val x: Int, val y: Int)