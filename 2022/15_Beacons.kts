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
// https://www.reddit.com/r/adventofcode/comments/zmcn64/2022_day_15_solutions/?sort=confidence
fun part2(): Long {
    val bounds = 0..20
    // lines: y = x + a, y = -x + b
    val aCoefficients = mutableListOf<Int>(); val bCoefficients = mutableListOf<Int>()
    positions.forEach { (sensor, beacon) ->
        val d = manhattanDistance(sensor, beacon)
        aCoefficients.add(sensor.x - sensor.y - d - 1); aCoefficients.add(sensor.x - sensor.y + d + 1)
        bCoefficients.add(sensor.x + sensor.y - d - 1); bCoefficients.add(sensor.x + sensor.y + d + 1)
    }
//    aCoefficients.forEach { a -> bCoefficients
//        .map { b -> Pos((b - a) / 2, (a + b) / 2) }
//        .filter { pos ->  pos.x in bounds && pos.y in bounds }
//        .filter { pos -> positions.all { (sensor, beacon) -> manhattanDistance(sensor, pos) > manhattanDistance(sensor, beacon) } }
//        .forEach { println(it) }
//    }

    val beaconPos = aCoefficients.flatMap { a -> bCoefficients
        .map { b -> Pos((b - a) / 2, (a + b) / 2) }
        .filter { pos ->  pos.x in bounds && pos.y in bounds }
        .filter { pos -> positions.all { (sensor, beacon) -> manhattanDistance(sensor, pos) > manhattanDistance(sensor, beacon) } }
        .distinct()
    }.single()

//    println("${beaconPos.x}, ${beaconPos.y}")
    return (4_000_000L * beaconPos.x) + beaconPos.y
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