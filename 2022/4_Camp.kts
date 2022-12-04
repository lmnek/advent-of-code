// https://adventofcode.com/2022/day/4

// refactored
println(generateSequence { readLine() }.count {
    val (a, b) = it.asRanges()
    part2(a, b)
})

fun part1(a: IntRange, b: IntRange) = a in b || b in a
fun part2(a: IntRange, b: IntRange) = a overlaps b

fun String.asRanges() = this.split(',').map { it.split('-').map{num -> num.toInt()}.asRange() }
fun List<Int>.asRange() = this[0]..this[1]

operator fun IntRange.contains(other: IntRange) = other.first >= first && other.last <= last // overload "in"
infix fun IntRange.overlaps(other: IntRange) = first <= other.last && other.first <= last


// old
//println(generateSequence { readLine() }.map {
//    val (a, b) = it.split(",").map { l ->
//        val nums = l.split("-").map { s -> s.toInt() }
//        nums[0]..nums[1]
//    }
//    if(part2(a, b)) 1 else 0
//}.sum())
//
//// interval in the other interval
//fun part1(a: IntRange, b: IntRange) = (a.first in b && a.last in b) || (b.first in a && b.last in a)
//
//// overlap
//fun part2(a: IntRange, b: IntRange) = a.first in b || a.last in b || b.first in a || b.last in a