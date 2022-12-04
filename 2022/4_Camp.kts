// https://adventofcode.com/2022/day/4

println(generateSequence { readLine() }.map {
    val (a, b) = it.split(",").map { l ->
        val nums = l.split("-").map { s -> s.toInt() }
        nums[0]..nums[1]
    }
    if(part2(a, b)) 1 else 0
}.sum())

fun part1(a: IntRange, b: IntRange) = (a.first in b && a.last in b) || (b.first in a && b.last in a)

fun part2(a: IntRange, b: IntRange) = a.first in b || a.last in b || b.first in a || b.last in a