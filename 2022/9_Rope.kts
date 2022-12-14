//https://adventofcode.com/2022/day/9
import kotlin.math.sign

val input = generateSequence(::readLine)
val commands = input.map {
    val tokens = it.split(' ')
    Pair(tokens[0], tokens[1].toInt())
}.toList()

println("Part1: ${compute(2)}")
println("Part2: ${compute(10)}")

fun compute(knotCount: Int): Int {
    val lastKnotVisited = mutableSetOf<Point>()
    val knots = List(knotCount){ Point(0, 0) }.toMutableList()
    lastKnotVisited.add(Point(0, 0))
    commands.forEach {
        val move = getCommandFunction(it.first)
        for(x in 0 until it.second) {
            move(knots[0])
            for(i in 1 until  knotCount){
                val head = knots[i - 1].copy()
                val tail = knots[i].copy()

                if(tail adjacentTo head) continue

                knots[i] += tail.getVectorTo(head)
                if(i == knotCount - 1) lastKnotVisited.add(knots[i])
            }
        }
    }
    return lastKnotVisited.size
}

data class Point(var x: Int, var y: Int){
    infix fun adjacentTo(other: Point) = other.x in x-1..x+1 && other.y in y-1..y+1
    fun getVectorTo(other: Point): Point = Point((other.x - x).sign, (other.y - y).sign)
    operator fun plus(other: Point) = Point(x + other.x, y + other.y)
    override fun toString() = "(x=$x, y=$y)"
}

fun getCommandFunction(direction: String): (Point) -> Int = when(direction){
    "L" -> { p: Point -> p.x-- }
    "R" -> { p: Point -> p.x++ }
    "D" ->  { p: Point -> p.y-- }
    "U" -> { p: Point -> p.y++ }
    else -> throw Error("Invalid command")
}






