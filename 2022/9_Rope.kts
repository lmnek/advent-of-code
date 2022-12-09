val input = generateSequence(::readLine)
val commands = input.map {
    val tokens = it.split(' ')
    Pair(tokens[0], tokens[1].toInt())
}

data class Point(var x: Int, var y: Int){
    infix fun diagonalWith(other: Point): Boolean = !(x == other.x || y == other.y)
    fun toPair() = Pair(y, x)
}

fun getCommandFunction(direction: String): (Point) -> Int = when(direction){
    "L" -> { p: Point -> p.x-- }
    "R" -> { p: Point -> p.x++ }
    "D" ->  { p: Point -> p.y-- }
    "U" -> { p: Point -> p.y++ }
    else -> throw Error("Invalid command")
}

//println(part1())

fun part1(): Int {
    val visited = mutableSetOf<Pair<Int, Int>>()
    val head = Point(0, 0)
    var tail = Point(0, 0)
    visited.add(tail.toPair())
    commands.forEach {
        val move = getCommandFunction(it.first)
        repeat(it.second) {
            val prevHead = head.copy()
            move(head)
            if ((tail diagonalWith prevHead && tail diagonalWith head)
                || (!(tail diagonalWith prevHead) && !(tail diagonalWith head) && (tail != head))
            ) {
                tail = prevHead
                visited.add(tail.toPair())
            }
        }
    }
    //visited.forEach { println(it) }
    return visited.size
}


println(part2(10))

fun part2(knotCount: Int): Int {
    val lastKnotVisited = mutableSetOf<Pair<Int, Int>>()
    val knots = List(knotCount){ Point(0, 0) }.toMutableList()
    lastKnotVisited.add(knots[0].toPair())
    commands.forEach {
        val move = getCommandFunction(it.first)
        println(it)
        repeat(it.second) {
            var head = knots[0]
            var prevHead = head.copy()
            move(head)
            for(i in 1 until  knotCount){
                knots.forEach { a -> println("$a ") }
                var tail = knots[i].copy()
                if ((tail diagonalWith prevHead && tail diagonalWith head)
                    || (!(tail diagonalWith prevHead) && !(tail diagonalWith head) && (tail != head))
                ) {
                    tail = prevHead
                    knots[i] = tail
                    if(i == knotCount - 1) lastKnotVisited.add(tail.toPair())
                }
                head = tail
                prevHead = head.copy()
                println()
            }
            println()
        }
    }
    //visited.forEach { println(it) }
    return lastKnotVisited.size
}


