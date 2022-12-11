val input = generateSequence(::readLine)
val commands = input.map {
    val tokens = it.split(' ')
    Pair(tokens[0], tokens[1].toInt())
}

data class Point(var x: Int, var y: Int){
    infix fun diagonalWith(other: Point) = !(x == other.x || y == other.y)
//    infix fun movedDiagonal(other: Point) = x != other.x && y != other.y
//    fun getDistance(other: Point) = abs(x - other.x) + abs(y - other.y)
    fun toPair() = Pair(y, x)
    override fun toString() = "(x=$x, y=$y)"

}

fun getCommandFunction(direction: String): (Point) -> Int = when(direction){
    "L" -> { p: Point -> p.x-- }
    "R" -> { p: Point -> p.x++ }
    "D" ->  { p: Point -> p.y-- }
    "U" -> { p: Point -> p.y++ }
    else -> throw Error("Invalid command")
}

println(part1())

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


//println(part2(10))
//
//fun part2(knotCount: Int): Int {
//    val lastKnotVisited = mutableSetOf<Pair<Int, Int>>()
//    val knots = List(knotCount){ Point(0, 0) }.toMutableList()
//    lastKnotVisited.add(knots[0].toPair())
//    commands.forEach {
//        val move = getCommandFunction(it.first)
//        println(it)
//        repeat(it.second) {
//            var head = knots[0]
//            var prevHead = head.copy()
//            move(head)
//            for(i in 1 until  knotCount){
//                val tail = knots[i].copy()
//                val tmp = tail.copy()
//                if (prevHead != head) {
//                    if ( !(prevHead movedDiagonal head) && ((tail diagonalWith prevHead && tail diagonalWith head)
//                    || (!(tail diagonalWith prevHead) && !(tail diagonalWith head) && (tail != head)))) {
//                        knots[i] = prevHead.copy()
//                    } else if(tail != prevHead && prevHead movedDiagonal head && (tail.getDistance(prevHead) != tail.getDistance(head))){
//                        // do the same motion as (prevHead -> head) but with tail
//                        knots[i] = Point(tail.x + (head.x - prevHead.x), tail.y + (head.y - prevHead.y))
//                    }
//                    if(i == knotCount - 1) lastKnotVisited.add(knots[i].toPair())
//                }
//                prevHead = tmp
//                head = knots[i].copy()
//            }
//            printRope(knots)
//        }
//    }
//    return lastKnotVisited.size
//}
//
//fun printRope(knots: MutableList<Point>){
//    val rowCount = 20
//    val colCount = 20
//    val array: List<MutableList<Int?>> = List(rowCount) { MutableList(colCount) { null } }
//    knots.forEachIndexed { i, it -> array[rowCount - it.y - 5][it.x + 9] = i }
//    array.forEach{
//        it.forEach{ it2->
//            print(it2 ?: ".")
//        }
//        println()
//    }
//    println("----------")
//}


