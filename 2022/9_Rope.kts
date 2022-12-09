val input = generateSequence(::readLine)
val commands = input.map {
    val tokens = it.split(' ')
    Pair(tokens[0], tokens[1].toInt())
}

data class Point(var x: Int, var y: Int){
    infix fun diagonalWith(other: Point): Boolean = !(x == other.x || y == other.y)
    fun getCommandFunction(direction: String): (Point) -> Int = when(direction){
            "L" -> { p: Point -> p.x-- }
            "R" -> { p: Point -> p.x++ }
            "D" ->  { p: Point -> p.y-- }
            "U" -> { p: Point -> p.y++ }
            else -> throw Error("Invalid command")
        }
    fun toPair() = Pair(y, x)
}

val visited = mutableSetOf<Pair<Int, Int>>()
var head = Point(0, 0)
var tail = Point(0, 0)
visited.add(tail.toPair())

commands.forEach{
    val move = head.getCommandFunction(it.first)
    repeat(it.second){
        val prevHead = head.copy()
        move(head)
        if((tail diagonalWith prevHead && tail diagonalWith head )
            || (!(tail diagonalWith prevHead) && !(tail diagonalWith head) && (tail != head))){
            tail = prevHead
            visited.add(tail.toPair())
        }
    }
}

//visited.forEach { println(it) }
println(visited.size)
