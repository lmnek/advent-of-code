// https://adventofcode.com/2022/day/12
import kotlin.system.exitProcess

data class Node(var value: Char, val y: Int, val x: Int){
    val isEnd = value == 'E'
    init { if(isEnd) value = 'z'; if(value == 'S') value = 'a' }
}

data queueEntry(val Node, val visitedStep: Int)

//val map = buildList<List<Node>> { readln().map(::Node).toList() }
var start: Node? = null
val map = generateSequence(::readLine).mapIndexed { y, line -> line.mapIndexed { x, c ->
        val node = Node(c, y, x)
        if(c == 'S') start = node
        node
    }
}.toList()

start!!.visitedStep = 0
val queue: ArrayDeque<Node> = ArrayDeque(0)
queue.addLast(start!!)

while(!queue.isEmpty()){
    val node = queue.removeFirst()
    tryNeighbor(node, 1, 0)
    tryNeighbor(node, 0, 1)
    tryNeighbor(node, -1, 0)
    tryNeighbor(node, 0, -1)
}

fun tryNeighbor(node: Node, incX: Int, incY: Int) {
    val y = node.y + incY
    val x = node.x + incX
    // in map?
    if(x >= 0 && y >= 0 && y < map.size && x < map[0].size){
        val neighbor = map[y][x]
        // not visited and can go up to?
        if(neighbor.visitedStep == -1 && neighbor.value.code - node.value.code <= 1){
            if(neighbor.isEnd){
                println("END: ${node.visitedStep + 1}")
                exitProcess(0)
            }
            neighbor.visitedStep = node.visitedStep + 1
            queue.addLast(neighbor)
        }
    }
}