// https://adventofcode.com/2022/day/12

enum class NodeType { START, END, NORMAL }

data class QueueEntry(val node: Node, val visitedStep: Int)

class Node(char: Char, private val y: Int, private val x: Int){
    val type = when(char) { 'E' -> NodeType.END; 'S' -> NodeType.START; else -> NodeType.NORMAL }
    val height = when(type) { NodeType.END -> 'z'; NodeType.START -> 'a'; else -> char}.code - 'a'.code

    companion object { private val DIRECTIONS = listOf(Pair(0, 1), Pair(1, 0), Pair(0, -1), Pair(-1, 0)) } // static attribute

    fun adjacentNodes() = DIRECTIONS
        .map { (incY, incX) -> Pair(y + incY, x + incX) }
        .filter { (y, x) -> x >= 0 && y >= 0 && y < map.size && x < map[0].size }
        .map { (y, x) -> map[y][x] }
}

val map = generateSequence(::readLine).mapIndexed { y, line -> line.mapIndexed { x, c -> Node(c, y, x) } }.toList()
println("Part1: ${bfs(map.flatten().first { it.type == NodeType.START })}")
println("Part2: ${map.flatten().filter { it.height == 0 }.mapNotNull { bfs(it) }.min()}")

fun bfs(startNode: Node): Int?{
    val visitedNodes = mutableSetOf(startNode)
    val queue: ArrayDeque<QueueEntry> = ArrayDeque<QueueEntry>().apply { add(QueueEntry(startNode, 0)) }
    while(!queue.isEmpty()){
        val entry = queue.removeFirst()
        entry.node.adjacentNodes()
            .filter { it.height - entry.node.height <= 1 }
            .filter { it !in visitedNodes }
            .forEach {
                if(it.type == NodeType.END) return entry.visitedStep + 1 // found the end
                visitedNodes.add(it)
                queue.addLast(QueueEntry(it, entry.visitedStep + 1))
            }
    }
    return null // cant reach the end
}