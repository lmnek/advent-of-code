// https://adventofcode.com/2022/day/14
import kotlin.math.sign

// parse input
val walls = generateSequence(::readLine).map{ it.split(" -> ").map{ coords ->
    Pos.of(coords.split(",").map { num -> num.toInt() })
}.windowed(2, 1) }.flatten().toList()

println("Part 1: ${Cave(walls).part1()}")
println("Part 2: ${Cave(walls).part2()}")


enum class CellType(val symbol: Char) { EMPTY('.'), WALL('#'), SAND('o') }

class Cave(walls: List<List<Pos>>){
    private val maxWallY = walls.flatten().maxOfOrNull{ it.y }!!
    private val maxWallX = walls.flatten().maxOfOrNull{ it.x }!!

    private val startPos = Pos(0, 500)
    private val grid = List(maxWallY + 3) { MutableList(startPos.x + (2 * maxWallY)) { CellType.EMPTY } }

    // build walls
    init {
        walls.forEach { (wallStart, wallEnd) ->
            val direction = wallStart vectorTo  wallEnd
            var cur = wallStart.copy()
            while(cur != wallEnd) {
                setAt(cur, CellType.WALL)
                cur += direction
            }
            setAt(wallEnd, CellType.WALL)
        }
        for(x in 0 until grid[0].size) grid[grid.lastIndex][x] = CellType.WALL // ground for part2
    }

    private fun at(pos: Pos) = grid[pos.y][pos.x]
    private fun setAt(pos: Pos, celltype: CellType) { grid[pos.y][pos.x] = celltype }

    fun part1() = simulateSand { sandPos: Pos -> sandPos.y == maxWallY + 1 } - 1
    fun part2() = simulateSand { sandPos: Pos -> sandPos == startPos }

    private fun simulateSand(endingCondition: (Pos) -> Boolean): Int{
        val directions = listOf(Pos(1, 0), Pos(1, -1), Pos(1, 1))
        var sandPos = startPos.copy()
        var sandCounter = 0
        loop@ while(true){
            for (dir in directions){
                val newPos = sandPos + dir
                val cell = at(newPos)
                if(cell == CellType.EMPTY){
                    sandPos = newPos
                    continue@loop // move sand
                }
            }
            // cant fall further -> stable sand
            setAt(sandPos, CellType.SAND)
            sandCounter++
            // end simulation?
            if(endingCondition(sandPos))
                return sandCounter
            sandPos = startPos.copy()
        }
    }

    fun printGrid(fromX: Int) {
        grid.forEach { row ->
            for(x in fromX until row.size) print(row[x].symbol)
            println()
        }
        println()
    }
}

data class Pos(val y: Int, val x: Int){
    companion object { fun of(values: List<Int>): Pos = Pos(values[1], values[0])  }
    operator fun plus(other: Pos) = Pos( y + other.y, x + other.x)
    infix fun vectorTo(other: Pos) =  Pos((other.y - y).sign, (other.x - x).sign)
    override fun toString() = "(x=$x, y=$y)"
}