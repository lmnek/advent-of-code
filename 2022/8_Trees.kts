//https://adventofcode.com/2022/day/8
import kotlin.math.max

val input = generateSequence(::readLine).toList()
data class Tree (val height: Int, var visible: Boolean)

val array = input.map { it.map { char -> Tree(char.digitToInt(), false) } }
val rowCount = array.size
val colCount = array[0].size

var visibleTreesCounter = 0
part1()
println("Part1 $visibleTreesCounter")

fun part1(){
    for(y in 0 until rowCount){
        scanDirection(y, 0 until colCount, ::getTree)
        scanDirection(y, colCount - 1 downTo  0, ::getTree)
    }
    for(x in 0 until colCount){
        scanDirection(x, 0 until rowCount, ::getTreeReversedIndex)
        scanDirection(x, rowCount - 1 downTo  0, ::getTreeReversedIndex)
    }
}

fun getTree(constIdx: Int, rangeIdx: Int) =  array[constIdx][rangeIdx]
fun getTreeReversedIndex(constIdx: Int, rangeIdx: Int) =  array[rangeIdx][constIdx]

fun scanDirection(constIdx: Int, range: IntProgression, getTree: (Int, Int) -> Tree){
    var maxHeight = -1
    for(rangeIdx in range){
        val tree = getTree(constIdx, rangeIdx)
        if(tree.height > maxHeight){
            maxHeight = tree.height
            if(!tree.visible) {
                tree.visible = true
                visibleTreesCounter++
            }
        }
    }
}

println("Part2 ${part2()}")

fun part2(): Int{
    var maxScore = 0
    for(y in 0 until rowCount){
        for(x in 0 until colCount )
            maxScore = max(maxScore, getViewScore(y, x, array[y][x].height) )
    }
    return maxScore
}

fun getViewScore(y: Int, x: Int, height: Int) =  getDirScore(y, x + 1 until  colCount, ::getTree, height) *
        getDirScore(y, x - 1 downTo 0, ::getTree, height) *
        getDirScore(x, y + 1 until  rowCount, ::getTreeReversedIndex, height) *
        getDirScore(x, y - 1 downTo 0, ::getTreeReversedIndex, height)

fun getDirScore(constIdx: Int, range: IntProgression, getTree: (Int, Int) -> Tree, height: Int): Int{
    var score = 0
    for(i in range){
        score++
        if (getTree(constIdx, i).height >= height) break
    }
    return score
}

// alternative with repeated code
fun getViewScore2(y: Int, x: Int): Int{
    val curHeight = array[y][x].height
    var d1 = 0
    for(i in x + 1 until  colCount){
        d1++
        if (array[y][i].height >= curHeight) break
    }
    var d2 = 0
    for (i in x - 1 downTo 0){
        d2++
        if (array[y][i].height >= curHeight) break
    }
    var d3 = 0
    for(i in y + 1 until  rowCount){
        d3++
        if (array[i][x].height >= curHeight) break
    }
    var d4 = 0
    for (i in y - 1 downTo 0){
        d4++
        if (array[i][x].height >= curHeight) break
    }
    return d1 * d2 * d3 * d4
}

