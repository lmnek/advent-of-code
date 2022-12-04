//https://adventofcode.com/2022/day/2
enum class Shape{
    ROCK, PAPER, SCISSORS
}

object Factory{
    fun getShape(a: String): Shape?{
        if(a == "A" || a == "X")
            return Shape.ROCK
        if(a == "B" || a == "Y")
            return Shape.PAPER
        if(a == "C" || a == "Z")
            return Shape.SCISSORS
        return null
    }
}

val lines = generateSequence { readLine() }
var score = 0
lines.forEach {
    val (enemy, me) = it.split(" ").asSequence().map { itt -> Factory.getShape(itt) }.toList()
    score += when(me){
        Shape.ROCK -> 1
        Shape.PAPER -> 2
        Shape.SCISSORS -> 3
        else -> throw Error()
    }

    score += when (enemy) {
        me -> 3 // draw
        Shape.ROCK -> if (me == Shape.PAPER) 6 else 0
        Shape.SCISSORS -> if (me == Shape.ROCK) 6 else 0
        Shape.PAPER -> if (me == Shape.SCISSORS) 6 else 0
        null -> throw Error()
    }
}
println(score)


