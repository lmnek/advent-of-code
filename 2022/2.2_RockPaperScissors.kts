enum class Shape(val score: Int) {
    ROCK(1), PAPER(2), SCISSORS(3);

    fun losingShape(): Shape{
        return when(this){
            ROCK -> SCISSORS
            PAPER -> ROCK
            SCISSORS -> PAPER
        }
    }

    fun winningShape(): Shape {
        return when(this){
            ROCK -> PAPER
            PAPER -> SCISSORS
            SCISSORS -> ROCK
        }
    }
}

object Factory{
    fun getShape(a: String): Shape = when(a){
        "A" -> Shape.ROCK
        "B" -> Shape.PAPER
        "C" -> Shape.SCISSORS
        else -> throw Exception()
    }
}

val lines = generateSequence { readLine() }
var score = 0
lines.forEach {
    val (a, me) = it.split(" ")
    val enemy = Factory.getShape(a)
    score += when(me){
        "X" -> 0 + enemy.losingShape().score // lose
        "Y" -> 3 + enemy.score // draw
        "Z" -> 6 + enemy.winningShape().score // win
        else -> throw Error()
    }
}
println(score)
