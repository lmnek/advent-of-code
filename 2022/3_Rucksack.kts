//https://adventofcode.com/2022/day/3
part2()

fun part1(){
    println(generateSequence { readLine() }.map { line ->
        line.substring(0, line.length / 2).filter { it in line.substring(line.length / 2) }
            .map { it.lowercaseChar() - 'a' + 1 + if(it.isUpperCase()) 26 else 0 }
            .distinct().sum()
    }.sum())
}

fun part2(){
    println(generateSequence { readLine() }.chunked(3).mapNotNull { group ->
        for(char in group[0]){
            if(char in group[1] && char in group[2])
                return@mapNotNull char
        }
        null
    }.map { it.lowercaseChar() - 'a' + 1 + if(it.isUpperCase()) 26 else 0 }.sum())
}

//fun part1(){
    //val lines = generateSequence { readLine() }
    //val score = lines.map { line ->
    //    val index = line.length / 2
    //    val compartment1 = line.substring(0, index)
    //    val compartment2 = line.substring(index)
    //    compartment1.filter { it in compartment2 }
    //        .map { it.lowercaseChar() - 'a' + 1 + if(it.isUpperCase()) 26 else 0 }
    //        .distinct().sum()
    //}.sum()
    //println(score)
//}

