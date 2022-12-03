
println(generateSequence { readLine() }.map { line ->
    line.substring(0, line.length / 2).filter { it in line.substring(line.length / 2) }
        .map { it.lowercaseChar() - 'a' + 1 + if(it.isUpperCase()) 26 else 0 }
        .distinct().sum()
}.sum())

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