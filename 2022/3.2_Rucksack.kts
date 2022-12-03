
println(generateSequence { readLine() }.chunked(3).mapNotNull { group ->
    for(char in group[0]){
        if(char in group[1] && char in group[2])
            return@mapNotNull char
    }
    null
}.map { it.lowercaseChar() - 'a' + 1 + if(it.isUpperCase()) 26 else 0 }.sum())

