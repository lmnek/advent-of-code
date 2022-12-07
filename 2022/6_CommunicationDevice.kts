//https://adventofcode.com/2022/day/6#part2
val bufferLength = 14

val buffer = arrayOfNulls<Char>(bufferLength)
var counter = 0
for(char in readLine()!!){
    buffer[counter % bufferLength] = char
    if(counter >= bufferLength - 1 && buffer.set().size == bufferLength)
        break
    counter+=1
}
println(counter + 1)

