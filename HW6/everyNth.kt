// accepts list L and positive integer N and returns list containing every
// Nth element of L, starting with (N-1)st element, then (2N-1)st element
fun <T> everyNth(l: List<T>, n: Int): MutableList<T> {
	val myList: MutableList<T> = mutableListOf()
	var length = l.size - 1
	for (i in 0..length) {
		if ((i + 1) % n == 0) {
			myList.add(l[i])
		}
	}

	return myList
}

fun main() {
	val list = listOf("1", "2", "hi", "4", "5", "bye", "7")
	var answer = everyNth(list, 3)
	println(answer)
}
