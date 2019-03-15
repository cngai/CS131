// accepts list L and positive integer N and returns list containing every
// Nth element of L, starting with (N-1)st element, then (2N-1)st element
fun <T> everyNth(l: List<T>, n: Int): List<T> {
	// make sure valid input
	if (l.size < n) {
		val emptyList = listOf<T>()
		return emptyList
	}

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
	// empty list
	println("Test 1")
	val list_1 = listOf<Int>()
	println(everyNth(list_1, 2))
	println()

	// list of strings, should print out [hi, bye]
	println("Test 2")
	val list_2 = listOf("1", "2", "hi", "4", "5", "bye", "7")
	println(everyNth(list_2, 3))
	println()

	// list of ints, should print out [2, 4, 6, 8]
	println("Test 3")
	val list_3 = listOf(1, 2, 3, 4, 5, 6, 7, 8)
	println(everyNth(list_3, 2))
	println()
}
