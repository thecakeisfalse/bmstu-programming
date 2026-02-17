package main

import "fmt"

func f() {
	const name_ = "main"
	fmt.Println(name_)
}

func g(s int) {
	fmt.Println(s)
}

const test_ = 42

func main() {
	const name_ = "hello"
	{
		const n = name_
		var m = name_
		fmt.Println(n)
		fmt.Println(m)
	}
	fmt.Println(test_)
}
