#!/usr/bin/env ruby

require_relative "memoization"

memoize def fib(n)
  n <= 1 ? n : fib(n-1) + fib(n-2)
end

memoize def fact(n)
  n <= 1 ? 1 : n * fact(n-1)
end

n = gets.to_i
n.times.each {|i| [fib(i), fact(i)] }

a = fib n
b = fact n

puts "#{a} [#{a.to_s.chars.map(&:to_i).sum}]"
puts "#{b} [#{b.to_s.chars.map(&:to_i).sum}]"
