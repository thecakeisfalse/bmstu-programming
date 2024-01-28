#!/usr/bin/env ruby

require_relative "generate_strings"

if ARGV.size != 2
  puts "usage: #{$0} <number of strings> <string length>"
  exit
end

n = ARGV[0].to_i
length = ARGV[1].to_i

puts generate_strings n, length
