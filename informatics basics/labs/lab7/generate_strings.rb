
def generate_strings(n, length)
  if n <= 0
    STDERR.puts "error: invalid number of strings (expected positive number, got: #{n})"
    exit -1
  end

  if length <= 0
    STDERR.puts "error: invalid string length (expected positive number, got: #{length})"
    exit -1
  end

  @printable ||= (?!..?~).reduce(:+)
  n.times.map{ length.times.map{ @printable[rand(@printable.size)] }.join }
end
