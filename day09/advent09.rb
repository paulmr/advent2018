class Node
  attr_accessor :number, :clockwise, :counterclockwise

  def initialize(number)
    @number = number
    @clockwise = nil
    @counterclockwise = nil
  end

  def add_marble(num)
    left = clockwise
    right = left.clockwise
    new_marble = Node.new num
    left.clockwise = new_marble
    new_marble.counterclockwise = left
    new_marble.clockwise = right
    right.counterclockwise = new_marble
    new_marble
  end

  def remove(n = 7)
    if n == 0
      puts "removing #{number}"
      counterclockwise.clockwise = clockwise
      clockwise.counterclockwise = counterclockwise
      self
    else
      counterclockwise.remove(n - 1)
    end
  end

  def to_s
    "#{@counterclockwise.number} (#{@number}) #{@clockwise.number}"
  end
end

def print_nodes(root, current)
  res = []
  n = root
  while true
    res << (n == current ? "(#{n.number})" :  "#{n.number}")
    n = n.clockwise
    break if n == root
  end
  puts res.join(" ")
end

def part1(max)
  root = Node.new 0
  root.clockwise = root
  root.counterclockwise = root
  current = root

  (1..max).each { |n|
    if n % 23 == 0
      rem = current.remove
      current = rem.clockwise
    else
      current = current.add_marble n
    end
  }

  print_nodes(root, current)
end

part1 25
