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

def solve(max, players)
  scores = [0] * players
  root = Node.new 0
  root.clockwise = root
  root.counterclockwise = root
  current = root
  player = 0

  (1..max).each { |n|
    if n % 23 == 0
      rem = current.remove
      scores[player] += (n + rem.number)
      current = rem.clockwise
    else
      current = current.add_marble n
    end
    player = (player + 1) % players
  }
  scores.sort[-1]
end

puts "Part 1: #{solve 71307, 458}"
puts "Part 2: #{solve 71307 * 100, 458}"
