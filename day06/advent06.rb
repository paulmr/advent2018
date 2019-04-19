require 'set'

class Point
  attr_reader :x, :y
  def initialize(x,y)
    @x = x; @y = y
  end
  def dist(other)
    (other.x - @x).abs + (other.y - @y).abs
  end

  def eql?(other)
    other.is_a?(Point) and other.x == @x and other.y == @y
  end

  def hash
    [@x, @y].hash
  end

  def to_s
    "(#{x}, #{y})"
  end

  def ns
    [ Point.new(x, y-1),
      Point.new(x, y+1),
      Point.new(x-1, y),
      Point.new(x+1, y) ]
  end
end

class Advent06
  def initialize(input)
    @input = input
  end

  def infinite(from, to)
    @input.all? do | p |
      to.dist(p) > from.dist(p)
    end
  end

  def sumdist(p)
    @input.map do |n| n.dist(p) end.sum
  end

  def closest(p)
    c = @input.each_with_index.map do | o, i |
      [ o.dist(p), i ]
    end.sort_by do | dist, i |
      dist
    end
    if c[0][0] == c[1][0] # same distance
      -1
    else
      c[0][1]
    end
  end

  def sizeOf(measure, pred, start)
    seen = Set.new
    stack = [start]
    size = 0
    while stack.length > 0
      p = stack.pop
      if seen.include?(p) then next end
      seen << p
      score = measure.call(p)
      if score == -1 then next end
      ns = p.ns.select &pred
      if ns.any? do |n| infinite(p, n) end
        return -1
      end
      size += 1
      stack += ns
    end
    size
  end
end

def read(lines)
  lines.map { |l|
    /([0-9]+)\s*,\s*([0-9]+)/.match(l)
    Point.new($1.to_i, $2.to_i)
  }
end

fname = ARGV.length > 0 ? ARGV[0] : "example.txt"

input = read(File.readlines fname)
a = Advent06.new(input)

byx = input.map do |p| p.x end.sort
byy = input.map do |p| p.y end.sort

mid = Point.new(
  (byx[0] + byx[byx.length - 1]) / 2,
  (byy[0] + byy[byy.length - 1]) / 2)

max = 10000

part1 = input.map do |p|
  cl = a.closest(p)
  a.sizeOf(a.method(:closest),
           -> n { a.closest(n) == cl }, p)
end.sort.reverse[0]
puts "part 1: #{part1}"

part2 = a.sizeOf(a.method(:sumdist),
                 -> n { a.sumdist(n) < max }, mid)
puts "Part 2: #{part2}"
