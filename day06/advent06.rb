require 'set'

class Point
  attr_reader :x, :y
  def initialize(x,y)
    @x = x; @y = y
  end
  def dist(other)
    (other.x - @x).abs + (other.y - @y).abs
  end
  def ==(other)
    other.x == @x and other.y == @y
  end

  alias :eql? :==
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

  def closest(p)
    c = @input.each_with_index.map do | o, i |
      [ o.dist(p), i ]
    end.sort_by do | dist, i |
      dist
    end
    if c[0][0] == c[1][0] # same distance
      "."
    else
      (c[0][1] + "a".ord).chr
    end
  end
end

def read(lines)
  lines.map { |l|
    /([0-9]+)\s*,\s*([0-9]+)/.match(l)
    Point.new($1.to_i, $2.to_i)
  }
end

points = read(File.readlines("example.txt"))
a = Advent06.new points

(0..9).each do |y|
  (0..9).each do |x|
    print a.closest(Point.new(x, y))
  end
  print "\n"
end

