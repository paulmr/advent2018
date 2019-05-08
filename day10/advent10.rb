class Point
  attr_reader :x, :y
  def initialize(x, y)
    @x = x; @y = y
  end
  def +(other)
    Point.new(@x + other.x, @y + other.y)
  end
  def self.from_s(s)
    x, y = s.split(/\s*,\s*/)
    Point.new(x.to_i,y.to_i)
  end
end

class Star
  attr_reader :pos, :vel
  def initialize(pos, vel)
    @pos = pos; @vel = vel
  end
  def next
    Star.new(pos + vel, vel)
  end
  def self.from_s(s)
    m = /.*<(.*)>.*<(.*)>/.match(s)
    Star.new(Point.from_s(m[1]), Point.from_s(m[2]))
  end
end

class Stars
  include Enumerable
  attr_reader :size, :dims
  def initialize(stars)
    @stars = stars
    pos = @stars.map(&:pos)
    xs = pos.map(&:x)
    ys = pos.map(&:y)
    @dims = [ Point.new(xs.min, ys.min),
      Point.new(xs.max, ys.max) ]
    min, max = dims
    @size = Point.new(max.x - min.x, max.y - min.y)
  end
  def self.from_lines(lines)
    Stars.new(lines.map { |l| Star.from_s(l) }.to_a)
  end
  def next
    Stars.new(@stars.map(&:next))
  end
  def to_s
    min, max = dims
    (min.y..max.y).map do |y|
      (min.x..max.x).map do |x|
        if @stars.detect { |s| s.pos.x == x and s.pos.y == y } then "#" else " " end
      end.join
    end.join("\n")
  end
end

init_pos = Stars.from_lines(
  File.readlines(ARGV[0] || "example.txt")
)

stars = init_pos.next
last = init_pos
turn = 0
while stars.size.x < last.size.x and stars.size.y < last.size.y
  turn += 1
  last = stars
  stars = stars.next
end

puts last
puts turn
