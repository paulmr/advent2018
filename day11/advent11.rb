class Advent
  attr_reader :serial, :max_x, :max_y
  def initialize(serial, max_x = 300, max_y = 300)
    @serial = serial
    @max_x = max_x
    @max_y = max_y
    @image = {}
  end

  def power_of(x, y)
    rack = x + 10
    power = ((rack * y) + serial) * rack
    power.to_s[-3].to_i - 5
  end

  def coords(lim = 0)
    (1..max_y-lim-1).flat_map do |y|
      (1..max_x-lim-1).map { |x| [x,y] }
    end
  end

  def image(x,y)
    @image[[x,y]] ||=
      begin
        if x < 1 or y < 1 or x > max_x or y > max_y
          0
        else
          power_of(x,y) + image(x - 1, y) + image(x, y - 1) - image(x - 1, y - 1)
        end
      end
  end

  def square(x,y,sz)
    a = image(x-1,y-1)
    b = image(x-1,y-1+sz)
    c = image(x-1+sz,y-1)
    d = image(x-1+sz,y-1+sz)
    (d - b - c) + a
  end

  def part1
    coords(3).max_by do |x,y|
      square(x,y,3)
    end
  end

  def part2
    max = nil

    (1..max_x).each do |sz|
      coords(sz).each do |x,y|
        r = [x,y,sz,square(x,y,sz)]
        # p r
        if not max
          max = r
        else
          max = max[3] < r[3] ? r : max
        end
      end
    end
    max
  end
end

# advent = Advent.new(8199)
advent = Advent.new(18)
# puts advent.part1.join(",")
puts advent.part2.join(",")

# puts Advent.new(18).square(90,269,16)

# puts Advent.new(18).image(1,1)

#p Advent.new(18).solve_for(90,269)
# p Advent.new(18).solve2
# p Advent.new(42).solve_for(232,251)
# p Advent.new(18).power_of_sq(33,45,3)
# puts Advent.new(8199).solve2
