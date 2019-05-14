class Advent
  attr_reader :serial, :grid
  def initialize(serial, max_x = 300, max_y = 300)
    @serial = serial
    @max_x = max_x
    @max_y = max_y
    @memo  = {}
  end

  def power_of(x, y)
    rack = x + 10
    power = ((rack * y) + serial) * rack
    power.to_s[-3].to_i - 5
  end

  def power_of_sq(x, y, sz)
    @memo[[x,y,sz]] ||=
      begin
        puts [x,y,sz].join(",")
        if sz == 1 then power_of(x,y)
        else
          row = (x..x-1+sz).map do |x|
            # puts "#{x},#{y-1+sz} = #{power_of(x,y-1+sz)}"
            power_of(x,y-1+sz)
          end.sum
          col = (y..y-2+sz).map do |y|
            # puts "#{x-1+sz},#{y} = #{power_of(x-1+sz,y)}"
            power_of(x-1+sz,y)
          end.sum
          # puts "======"
          row + col + power_of_sq(x,y,sz-1)
        end
      end
  end

  def solve1(sz=3)
    (1..@max_y-sz-1).flat_map do |y|
      (1..@max_x-sz-1).map do |x|
        [ x, y, power_of_sq(x,y,sz) ]
      end
    end.max_by { |a| a[2] }
  end

  def solve_for(x,y)
    lim = [ @max_x + 1 - x, @max_y + 1 - y ].min
    # puts "x: #{x}, y: #{y}, lim: #{lim}"
    sz = (1..lim).max_by { |sz| power_of_sq(x,y,sz) }
    [ x, y, sz, power_of_sq(x,y,sz) ]
  end

  def solve2
    (1..@max_y).flat_map do |y|
      (1..@max_x).map do |x|
        solve_for(x,y)
      end
    end.max_by { |a| a[3] }
  end
end

#puts Advent.new(8199).solve1
#p Advent.new(18).solve_for(90,269)
p Advent.new(18).solve2
# p Advent.new(42).solve_for(232,251)
# p Advent.new(18).power_of_sq(33,45,3)
# puts Advent.new(8199).solve2
