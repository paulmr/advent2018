require 'minitest/autorun'

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
        if sz == 1 then power_of(x,y)
        else
          row = (x..x-1+sz).map { |x| power_of(x,y) }.sum
          col = (y+1..y-1+sz).map { |y| power_of(x,y) }.sum
          row + col + power_of_sq(x+1,y+1,sz-1)
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

  def solve2
    @max_x.downto(1).map do |sz|
      puts "Size: #{sz}"
      x,y,p = solve1(sz)
      [ x,y,sz,p ]
    end.may_by { |a| a[3] }
  end
end

class AdventTest < Minitest::Test
  [ [ 3, 5, 8, 4 ],
    [ 122, 79, 57, -5 ],
    [ 217, 196, 39, 0 ],
    [ 101, 153, 71, 4 ]
  ].each do |x,y,serial,exp|
    define_method("test_example_#{x}_#{y}") do
      assert_equal exp, Advent.new(serial).power_of(x,y)
    end
  end

  def test_square
    assert_equal 29, Advent.new(18).power_of_sq(33,45, 3)
  end

  def test_solve
    skip
    assert_equal [33, 45, 29], Advent.new(18).solve
    assert_equal [21, 61, 30], Advent.new(42).solve
  end
end

puts Advent.new(8199).solve1
puts Advent.new(8199).solve2
