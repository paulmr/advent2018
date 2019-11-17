class StateEnum
  include Enumerable
  def initialize(st, init)
    @i = init
    @st = st
  end

  def each
    loop do
      while @i < @st.nums.length do
        @i += 1
        yield @st.nums[@i-1]
      end
      @st.tick
    end
  end
end

class State
  attr_reader :elves, :nums
  def initialize(nums = [3, 7], elves = [0, 1])
    @nums = nums
    @e1, @e2 = elves
  end

  def enum(from = 0)
    StateEnum.new(self, from)
  end

  def make_recipes
    sum = nums[@e1] + nums[@e2]
    nxt = []
    d1 = sum % 10
    sum /= 10
    nums << sum if sum > 0
    nums << d1
  end

  def move_elves
    @e1 = (nums[@e1] + 1 + @e1) % nums.length
    @e2 = (nums[@e2] + 1 + @e2) % nums.length
  end

  def tick
    make_recipes
    move_elves
  end

  def search(patt)
    pi = 0
    enum.each_with_index do |n,i|
      if n == patt[pi]
        return i - pi if pi == patt.length-1
        pi += 1
      else
        pi = 0
      end
    end
  end

  def calc(n)
    e = enum
    e.take n
    (e.take 10).join
  end
end

s = State.new

puts "Part 1: #{s.calc(503761)}"
puts "Part 2: #{s.search([5,0,3,7,6,1])}"
