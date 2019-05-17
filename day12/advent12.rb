require 'set'

class State
  def initialize
    @plants = Set.new
  end

  def has_plant?(pot)
    @plants.member? pot
  end

  def has_plant(pot)
    @plants.add pot
  end

  def seg(pot)
    (pot-2..pot+2).map { |p| if has_plant?(p) then "#" else "." end }.join
  end

  def count
    @plants.sum
  end

  def self.from_s(s)
    res = State.new
    s.chars.each_with_index do |c,i|
      res.has_plant(i) if c == "#"
    end
    res
  end

  def bounds
    ( @plants.min-2..@plants.max+2 )
  end

  def next(rules, count = 1)
    if count == 0
      self
    else
      bounds.each_with_object(State.new) { | pot, state |
        # puts "#{pot} #{seg(pot)} #{rules[seg(pot)]}"
        state.has_plant(pot) if rules[seg(pot)]
      }.next(rules, count-1)
    end
  end

  def to_s
    bounds.map { |pot|
      has_plant?(pot) ? "#" : "."
    }.join
  end
end

def read_file(f)
  lines = File.readlines(f)
  /initial state: (.+)$/.match(lines.first)
  s = State.from_s($1)
  rules = lines.drop(2).map do |l|
    [ l.slice(0, 5), l.slice(9) == "#" ]
  end.to_h
  [ s, rules ]
end

# s, rules = read_file "example.txt"
s, rules = read_file "input12.txt"
puts "Part 1: #{s.next(rules, 20).count}"
puts "Part 2: #{s.next(rules, 50000000000).count}"
