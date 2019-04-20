require 'set'

class Advent
  def initialize(deps)
    f = proc do | h,k | h[k] = Set.new end
    @h   = Hash.new &f
    @inv = Hash.new &f
    @state = Hash.new
    deps.each do |k,v|
      add_dep k, v
    end
  end

  def add_dep(k, v)
    @h[k].add v
    @state[k] = :init
    @state[v] = :init
    @inv[v].add k
  end

  def resolve(k)
    @state[k] = :complete
  end

  def ready
    @state.select do |k,v|
      v == :init and @h[k].all? do |j|
        @state[j] == :complete
      end
    end.map do |k, v|
      k
    end.sort
  end

end

fname = ARGV.length > 0 ? ARGV[0] : "example.txt"
input = File.readlines fname

@deps = []

input.map do |l|
  if not %r(Step ([A-Z]) must be finished before step ([A-Z]) can begin.).match(l)
    puts "Bad line"
  end
  @deps.push [$2, $1]
end

a = Advent.new @deps

order = []
while a.ready.length > 0
  nxt = a.ready[0]
  order.push nxt
  a.resolve nxt
end

puts order.join

#def part2
  #a = Advent.new @deps
#end

#puts part1
