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

  def start(k)
    @state[k] = :in_progress
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

  def complete
    @state.all? do |k,v| v == :complete end
  end

end

fname = ARGV.length > 0 ? ARGV[0] : "example.txt"
input = File.readlines fname

deps = []

input.map do |l|
  if not %r(Step ([A-Z]) must be finished before step ([A-Z]) can begin.).match(l)
    puts "Bad line"
  end
  deps.push [$2, $1]
end

def part1(deps)
  a = Advent.new deps

  order = []
  while a.ready.length > 0
    nxt = a.ready[0]
    order.push nxt
    a.resolve nxt
  end

  order.join
end

def part2(deps)
  a = Advent.new deps
  events = []
  workers = 5
  time = 0
  offset = 60

  while not a.complete
    # first assign any ready workers to work
    while (a.ready.length > 0 and workers > 0)
      workers -= 1
      next_work = a.ready[0]
      end_time = (next_work.ord - ('A'.ord - 1)) + time + offset
      events.push [ next_work, end_time ]
      a.start next_work
    end

    events.sort_by! do | k, v | v end
    k, time = events.shift
    a.resolve(k)
    workers += 1
  end
  time
end

puts "Part 1: #{part1 deps}"
puts "Part 2: #{part2 deps}"
