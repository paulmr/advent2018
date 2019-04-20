require 'set'

class Advent
  def initialize
    f = proc do | h,k | h[k] = Set.new end
    @h   = Hash.new &f
    @inv = Hash.new &f
  end

  def add_dep(k, v)
    @h[k].add v
    @h[v]
    @inv[v].add k
  end

  def resolve(k)
    # puts "resolving #{k}"
    @inv[k].each do |j|
      # puts "\tremoving from #{j}"
      @h[j].delete k
    end
    @h.delete k
  end

  def ready
    @h.select do |k,v|
      v.length == 0
    end.map do |k, v|
      k
    end.sort
  end

end

a = Advent.new

fname = ARGV.length > 0 ? ARGV[0] : "example.txt"
input = File.readlines fname

input.map do |l|
  if not %r(Step ([A-Z]) must be finished before step ([A-Z]) can begin.).match(l)
    puts "Bad line"
  end
  a.add_dep $2, $1
end

order = []
while a.ready.length > 0
  nxt = a.ready[0]
  order.push nxt
  a.resolve nxt
end

puts order.join
