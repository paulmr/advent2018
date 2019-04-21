class Node
  def initialize(children, metas)
    @children = children
    @metas = metas
  end

  def meta_sum
    ch = @children.map do |n|
      n.meta_sum
    end.reduce(0,:+)

    ch + @metas.reduce(0, :+)
  end

  def value
    if @children.length == 0
      meta_sum
    else
      @metas.map do |m|
        if m > 0 and m <= @children.length
          @children[m-1].value
        else
          0
        end
      end.reduce(0, :+)
    end
  end

  def self.read(input)
    num_children = input.shift
    num_metas = input.shift

    children = []
    num_children.times do
      children << Node.read(input)
    end

    metas = []
    num_metas.times do
      metas << input.shift
    end

    Node.new(children, metas)
  end

end

fname = ARGV.length > 0 ? ARGV[0] : "example.txt"
nums = []
input = File.readlines(fname).each do |l|
  l.split.each do |n| nums << n.to_i end
end

n = Node.read(nums)

puts "Part 1: #{n.meta_sum}"
puts "Part 2: #{n.value}"
