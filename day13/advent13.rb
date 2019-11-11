Cart = Struct.new(:x, :y, :dir, :turn)
Corner = Struct.new(:x, :y, :c)

Dirs = "<v>^"

class Cart
  attr_accessor :x, :y, :dir, :turn

  def initialize(x, y, dir)
    @x = x
    @y = y
    @dir = dir
    @turn = 0
  end

  def left
    @dir = (@dir + 1) % 4
  end
  def right
    if @dir == 0 then @dir = 3 else @dir -= 1 end
  end
end

def read_file(fname)
  carts = []
  corners = []

  #  x = y = 0
  File.open fname do |f|
    f.each_line
      .with_index do |l, y|
      l.each_char.with_index do |c, x|
        case c
        when "/", "\\", "+"
          corners << Corner.new(x, y, c)
        when "<", ">", "^", "v"
          carts << Cart.new(x, y, Dirs.index(c))
        end
      end
    end
  end
  [ carts, corners ]
end

carts, corners = read_file(ARGV[0] || "example.txt")

firstCrash = nil

while carts.length > 1
  carts.sort_by { |c| [ c.x, c.y ] }.each do |c|
    crn = corners.find { |crn| crn.x == c.x and crn.y == c.y }
    if crn
      case crn.c
      when "/"
        if c.dir.odd? then c.right else c.left end
      when "\\"
        if c.dir.odd? then c.left else c.right end
      when "+"
        case c.turn
        when 0
          c.left
        when 2
          c.right
        end
        c.turn = (c.turn + 1) % 3
      end
    end

    case c.dir
    when 3
      c.y -= 1
    when 1
      c.y += 1
    when 0
      c.x -= 1
    when 2
      c.x += 1
    end

    if carts.find { |d| (not d.equal?(c)) and d.x == c.x and d.y == c.y }
      firstCrash ||= [ c.x, c.y ]
      carts.delete_if { |d| c.x == d.x and c.y == d.y }
    end
  end
end

puts "Part 1: #{firstCrash[0]},#{firstCrash[1]}"
puts "Part 2: #{carts[0].x},#{carts[0].y}"
