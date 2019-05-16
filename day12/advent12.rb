class State
  def initialize
    @pots_left  = Array.new
    @pots_right = Array.new
  end

  def has_plant?(pot)
    (pot < 0 ? @pots_left[pot.abs] : @pots_right[pot]) == true
  end

  def has_plant(pot)
    if(pot < 0)
      @pots_left[pot.abs] = true
    else
      @pots_right[pot] = true
    end
  end

  def to_s
    (@pots_left.reverse + @pots_right).map { |p| p ? "#" : "." }.join()
  end
end

s = State.new
s.has_plant(100)
puts s
