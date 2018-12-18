defmodule Advent do
  def isEqu(a, b), do: a != b and String.upcase(a) == String.upcase(b)
  def reduce(s), do:
    length(reduce([], s))

  def reduce(stk, []), do: stk
  def reduce([], [h|t]), do: reduce([h], t)
  def reduce([sh|st], [h|t]) do
    if isEqu(sh, h) do
      reduce(st, t)
    else
      reduce([h, sh | st], t)
    end
  end

  def remove(input, c) do
    Enum.filter(input, fn (x) ->
      String.upcase(x) != c
    end)
  end

  def part2(input) do
    uniq = Enum.into(
      Enum.map(input, &String.upcase(&1)),
      %MapSet{}
    )
    Enum.map(uniq, fn (x) ->
      reduce(remove(input, x))
    end) |> Enum.min
  end
end

input = File.read!("input05.txt")|>
  String.trim |> String.codepoints

IO.puts Advent.reduce(input) 
IO.puts Advent.part2(input)
