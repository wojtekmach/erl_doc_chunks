defmodule BarTest do
  use ExUnit.Case
  doctest Bar

  test "greets the world" do
    assert Bar.hello() == :world
  end
end
