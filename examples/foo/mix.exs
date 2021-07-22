defmodule Foo.MixProject do
  use Mix.Project

  def project do
    [
      app: :foo,
      version: "0.1.0",
      elixir: "~> 1.10",
      deps: deps(),
      compilers: Mix.compilers() ++ [:erl_doc_chunks],
      language: :erlang
    ]
  end

  defp deps do
    [
      {:erl_doc_chunks, path: "../.."}
    ]
  end
end
