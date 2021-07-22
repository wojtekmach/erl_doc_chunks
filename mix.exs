defmodule ErlDocChunks.MixProject do
  use Mix.Project

  def project do
    [
      app: :erl_doc_chunks,
      version: "0.1.0",
      elixir: "~> 1.10"
    ]
  end

  def application do
    [
      extra_applications: [:syntax_tools]
    ]
  end
end
