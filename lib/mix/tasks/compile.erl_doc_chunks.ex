defmodule Mix.Tasks.Compile.ErlDocChunks do
  use Mix.Task.Compiler

  @impl true
  def run(args) do
    {opts, _args} = OptionParser.parse!(args, switches: [force: :boolean])

    app = Mix.Project.config()[:app]
    manifest = Path.join(Mix.Project.manifest_path(), "compile.erl_doc_chunks")
    dest = Application.app_dir(app, Path.join("doc", "chunks"))

    Mix.Compilers.Erlang.compile(
      manifest,
      [{"src", dest}],
      :erl,
      :chunk,
      opts,
      fn input, output ->
        IO.puts("mix compile.erl_doc_chunks: writing " <> Path.relative_to(output, File.cwd!()))
        chunk = :erl_doc_chunks.file(String.to_charlist(input))
        File.mkdir_p!(Path.dirname(output))
        File.write!(output, :erlang.term_to_binary(chunk))
        contents = []
        warnings = []
        {:ok, contents, warnings}
      end
    )
  end
end
