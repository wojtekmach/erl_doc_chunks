# `erl_doc_chunks`

`erl_doc_chunks` is a Mix compiler that generates docs chunks for Erlang files.

This project is a reminder that Mix works really well for building Erlang codebases too.
I'm hoping things shown hear will come to Rebar & EUnit soon too!

## Usage

See [`examples/foo`](examples/foo) project which is an Erlang project built with Mix. Also see
[`examples/bar`](examples/bar) which is an Elixir project that uses `foo`.

1. Create a Mix project for your Erlang codebase and install `erl_doc_chunks`:

   ```bash
   $ mix new foo
   ```

   It is important that the compiler is added last, it needs to run after the Erlang compiler.

   ```elixir
   # mix.exs
   defmodule Foo.MixProject do
     use Mix.Project

     def project do
       [
         app: :foo,
         version: "0.1.0",
         elixir: "~> 1.10",
         deps: deps(),
         compilers: Mix.compilers() ++ [:erl_doc_chunks]
       ]
     end

     defp deps do
       [
         {:erl_doc_chunks, github: "wojtekmach/erl_doc_chunks"}
       ]
     end
   end
   ```

2. Document your Erlang file with Markdown:

   ```erlang
   %% @doc
   %% The foo module.
   -module(foo).
   -export([hello/0, bar/0]).

   %% @doc
   %% Returns a hello.
   %%
   %% ## Examples
   %%
   %%     iex> :foo.hello()
   %%     :world2
   hello() ->
     world.

   bar() ->
     bar.
   ```

   The choice of Markdown is totally arbitrary, any other documentation format would work.

3. You can now access the docs from the IEx shell:

   ```bash
   $ iex -S mix
   iex(1)> h :foo

                                         :foo

   The foo module.
   ```

   As well as from the Erlang shell:

   ```bash
   $ erl -pa _build/dev/lib/foo/ebin
   1> h(foo).

      foo

       The foo module.

   ok
   ```

4. You can write an ExUnit test case and use the doctest feature:

   ```elixir
   # test/foo_test.exs
   defmodule :foo_test do
     use ExUnit.Case, async: true
     doctest :foo
   end
   ```

   ```bash
   $ mix test
     1) doctest :foo.hello/0 (1) (:foo_test)
        test/foo_test.exs:3
        Doctest failed
        doctest:
          iex> :foo.hello()
          :world2
        code:  :foo.hello() === :world2
        left:  :world
        right: :world2
        stacktrace:
          src/foo.erl:11: :foo (module)



   Finished in 0.02 seconds (0.02s async, 0.00s sync)
   1 doctest, 1 failure
   ```

   Doctest support requires a [tiny change to the Elixir](https://github.com/elixir-lang/elixir/pull/11134).

## License

Copyright (c) 2021 Wojtek Mach

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
