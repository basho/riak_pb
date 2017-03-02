defmodule RiakPb.Mixfile do
  use Mix.Project

  @version File.read!("VERSION") |> String.strip

  def project do
    [app: :riak_pb,
     version: @version,
     description: "Riak Protocol Buffers Messages",
     package: package(),
     deps: deps(),
     erlc_options: erlc_options()]
  end

  defp erlc_options do
    extra_options = try do
      case :erlang.list_to_integer(:erlang.system_info(:otp_release)) do
        v when v >= 17 ->
          [{:d, :namespaced_types}]
        _ ->
          []
      end
    catch
      _ ->
        []
    end
    [:debug_info, :warnings_as_errors | extra_options]
  end

  defp deps do
    [
      {:hamcrest, "~> 0.4.1", hex: :basho_hamcrest},
      {:ex_doc, ">= 0.0.0", only: :dev}
    ]
  end

  defp package do
    [files: ~w(mix.exs src plugins include rebar.config tools.mk README.md VERSION LICENSE Makefile),
     maintainers: ["Drew Kerrigan, Luke Bakken, Alex Moore"],
     licenses: ["Apache 2.0"],
     links: %{"GitHub" => "https://github.com/basho/riak_pb"}]
  end
end
