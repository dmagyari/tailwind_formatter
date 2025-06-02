defmodule TailwindFormatter do
  @external_resource "README.md"
  @moduledoc "README.md"
             |> File.read!()
             |> String.split("<!-- MDOC !-->")
             |> Enum.fetch!(1)

  alias TailwindFormatter.{Order, HEExTokenizer}

  @behaviour Mix.Tasks.Format

  @placeholder "💧"

  def features(_opts) do
    [sigils: [:H], extensions: [".heex"]]
  end

  @default_line_length 98
  # Module.get_attribute(Phoenix.LiveView.HTMLFormatter, :default_line_length)

  def format(contents, opts) do
    max_line_length =
      opts[:heex_line_length] || opts[:line_length] || @default_line_length

    contents
    |> HEExTokenizer.tokenize()
    |> Enum.reduce([contents], fn
      {elt, _name, attrs, elt_meta}, contents
      when elt in [:tag, :local_component, :remote_component] ->
        Enum.reduce(attrs, contents, fn
          {"class", class_attr, meta}, [remainder | acc] ->
            [attr, remainder] = String.split(remainder, old_classes(class_attr), parts: 2)

            indent =
              if elt_meta.line === meta.line do
                "\n" <> String.duplicate(" ", elt_meta.column + 1)
              else
                "\n" <> String.duplicate(" ", meta.column + 1)
              end

            [remainder, sort_classes(class_attr, {indent, max_line_length}), attr | acc]

          _, contents ->
            contents
        end)

      _, contents ->
        contents
    end)
    |> Enum.reverse()
    |> Enum.join()
  end

  defp old_classes({_type, classes, _meta}), do: classes
  defp sort_classes({:string, classes, _meta}, indent), do: sort(classes, indent)

  defp sort_classes({:expr, expr_class, _meta}, indent) do
    expr_class
    |> Code.string_to_quoted!(literal_encoder: &{:ok, {:__block__, &2, [&1]}})
    |> sort_expr(indent)
    |> Code.quoted_to_algebra()
    |> Inspect.Algebra.format(:infinity)
    |> IO.iodata_to_binary()
  end

  defp sort_expr({:<<>>, meta, children}, indent),
    do: {:<<>>, meta, handle_interpolation(children, indent)}

  defp sort_expr({a, b, c}, indent),
    do: {sort_expr(a, indent), sort_expr(b, indent), sort_expr(c, indent)}

  defp sort_expr({a, b}, indent), do: {sort_expr(a, indent), sort_expr(b, indent)}
  defp sort_expr(list, indent) when is_list(list), do: Enum.map(list, &sort_expr(&1, indent))
  defp sort_expr(text, indent) when is_binary(text), do: sort(text, indent)
  defp sort_expr(node, _indent), do: node

  defp handle_interpolation(children, indent) do
    {classes_with_placeholders, {placeholder_map, _index}} =
      Enum.map_reduce(children, {%{}, 0}, fn
        str, acc when is_binary(str) ->
          {str, acc}

        node, {placeholder_map, index} ->
          {"#{@placeholder}#{index}#{@placeholder}",
           {Map.put(placeholder_map, "#{index}", sort_expr(node, indent)), index + 1}}
      end)

    classes_with_placeholders
    |> Enum.reduce("", fn class, acc ->
      if placeholder?(class) or String.starts_with?(class, "-"),
        do: acc <> class,
        else: "#{acc} #{class}"
    end)
    |> sort(indent)
    |> String.split()
    |> weave_in_code(placeholder_map)
  end

  defp weave_in_code(classes, placeholder_map) do
    classes
    |> Enum.map(fn class ->
      if placeholder?(class) do
        [prefix, index, suffix] = String.split(class, @placeholder)
        [prefix, Map.fetch!(placeholder_map, index), suffix]
      else
        class
      end
    end)
    |> Enum.intersperse(" ")
    |> List.flatten()
  end

  defp sort_variant_chains(classes) do
    classes
    |> String.split(~r/\s+(?![^\[]*\])/i, trim: true)
    |> Enum.map(fn class ->
      class
      |> String.split(":")
      |> Enum.sort_by(&variant_position/1, :desc)
      |> Enum.join(":")
    end)
  end

  defp sort(classes, {indent, max_line_length}) when is_binary(classes) do
    leading_space = if classes =~ ~r/\A\s/, do: " "
    trailing_space = if classes =~ ~r/\s\z/, do: " "
    max_line_length = max_line_length - String.length(indent)

    classes =
      classes
      |> sort_variant_chains()
      |> sort()
      |> Enum.reduce({[], 0}, fn
        class, {[], _current_line_length} ->
          {[[class]], String.length(class)}

        class, {[current_line | rest], current_line_length} ->
          class_length = String.length(class)
          line_length = current_line_length + 1 + class_length

          if line_length <= max_line_length do
            {[current_line ++ [class] | rest], line_length}
          else
            {[[class] | [current_line | rest]], class_length}
          end
      end)
      |> elem(0)
      |> Enum.reverse()
      |> Enum.map(&(&1 |> Enum.join(" ")))

    case classes do
      [] ->
        ""

      [single_line] ->
        Enum.join([leading_space, single_line, trailing_space])

      multiple_lines ->
        Enum.join([indent, Enum.join(multiple_lines, indent), String.slice(indent, 0..-3//1)])
    end
  end

  defp sort([]), do: []

  defp sort(class_list) when is_list(class_list) do
    {variants, base_classes} = Enum.split_with(class_list, &variant?/1)

    Enum.sort_by(base_classes, &class_position/1) ++ sort_variant_classes(variants)
  end

  defp placeholder?(class), do: String.contains?(class, @placeholder)
  defp variant?(class), do: String.contains?(class, ":") and not String.starts_with?(class, "[")
  defp prose?(class), do: String.contains?(class, "prose")

  defp class_position(class),
    do: if(placeholder?(class), do: -1_000_000, else: Map.get(Order.classes(), class, -1))

  # prose variant order matters, thus push to front
  defp variant_position(variant),
    do: if(prose?(variant), do: 0, else: Map.get(Order.variants(), variant, -1))

  defp sort_variant_classes(variants) do
    variants
    |> group_by_first_variant()
    |> Enum.sort_by(fn {variant, _rest} -> variant_position(variant) end)
    |> Enum.map(fn {variant, rest} -> {variant, sort(rest)} end)
    |> Enum.flat_map(fn {variant, rest} -> Enum.map(rest, &"#{variant}:#{&1}") end)
  end

  defp group_by_first_variant(variants) do
    variants
    |> Enum.map(&String.split(&1, ":", parts: 2))
    |> Enum.group_by(&List.first/1, &List.last/1)
  end
end
