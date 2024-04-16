import gleam/bool.{guard, lazy_guard}
import gleam/float as f
import gleam/int as i
import gleam/io
import gleam/list as l
import gleam/result as r
import gleam/string as s

pub type Color {
  Color(
    red: Int,
    green: Int,
    blue: Int,
    alpha: Int,
    hue: Int,
    saturation: Int,
    lightness: Int,
  )
  InvalidColor(String)
}

pub type ColorError {
  ColorError(String)
}

fn rgb_to_hsl(
  red: Int,
  green: Int,
  blue: Int,
) -> Result(#(Int, Int, Int), String) {
  let red = i.to_float(red) /. 255.0
  let green = i.to_float(green) /. 255.0
  let blue = i.to_float(blue) /. 255.0
  use max <- r.try(
    [red, green, blue]
    |> l.reduce(f.max)
    |> r.replace_error("Could not found RGB max value"),
  )
  use min <- r.try(
    [red, green, blue]
    |> l.reduce(f.min)
    |> r.replace_error("Could not found RGB min value"),
  )
  let diff = max -. min
  let lightness = { max +. min } /. 2.0

  let #(hue, saturation, lightness) = case diff {
    0.0 -> #(0.0, 0.0, lightness)
    _ -> {
      let add = case green <. blue {
        True -> 6.0
        False -> 0.0
      }
      let hue =
        {
          use <- guard(diff == red, return: { green -. blue } /. diff +. add)
          use <- guard(diff == green, return: { blue -. red } /. diff +. 2.0)
          use <- guard(diff == blue, return: { red -. green } /. diff +. 4.0)
          0.0
        }
        /. 6.0
        |> f.multiply(360.0)

      let saturation =
        guard(
          when: lightness >. 0.5,
          return: diff /. { 2.0 -. max -. min },
          otherwise: fn() { diff /. { max +. min } },
        )

      #(hue, saturation, lightness)
    }
  }

  Ok(#(
    hue
      |> f.truncate(),
    saturation
      |> f.multiply(100.0)
      |> f.truncate(),
    lightness
      |> f.multiply(100.0)
      |> f.truncate(),
  ))
}

pub fn do_parse_hex(value: String) -> Result(Color, Color) {
  let hex = case value {
    "#" <> hex -> hex
    hex -> hex
  }

  let corrected_value_result = case s.length(hex) {
    3 ->
      Ok(
        hex
        |> s.to_graphemes()
        |> l.map(s.repeat(_, 2))
        |> s.join("")
        <> "FF",
      )
    6 -> Ok(hex <> "FF")
    8 -> Ok(hex)
    _ -> Error(InvalidColor("Invalid hex string length"))
  }

  use corrected_value <- r.try(corrected_value_result)
  let rgba_hex =
    [
      s.slice(corrected_value, at_index: 0, length: 2),
      s.slice(corrected_value, at_index: 2, length: 2),
      s.slice(corrected_value, at_index: 4, length: 2),
      s.slice(corrected_value, at_index: 6, length: 2),
    ]
    |> l.try_map(i.base_parse(_, 16))
    |> r.replace_error(InvalidColor("Invalid hex string"))

  case rgba_hex {
    Ok([r_hex, g_hex, blue_hex, alpha_hex]) -> {
      use #(hue, saturation, lightness) <- r.try(
        rgb_to_hsl(r_hex, g_hex, blue_hex)
        |> r.map_error(fn(error) {
          InvalidColor("Could not convert RGB to HSL. Reason: " <> error)
        }),
      )
      Ok(Color(
        red: r_hex,
        green: g_hex,
        blue: blue_hex,
        alpha: i.to_float(alpha_hex) /. 255.0 *. 100.0
          |> f.truncate(),
        hue: hue,
        saturation: saturation,
        lightness: lightness,
      ))
    }
    _ -> Error(InvalidColor("Invalid hex string"))
  }
}

pub fn do_parse_rgb(red: Int, green: Int, blue: Int) -> Result(Color, Color) {
  let is_valid =
    [red, green, blue]
    |> l.all(fn(x) {
      io.debug(x >= 0 && x <= 255)
      x >= 0 && x <= 255
    })

  use <- lazy_guard(is_valid, otherwise: fn() {
    Error(InvalidColor("Invalid RGB values"))
  })
  use #(hue, saturation, lightness) <- r.try(
    rgb_to_hsl(red, green, blue)
    |> r.map_error(fn(error) {
      InvalidColor("Could not convert RGB to HSL. Reason: " <> error)
    }),
  )
  Ok(Color(
    red: red,
    green: green,
    blue: blue,
    alpha: 100,
    hue: hue,
    saturation: saturation,
    lightness: lightness,
  ))
}

pub fn parse_hex(hex: String) -> Color {
  do_parse_hex(hex)
  |> r.unwrap_both()
}

pub fn parse_rgb(red: Int, green: Int, blue: Int) -> Color {
  do_parse_rgb(red, green, blue)
  |> r.unwrap_both()
}

pub fn parse_rgba(red: Int, green: Int, blue: Int, alpha: Int) -> Color {
  use <- lazy_guard(alpha >= 0 && alpha <= 100, otherwise: fn() {
    InvalidColor("Invalid alpha value")
  })
  case
    do_parse_rgb(red, green, blue)
    |> r.unwrap_both()
  {
    Color(r, g, b, _, hue, saturation, lightness) -> {
      Color(r, g, b, alpha, hue, saturation, lightness)
    }
    invalid -> invalid
  }
}

pub fn is_valid(color: Color) -> Bool {
  case color {
    Color(_, _, _, _, _, _, _) -> True
    InvalidColor(_) -> False
  }
}

pub fn hex_parse_rgb(red: Int, green: Int, blue: Int) -> Color {
  do_parse_rgb(red, green, blue)
  |> r.unwrap_both()
}

pub fn to_hex_string(
  value: Color,
  with_alpha with_alpha: Bool,
) -> Result(String, ColorError) {
  case value {
    Color(red, green, blue, alpha, _, _, _) -> {
      Ok(
        "#"
        <> i.to_base16(red)
        |> s.pad_left(2, "0")
        <> i.to_base16(green)
        |> s.pad_left(2, "0")
        <> i.to_base16(blue)
        |> s.pad_left(2, "0")
        <> case with_alpha {
          True ->
            i.to_base16(
              {
                alpha
                |> i.multiply(255)
                |> i.to_float()
              }
              /. 100.0
              |> f.round(),
            )
            |> s.pad_left(2, "0")
          False -> ""
        },
      )
    }
    InvalidColor(error) -> Error(ColorError(error))
  }
}

pub fn to_rgb_string(value: Color) -> Result(String, ColorError) {
  case value {
    Color(red, green, blue, _, _, _, _) -> {
      Ok(
        "rgb("
        <> [red, green, blue]
        |> l.map(i.to_string)
        |> s.join(", ")
        <> ")",
      )
    }
    InvalidColor(error) -> Error(ColorError(error))
  }
}

pub fn to_rgba_string(value: Color) -> Result(String, ColorError) {
  case value {
    Color(red, green, blue, _, _, _, _) -> {
      Ok(
        "rgba("
        <> [red, green, blue]
        |> l.map(i.to_string)
        |> s.join(", ")
        <> ")",
      )
    }
    InvalidColor(error) -> Error(ColorError(error))
  }
}
