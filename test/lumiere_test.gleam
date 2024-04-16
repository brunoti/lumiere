import gleeunit
import gleeunit/should
import lumiere.{Color, InvalidColor}

pub fn main() {
  gleeunit.main()
}

pub fn parse_hex_without_alpha_test() {
  lumiere.parse_hex("#000000")
  |> should.equal(Color(0, 0, 0, 100, 0, 0, 0))
  lumiere.parse_hex("#ffffff")
  |> should.equal(Color(255, 255, 255, 100, 0, 0, 100))
}

pub fn parse_hex_with_alpha_test() {
  lumiere.parse_hex("#ffffffff")
  |> should.equal(Color(255, 255, 255, 100, 0, 0, 100))
  lumiere.parse_hex("#000000ff")
  |> should.equal(Color(0, 0, 0, 100, 0, 0, 0))
}
