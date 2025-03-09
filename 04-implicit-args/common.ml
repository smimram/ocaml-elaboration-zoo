type icit = [`Explicit | `Implicit]

let icit_pa = function
  | `Explicit -> fun s -> "("^s^")"
  | `Implicit -> fun s -> "{"^s^"}"
