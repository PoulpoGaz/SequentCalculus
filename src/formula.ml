type 'a formula =
  | Top
  | Bot
  | V of 'a
  | Not of 'a formula
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula
  | Impl of 'a formula * 'a formula