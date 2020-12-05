open Common

type register =
  |A
  |B
  |C
  |D
  |E
  |H
  |L

type register_pair =
  |BC
  |DE
  |HL

let string_of_register r =
  match r with
  |A -> "A"
  |B -> "B"
  |C -> "C"
  |D -> "D"
  |E -> "E"
  |H -> "H"
  |L -> "L"

let string_of_register_pair r =
  match r with
  |BC -> "B"
  |DE -> "D"
  |HL -> "H"

let fst_register rp =
  match rp with
  |BC -> B
  |DE -> D
  |HL -> H

let snd_register rp =
  match rp with
  |BC -> C
  |DE -> E
  |HL -> L
