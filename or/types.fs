namespace Operations.Research

module Types =

  type SolverParams = {
    name: string
  }


  type Variable =
    | Boolean of string
    | Integer of string * int * int
    | Real of string * float * float
