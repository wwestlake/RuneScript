// File: Ast.fs
namespace RuneScript

// Define the basic types for tokens and expressions
type Token =
    | Identifier of string
    | Number of float
    | Operator of string
    | Keyword of string

type Expression =
    | Literal of Token
    | Variable of string
    | BinaryOp of string * Expression * Expression
    | FunctionCall of string * Expression list
    | Conditional of Expression * Expression * Expression option  // For if-else
    | WhileLoop of Expression * Expression                       // For while loops
    | ForLoop of string * Expression * Expression * Expression   // For for loops
    | FunctionDef of string * string list * Expression           // For function definitions
    | Return of Expression option                                // For return statements
