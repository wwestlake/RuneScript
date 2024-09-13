// File: Parser.fs
namespace RuneScript

open FParsec
open RuneScript

module Parser =

    // Define a parser for whitespace
    let ws = spaces

    // Define a parser for identifiers (variable names, function names, etc.)
    let identifier: Parser<string, unit> =
        many1SatisfyL (fun c -> isLetter c || isDigit c || c = '_') "identifier"

    // Define a parser for numbers
    let number: Parser<float, unit> =
        pfloat

    // Define a parser for operators
    let operator: Parser<string, unit> =
        choice [
            pstring "+"
            pstring "-"
            pstring "*"
            pstring "/"
        ]

    // Define a parser for tokens
    let token: Parser<Token, unit> =
        choice [
            identifier |>> Identifier
            number |>> Number
            operator |>> Operator       ]

    // Create a forward reference for parseExpression
    let parseExpression, parseExpressionRef = createParserForwardedToRef<Expression, unit>()

    // Parser for conditional expressions
    let parseConditional: Parser<Expression, unit> =
        let parseIf = pstring "if" >>. ws
        let parseElse = pstring "else" >>. ws
        parseIf >>. parseExpression .>> ws
        .>>. (between (pchar '{') (pchar '}') parseExpression)
        .>>. opt (parseElse >>. (between (pchar '{') (pchar '}') parseExpression))
        |>> (fun ((cond, thenExpr), elseExpr) -> Conditional(cond, thenExpr, elseExpr))

    // Parser for while loops
    let parseWhile: Parser<Expression, unit> =
        let parseWhileKeyword = pstring "while" >>. ws
        parseWhileKeyword >>. parseExpression .>> ws
        .>>. (between (pchar '{') (pchar '}') parseExpression)
        |>> (fun (cond, body) -> WhileLoop(cond, body))

    // Parser for for loops
    let parseFor: Parser<Expression, unit> =
        let parseForKeyword = pstring "for" >>. ws
        let parseInKeyword = pstring "in" >>. ws
        parseForKeyword >>. identifier .>> ws .>> parseInKeyword
        .>>. parseExpression .>> ws
        .>>. (between (pchar '{') (pchar '}') parseExpression)
        |>> (fun ((varName, rangeExpr), body) -> ForLoop(varName, rangeExpr, rangeExpr, body))

    // Parser for function definitions
    let parseFunctionDef: Parser<Expression, unit> =
        let parseDef = pstring "def" >>. ws
        let parseParams = between (pchar '(') (pchar ')') (sepBy identifier (pchar ',' >>. ws))
        parseDef >>. identifier .>> ws
        .>>. parseParams .>> ws
        .>>. (between (pchar '{') (pchar '}') parseExpression)
        |>> (fun ((name, params), body) -> FunctionDef(name, params, body))

    // Parser for return statements
    let parseReturn: Parser<Expression, unit> =
        let parseReturnKeyword = pstring "return" >>. ws
        parseReturnKeyword >>. opt parseExpression |>> Return

    // Define the recursive function to parse expressions
    let rec parseExpressionImpl: Parser<Expression, unit> =
        let term: Parser<Expression, unit> = choice [
            parseConditional
            parseWhile
            parseFor
            parseFunctionDef
            parseReturn
            number |>> (fun n -> Literal (Number n))
            identifier |>> (fun id -> Variable id)
                                                    ] .>> ws // Ensure to consume any trailing whitespace

        let binaryOpParser: Parser<(Expression -> Expression -> Expression), unit> =
            ws >>. operator .>> ws |>> (fun op -> fun left right -> BinaryOp(op, left, right))

        chainl1 term binaryOpParser

    // Assign the implementation to the forward reference
    do parseExpressionRef := parseExpressionImpl

    // Define the entry point to start the parsing process
    let runescriptParser: Parser<Expression, unit> = 
        ws >>. parseExpression .>> eof
