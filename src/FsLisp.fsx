// #r "netstandard"
#r "../packages/FParsec/lib/netstandard2.0/FParsecCS.dll"
#r "../packages/FParsec/lib/netstandard2.0/FParsec.dll"

type LispVal =
  | Atom of string
  | Number of int
  | Bool of bool
  | List of LispVal list
  | Error of string
with
  override this.ToString() =
    this |> function
    | Atom(s) -> s
    | Number(n) -> sprintf "%d" n
    | Bool(true) -> "#t"
    | Bool(false) -> "#f"
    | List(l) -> l |> Seq.map string |> String.concat " " |> sprintf "(%s)"
    | Error msg -> sprintf "Error: %s" msg

  static member inline FromFSharpType(x) = Number x
  static member inline FromFSharpType(x) = Bool x

  static member inline TryFSharpType(x, _: int) = x |> function
    | Number n -> Some n
    | _ -> None

  static member inline TryFSharpType(x, _: bool) = x |> function
    | Bool t -> Some t
    | _ -> None

module private Eval =
  type Env = Map<string, LispVal>

  let error fmt = Printf.kprintf (sprintf "%s" >> Error) fmt

  let inline fromFSharpType(a: ^a) : ^b =
    ((^a or ^b): (static member FromFSharpType: _->_)a)

  let inline (|FSharpType|_|) (a: ^a): ^b option =
    ((^a or ^b): (static member TryFSharpType:_*_->_) a, Unchecked.defaultof< ^b >)

  let inline binOp opName (op: 'a -> 'a -> 'a) : string*_ =
    opName, (
      function
      | FSharpType a :: FSharpType b :: xs ->
        let rec f r = function
        | [] -> fromFSharpType r
        | (FSharpType x) :: xs -> f (op r x) xs
        | x :: _ -> error "'%O' is an invalid argument for operator '%s'" x opName
        f (op a b) xs
      | args -> error "'%O' is an invalid argument for operator '%s'" (List args) opName
    )

  let inline bin2Op opName (op: 'a -> 'a -> 'b) : string*_ =
    opName, (
      function
      | [FSharpType a ; FSharpType b] -> op a b |> fromFSharpType
      | args -> error "'%O' is invalid argument for operator '%s'" (List args) opName
    )

  let inline numBin2Op opName (op: int -> int -> bool) = bin2Op opName op

  let primitives = dict [
    binOp "+" (+)
    binOp "-" (-)
    binOp "*" ( * )
    binOp "/" (/)
    numBin2Op ">" (>)
    numBin2Op ">=" (>=)
    numBin2Op "<" (<)
    numBin2Op "<=" (<=)
    binOp "and" (&&)
    binOp "or" (||)
    "=", (function
      | [Bool a; Bool b] -> Bool (a = b)
      | [Number a; Number b] -> Bool (a = b)
      | args -> error "'%O' is invalid argument for operator '='" (List args)
    )
    "/=", (function
      | [Bool a; Bool b] -> Bool (a <> b)
      | [Number a; Number b] -> Bool (a <> b)
      | args -> error "'%O' is invalid argument for operator '/='" (List args)
    )
  ]

  let apply funcName args =
    primitives.TryGetValue(funcName) |> function
    | true, f -> f args
    | _ -> error "function '%s' is not found" funcName

  let rec eval env = function
    | List [Atom "if"; ifForm; thenForm; elseForm] ->
      eval env ifForm |> fst |> function
        | Bool true -> eval env thenForm
        | Bool false -> eval env elseForm
        | _ -> error "'%O' is invalid if-condition" ifForm, env

    | List (Atom "if" :: _) -> Error "invalid format of 'if' expression", env

    | List [Atom "define"; Atom var; form] ->
      let res, _ = eval env form
      res, Map.add var res env

    | List(Atom funcName :: args) -> apply funcName (List.map (eval env >> fst) args), env
    | Atom var ->
      (Map.tryFind var env |> function
        | Some v -> v
        | None -> error "Unknown atom '%s'" var
      ), env
    | lispVal -> lispVal, env


module private Parser =
  open FParsec

  let charsToStr cs = new string (Array.ofSeq cs)

  let symbol<'a> : Parser<char, 'a> = anyOf "!#$%&|*+-/:<=>?@^_~"

  let parseAtom<'a> : Parser<LispVal, 'a> =
    parse {
      let! first = letter <|> symbol
      let! rest = many (letter <|> digit <|> symbol)
      return Atom (charsToStr <| first :: rest)
    }

  let parseNumber<'a> : Parser<LispVal, 'a> =
    many1 digit |>> (charsToStr >> int >> Number)

  let parseBool<'a> : Parser<LispVal, 'a> =
    (attempt <| stringReturn "#t" (Bool true))
    <|> (attempt <| stringReturn "#f" (Bool false))

  let rec parseList<'a> : Parser<LispVal, 'a> =
    parse {
      let! _ = pchar '('
      let! xs = sepBy parseExpr (skipMany1 spaces1)
      let! _ = pchar ')'
      return List xs
    }

  and parseExpr<'a> : Parser<LispVal, 'a> =
    parseBool
    <|> parseAtom
    <|> parseNumber
    <|> parseList

  let parse s =
    run parseExpr s |> function
    | Success(x, _, _) -> Result.Ok x
    | Failure (msg, _, _) -> Result.Error msg

let run env input =
  Parser.parse input
  |> Result.map (Eval.eval env)
