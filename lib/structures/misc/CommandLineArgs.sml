structure CommandLineArgs :>
sig
  (* Must be called before any of the functions below. *)
  val init : unit -> unit

  (* parseOrDefaultX (k, d) returns v if "-k v" was given at command line,
   * or d otherwise. *)
  val parseOrDefaultString : string * string -> string
  val parseOrDefaultInt : string * int -> int
  val parseOrDefaultReal : string * real -> real

  (* parseFlag k returns whether or not --k was given at at the command line *)
  val parseFlag : string -> bool
end =
struct

  structure Table = MkTreapTable (structure Key = StringElt)
  structure Set = Table.Set

  val argTable : (string Table.t * Set.t) option ref = ref NONE

  fun makeArgTable (stringArgs : string list) =
    let
      datatype arg = KEY of string | FLAG of string | VALUE of string | INVALID of string
      fun arg x =
        let val n = String.size x
        in if n = 0 then INVALID x else
           if String.sub (x, 0) <> #"-" then VALUE x else
           if n = 1 then INVALID x else
           if String.sub (x, 1) <> #"-" then KEY (String.extract (x, 1, NONE)) else
           if n = 2 then INVALID x else
           FLAG (String.extract (x, 2, NONE))
        end

      fun organize (keys, flags) (args : arg list) =
        case args of
          [] => (keys, flags)
        | KEY k :: VALUE v :: args' => organize (Table.insert (keys, (k, v)), flags) args'
        | FLAG f :: args' => organize (keys, Table.Set.insert (flags, f)) args'
        | INVALID x :: _ => raise Fail ("invalid parameter \"" ^ x ^ "\"")
        | _ => raise Fail "badly formatted parameters"

      val init = (Table.empty (), Table.Set.empty ())
    in
      organize init (List.map arg stringArgs)
    end

  fun init () = (argTable := SOME (makeArgTable (CommandLine.arguments ())))

  fun keys () =
    case !argTable of
      NONE => raise Fail "Need to call CommandLineArgs.init ()"
    | SOME (ks, _) => ks

  fun flags () =
    case !argTable of
      NONE => raise Fail "Need to call CommandLineArgs.init ()"
    | SOME (_, fs) => fs

  fun parseOrDefaultString (k, d) =
    case Table.find (keys ()) k of
      NONE => d
    | SOME v => v

  fun parseOrDefaultInt (k, d) =
    case Table.find (keys ()) k of
      NONE => d
    | SOME v =>
        case Int.fromString v of
          NONE => (print ("Unable to parse integer from " ^ v ^ ", using default of " ^ Int.toString d ^ " instead.\n"); d)
        | SOME x => x

  fun parseOrDefaultReal (k, d) =
    case Table.find (keys ()) k of
      NONE => d
    | SOME v =>
        case Real.fromString v of
          NONE => (print ("Unable to parse real from " ^ v ^ ", using default of " ^ Real.toString d ^ " instead.\n"); d)
        | SOME x => x

  fun parseFlag f = Set.find (flags ()) f

end
