(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** for the challenge problem only ****)

datatype typ = Anything
	           | UnitT
	           | IntT
	           | TupleT of typ list
	           | Datatype of string

                               (**** you can put all your code here ****)
val only_capitals=
  List.filter (fn str => ((Char.isUpper o String.sub) (str,0) ))

val longest_string1=
  foldl (fn (str,ans)=>if String.size str>String.size ans then str else ans) ""

val longest_string2=
  foldl (fn (str,ans)=>if String.size str>=String.size ans then str else ans) ""

fun longest_string_helper cmp = fn strs =>
                                   foldl (fn (str,ans)=>if cmp(String.size str,String.size ans) then str else ans) "" strs

val longest_string3=longest_string_helper (fn (a,b)=>a>b)
val longest_string4=longest_string_helper (fn (a,b)=>a>=b)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode

fun first_answer f = fn lst =>
                    case lst of
                        []=>raise NoAnswer
                      | x::lst' => case (f x) of
                                       NONE=>first_answer f lst'
                                     | SOME value => value

fun all_answers f = fn lst =>
                       let fun helper (lst,SOME ans)=
                                         case lst of
                                             []=>SOME []
                                           | x::lst' => case (f x) of
                                                           NONE=>NONE
                                                         | SOME v => helper(lst',SOME (ans@v))
                       in
                           helper(lst,SOME [])
                       end

(* part2 *)
datatype pattern = Wildcard
		             | Variable of string
		             | UnitP
		             | ConstP of int
		             | TupleP of pattern list
		             | ConstructorP of string * pattern

datatype valu = Const of int
	            | Unit
	            | Tuple of valu list
	            | Constructor of string * valu

fun g f1 f2 p =
  let
	    val r = g f1 f2
  in
	    case p of
	        Wildcard          => f1 ()
	      | Variable x        => f2 x
	      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	      | ConstructorP(_,p) => r p
	      | _                 => 0
  end

val count_wildcards = g (fn ()=>1) (fn s=>0)

val count_wild_and_variable_lengths = g (fn ()=>1) String.size

val count_some_var=fn (s,p) => g (fn()=>0) (fn x=>if x=s then 1 else 0) p

val check_pat =
  let
      fun getStrs p=
        case p of
            Variable x=>[x]
          | TupleP ps => List.foldl (fn (p,acc)=>(getStrs p)@acc) [] ps
          | ConstructorP(_,p) => getStrs p
          | _ => []
      fun NotDuplicate lst=
        case lst of
            x::lst'=>(not (List.exists (fn a=>a=x) lst')) andalso (NotDuplicate lst')
          | _ => true
  in
      NotDuplicate o getStrs
  end

fun match (valu,p) =
               case (valu,p) of
                   (_,Wildcard)=>SOME []
                 | (v,Variable s) =>SOME [(s,v)]
                 | (Unit,Unip) =>SOME []
                 | (Const a,ConstP b) => if a=b then SOME [] else NONE
                 | (Constructor(s2,v),ConstructorP(s1,p)) => if s1=s2 then match(v,p) else NONE
                 | (Tuple vs,TupleP ps) => if List.length vs=List.length ps then all_answers match (ListPair.zip(vs,ps)) else NONE
                 | _ => NONE


fun first_match valu ps =
  SOME (first_answer (fn p=>match(valu,p)) ps) handle NoAnswer=>NONE

