(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

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

(**** for the challenge problem only ****)

datatype typ = Anything
	           | UnitT
	           | IntT
	           | TupleT of typ list
	           | Datatype of string

                               (**** you can put all your code here ****)
fun only_capitals (strs) =
  List.filter (fn str => ((Char.isUpper o String.sub) (str,0) ))  strs

fun longest_string1 (strs) =
  foldl (fn (str,ans)=>if String.size str>String.size ans then str else ans) "" strs

fun longest_string2 (strs) =
  foldl (fn (str,ans)=>if String.size str>=String.size ans then str else ans) "" strs

fun longest_string_helper cmp = fn strs =>
                                   foldl (fn (str,ans)=>if cmp(String.size str,String.size ans) then str else ans) "" strs

val longest_string3=longest_string_helper (fn (a,b)=>a>b)
val longest_string4=longest_string_helper (fn (a,b)=>a>=b)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f = fn lst =>
                    case lst of
                        []=>raise NoAnswer
                      | x::lst' => case (f x) of
                                       NONE=>first_answer f lst'
                                     | SOME value => value

fun all_answers f = fn lst =>
                       let val helper=fn(a,acc)=>
                                         case (f a) of
                                             NONE=>acc
                                           | SOME b => acc@[b]
                       in
                           case lst of
                               []=>SOME []
                             | _ => case (foldl helper [] lst) of []=>NONE
                                                               | lst => SOME lst
                       end

