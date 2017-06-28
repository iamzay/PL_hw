(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str:string,lst:string list) =
  let fun helper(next:string list,prev:string list) =
        case next of
            []=>NONE
          | x::next' => if x=str then SOME (prev@next') else helper(next',prev@[x])
  in
      helper(lst,[])
  end

fun get_substitutions1 (subs:string list list,s:string) =
  case subs of
      []=>[]
    | lst::subs' => case all_except_option(s,lst) of
                       NONE=>get_substitutions1(subs',s)
                     | SOME x => x@get_substitutions1(subs',s)

fun get_substitutions2 (subs:string list list,s:string) =
  let fun helper(next,ans) =
        case next of
            []=>ans
          | lst::next' => case all_except_option(s,lst) of
                              NONE=>helper(next',ans)
                            | SOME x => helper(next',ans@x)
  in
      helper(subs,[])
  end

fun similar_names (lst:string list list,{first=x,middle=y,last=z}) =
  let val subs=get_substitutions2(lst,x)
      fun helper (remain,ans) =
        case remain of
            []=>ans
          | s::remain' => helper(remain',ans@[{first=s,middle=y,last=z}])
  in
      helper(subs,[{first=x,middle=y,last=z}])
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

              (* put your solutions for problem 2 here *)
fun card_color (s,r) =
  case s of
      Clubs=> Black
    | Spades => Black
    | _ => Red

fun card_value (s,r) =
  case r of
      Ace=>11
    | Num value => value
    | _ => 10

fun remove_card (cs:card list,c:card,e) =
  let fun helper (next,c,e,prev)  =
        case next of
            []=>raise e
          | h::next' => if h=c then prev@next' else helper(next',c,e,prev@[h])
  in
      helper(cs,c,e,[])
  end

fun all_same_color (cs:card list) =
  case cs of
      [] => true
    | _::[] => true
    | c1::c2::cs' => card_color(c1)=card_color(c2) andalso all_same_color(c2::cs')

fun sum_cards (cs:card list) =
  let fun helper (cs,prev_sum) =
        case cs of
            []=>prev_sum
          | c::cs' => helper(cs',prev_sum+card_value(c))
  in
      helper(cs,0)
  end

fun score (cs:card list,goal:int) =
  let val sum=sum_cards cs
      val prevscore=if sum>goal then 3*(sum-goal) else goal-sum
  in
      if all_same_color cs
      then prevscore div 2
      else prevscore
  end

fun officiate (cs:card list,moves: move list,goal:int) =
  let fun helper(cs:card list,held:card list,moves:move list) =
        case moves of
            []=>score(held,goal)
          | move::moves' => case move of
                                Discard c => helper(cs,remove_card(held,c,IllegalMove),moves')
                              | Draw => case cs of
                                            []=>score(held,goal)
                                          | c::cs' => if sum_cards(c::held)>goal then score(c::held,goal) else helper(cs',c::held,moves')
  in
      helper(cs,[],moves)
  end
