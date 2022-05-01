(* # CSCI3180 Principles of Programming Languages
#
# --- Declaration ---
#
# I declare that the assignment here submitted is original except for source
# material explicitly acknowledged. I also acknowledge that I am aware of
# University policy and regulations on honesty in academic work, and of the
# disciplinary guidelines and procedures applicable to breaches of such policy
# and regulations, as contained in the website
# http://www.cuhk.edu.hk/policy/academichonesty/
#
# Assignment 4
# Name : Tang Zi Zhou
# Student ID : 1155124275
# Email Addr : 1155124275@link.cuhk.edu.hk *)

datatype hTree = Leaf of char list * int
               | Interior of hTree * hTree * char list * int

(* 1. TODO: implement getSymbols *)
fun getSymbols (x : hTree) : char list =
    case x of
      Interior (ht1, ht2, ls, n) => ls
     |Leaf (ls, n) => ls

(* 2. TODO: implement getWeight *)
fun getWeight (x : hTree) : int =
    case x of
      Interior (ht1, ht2, ls, n) => n
     |Leaf (ls, n) => n

(* 3. TODO: implement mergeNode *)
fun mergeNode (ht1, ht2 : hTree) : hTree =
    let 
      val weights = getWeight(ht1) + getWeight(ht2)
      val symbols = getSymbols(ht1)@getSymbols(ht2)
    in
      Interior (ht1, ht2, symbols, weights)
    end

(* 4. TODO: implement insertNode *)
fun insertNode (ht1, ls)= 
    let fun insertTree(h::t, ys, 0) = 
              if getWeight(h) < getWeight(ys)
              then
              h::insertTree(t,ht1,0)
              else 
              ys::insertTree(h::t,ht1,1)
        | insertTree([], ys, 0) = [ys]
        | insertTree(xs, ys, 1) = xs
    in
      insertTree(ls, ht1, 0)
    end
    

(* 5. TODO: implement makeTree *)
fun makeTree (ls)= 
    if length(ls) = 1
      then hd(ls)
    else
      let 
          val newtree = mergeNode(hd(ls), hd(tl(ls)))
          val newlist = insertNode(newtree,tl(tl(ls)))
      in
          makeTree(newlist)
      end

(* Check your implementations.

- val leaves = [(Leaf([#"A"],2)),(Leaf([#"D"],2)),(Leaf([#"B"],3)),(Leaf([#"C"],3))];
val leaves = [Leaf ([#"A"],2),Leaf ([#"D"],2),Leaf ([#"B"],3),Leaf ([#"C"],3)]
  : hTree list
- val tree = makeTree leaves;

Due to different versions of the compiler, the output might be different. 
If your compiler version is 110.99, you should get the following result.

val tree =
  Interior
    (Interior (Leaf ([#"A"],2),Leaf ([#"D"],2),[#"A",#"D"],4),
     Interior (Leaf ([#"B"],3),Leaf ([#"C"],3),[#"B",#"C"],6),
     [#"A",#"D",#"B",#"C"],10) : hTree

If your run the code on tutorialpoint with compiler of version 110.78 or 
sparc1 with compiler of version 110.0.7, you should get the following result.

val tree =
  Interior
    (Interior (Leaf #,Leaf #,[#,#],4),Interior (Leaf #,Leaf #,[#,#],6),
     [#"A",#"D",#"B",#"C"],10) : hTree

*)
