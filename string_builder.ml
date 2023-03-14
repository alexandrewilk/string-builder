(* Q 1 *)

(* Déclaration du type *)
type string_builder = 
|Mot of string*int
|Noeud of string_builder * string_builder * int ;;

(* On créé 2 string_builders pour nos test *)

let gattaca = Noeud(Mot("g", 1), Noeud(Mot("att", 3), Noeud(Mot("a", 1), Mot("ca", 2), 3), 6), 7);;
let acattag = Noeud(Mot("a", 1), Noeud(Mot("cat", 3), Noeud(Mot("t", 1), Mot("ag", 2), 3), 6), 7);;

(* Fonction simple transformant un string en un string_builder *)
let word s = Mot(s, String.length s);;

let () = assert(word "gattaca" = Mot("gattaca", 7))

 (* Fonction de concatenation de 2 string_builders *)

 let concat sb1 sb2 = match sb1, sb2 with
|Mot(_, c1), Mot(_, c2) -> Noeud(sb1, sb2, c1+c2)
|Mot(_, c1), Noeud(_, _, c2) -> Noeud(sb1, sb2, c1+c2)
|Noeud(_, _, c1), Mot(_, c2) -> Noeud(sb1, sb2, c1+c2)
|Noeud(_, _, c1), Noeud(_, _, c2) -> Noeud(sb1, sb2, c1+c2);;

let () = assert (concat gattaca acattag = Noeud(Noeud(Mot("g", 1), Noeud(Mot("att", 3), Noeud(Mot("a", 1), Mot("ca", 2), 3), 6), 7), Noeud(Mot("a", 1), Noeud(Mot("cat", 3), Noeud(Mot("t", 1), Mot("ag", 2), 3), 6), 7), 14)) ;;

(* Question 2 *)

(* Fonction donnant la longeur d'un string_builder, utilisée dans char_at *)
let length sb = match sb with
|Mot(_, c) -> c 
|Noeud(_, _, c) -> c;;

(* Fonction donnant le caractère i d'un string_builder (i de 0 à length sb -1)*)
let rec char_at sb i = match sb with 
|Mot(s, _) -> String.get s i
|Noeud(sbg, sbd, c) -> if (i< (length sbg)) then char_at sbg i else char_at sbd (i - (length sbg));;

let () = assert(char_at gattaca 5 = 'c')
let () = assert(char_at acattag 1 = 'c')

(* Question 3 *)

(* Fonction donnant le  string builder issu du caractère i de taille m *)
let rec sub_string sb i m = match sb with
|Mot(s,_) -> if ((String.length s)> 0 && m>0 && i>=0)  then Mot(String.sub s i m, m) else Mot("", 0) (*Car je ne sais pas dire else doNothing/Continue....*)
|Noeud(sbg,sbd,ln) -> let lg = length sbg in 
                        if i+m<=lg then sub_string sbg i m
                        else if i>lg then sub_string sbd i m
                        else Noeud(sub_string sbg i (lg-i), sub_string sbd 0 (m-lg+i), m) ;;

let () = assert (sub_string gattaca 2 3 = Noeud (Mot ("t", 1), Noeud (Mot ("a", 1), Mot ("c", 1), 2), 3))
let() = assert (sub_string acattag 1 4 = Noeud (Mot ("", 0), Noeud (Mot ("cat", 3), Mot ("t", 1), 4), 4))

(* Question 4  *)

(* Fonction de cout d'accès *)
let cost sb = let rec aux sb depth  = match sb with
|Noeud(sbg, sbd, c) -> aux sbg (1+depth) + (aux sbd (1+depth))
|Mot(s, c) -> depth * c
in aux sb 0;;

let () = assert(cost gattaca = 16)
let () = assert(cost acattag = 16)

(* Question 5 *)
let _ = Random.self_init ;; (* Pour initialiser Random*)

(* Génère un char random sous forme de string utile dans rd_string_of_length*)
let random_char () = Char.escaped (Char.chr (97 + (Random.int 26)));;

(* Génère un string de taille n  *)
let rd_string_of_length n = let rec gen str acc n = if acc < n then gen ((random_char ()) ^ str) (acc+1) n else str 
in gen "" 0 n;;

(* Génère un string builder de profondeur i, Dont la forme et les Mots sont générés aléatoirement *)
let rec random_string i = if i = 0 then word (rd_string_of_length ((Random.int 4) + 1)) else let aux rd i = if rd=0 then 
  concat (random_string (i-1)) (random_string (i-1)) else if rd=1 then
  concat (random_string(i-1)) (word (rd_string_of_length (Random.int 5))) else 
  concat (word (rd_string_of_length (Random.int 5))) (random_string(i-1)) 
  in aux (Random.int 3) i;; 

(* let _ = random_string 3  *) (* A uncomment pour afficher dans la console le random_string généré*)

(* Question 6 *)

(* Fonction retournant une char list a partir d'un string (dans le même ordre)*)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

(* Fonction retournant le string issue d'un string_builder (parcours infixe)*)
let rec string_of sb = match sb with 
|Mot(s1, c1) -> s1
|Noeud(sbg, sbd, c) -> (string_of sbg) ^ (string_of sbd);;

(* Fonction retournant la char list des caractère du string_builder  *)
let list_of_string sb = explode (string_of sb) ;;

let () = assert(list_of_string gattaca = ['g'; 'a'; 't'; 't'; 'a'; 'c'; 'a']);;
let () = assert(list_of_string acattag = ['a'; 'c'; 'a'; 't'; 't'; 'a'; 'g']);;

(* Question 7 *)
(* Liste des mots du string_builder *)
let list_of_mot sb = List.map word (List.map (Char.escaped) (list_of_string sb))

(*Cout de la concaténation de 2 string builders*)
let cost_concat sb1 sb2 = cost (concat sb1 sb2) 

(* Position du dans la liste du noeud ou le coût de concaténation est le plus faible *)
let min_concat_cost_pos l = let rec aux l acc cur_pos min_pos = match l with 
|[sb1]::[sb2]::t -> let c = cost_concat sb1 sb2  in if c >acc then aux ([sb2]::t) c (cur_pos +1) (cur_pos)
   else aux ([sb2]::t) acc (cur_pos) (min_pos)
|_ -> min_pos
  in aux l 1000 0 0;;

