(* private_key *)
type private_key
  
(* public_key *)
type public_key
  
type cipher

val generate_keys : unit -> private_key * public_key

val encrypt_big_int : public_key -> BatBig_int.t -> BatBig_int.t
val decrypt_big_int : private_key -> BatBig_int.t -> BatBig_int.t

val encrypt : public_key -> bytes -> cipher
val decrypt : private_key -> cipher -> bytes

(* not done yet
   type signedMessage = {
    message : string;
    signature : string} (* hash of message encrypt with private_key *)

   val sign : private_key -> string -> signedMessage
   val checksign : public_key -> signedMessage -> bool
*)
