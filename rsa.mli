(* private_key *)
type private_key = {
  p : BatBig_int.t; (* prime number *)
  q : BatBig_int.t; (* prime number *)
  d : BatBig_int.t} (* private key exponent *)

(* public_key *)
type public_key = {
  n : BatBig_int.t; (* primes number product *)
  e : BatBig_int.t} (* public key exponent *)

val generate_keys : unit -> private_key * public_key

val encrypt : public_key -> BatBig_int.t -> BatBig_int.t
val decrypt : private_key -> BatBig_int.t -> BatBig_int.t

(* not done yet
   type signedMessage = {
    message : string;
    signature : string} (* hash of message encrypt with private_key *)

   val sign : private_key -> string -> signedMessage
   val checksign : public_key -> signedMessage -> bool
*)
