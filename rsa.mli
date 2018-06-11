(** private_key **)
type private_key = {
    p : int; (** prime number **)
    q : int; (** prime number **)
    d : int} (** private key exponent **)

(** public_key **)
type public_key = {
    n : int; (** primes number product **)
    e : int} (** public key exponent **)

val generate_keys : unit -> private_key * public_key

val encrypt : public_key -> int -> int
val decrypt : private_key -> int -> int

type signedMessage = {
    message : string;
    signature : string} (** hash of message encrypt with private_key **)

val sign : private_key -> string -> signedMessage
val checksign : public_key -> signedMessage -> bool
