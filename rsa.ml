open Primes
open Batteries

type private_key = {
    p : int; (** prime number **)
    q : int; (** prime number **)
    d : int} (** private key exponent **)

type public_key = {
    n : int; (** primes number product **)
    e : int} (** public key exponent **)

let primes_list = 
    let select x = x > BatInt.pow 2 13 in List.filter select (Primes.primes (BatInt.pow 2 14))
let primes_count = List.length primes_list

let euclid phin e =
    let rec eucl r u v r2 u2 v2 = 
        if r2 = 0 then (r, u, v)
        else eucl r2 u2 v2 (r - (r/r2)*r2) (u - (r/r2)*u2) (v - (r/r2)*v2) in
    eucl phin 1 0 e 0 1

let phi p q = 
    let a = p-1 and b = q-1 in
    let (gcd,_,_) = euclid a b in
    a*b/gcd
    

let _ = Random.self_init()

let rec generate_keys () =
    let pi  = Random.int primes_count
    and qi  = Random.int primes_count 
    and e   = Random.int 10000 in
    let p   = List.nth primes_list pi
    and q   = List.nth primes_list qi in
    let n   = p*q
    and phi = phi p q in
    let (r, u, v) = euclid phi e in
    if r = 1 then ({
        p   = p;
        q   = q;
        d   = if v < 0 then v+phi else v},{
        n   = n;
        e   = e
        })
    else
        generate_keys()

let rec powmod (m, e, n, acc) = match e with
    | 0 -> acc
    | e -> powmod (m, e-1, n, (acc*m) mod n)

let encrypt k m = 
    powmod (m, k.e, k.n, 1)

let decrypt k m = 
    powmod (m, k.d, k.p*k.q, 1)

type signedMessage = {
    message : string;
    signature : string} (** hash of message encrypt with private_key **)

let sign k m = {message=""; signature=""}
let checksign k m = false
