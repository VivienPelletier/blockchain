open BatBig_int

type private_key = {
  p : big_int; (* prime number *)
  q : big_int; (* prime number *)
  d : big_int} (* private key exponent *)

type public_key = {
  n : big_int; (* primes number product *)
  e : big_int} (* public key exponent *)

let primes_list = 
  let select x =
    gt_big_int (of_int x) (pow (of_int 2) (of_int 13)) in
  List.filter select (Primes.primes (BatInt.pow 2 14))
let primes_count = List.length primes_list

let random_prime () =
  let i  = Random.int primes_count in
  of_int (List.nth primes_list i)

let euclid phi_n e =
  let rec eucl r u v r2 u2 v2 = 
    if equal r2 zero then (r, u, v)
    else eucl r2 u2 v2 (r - (r/r2)*r2) (u - (r/r2)*u2) (v - (r/r2)*v2) in
  eucl phi_n one zero e zero one

let phi p q = 
  let a = pred p and b = pred q in
  let (gcd,_,_) = euclid a b in
  a*b/gcd


let _ = Random.self_init()

let rec generate_keys () =
  let e   = of_int (Random.int 10000)
  and p   = random_prime()
  and q   = random_prime() in
  let n   = p*q
  and phi = phi p q in
  let (r, u, v) = euclid phi e in
  if equal r one then ({
      p   = p;
      q   = q;
      d   = if lt_big_int v zero then v+phi else v},{
        n   = n;
        e   = e
      })
  else
    generate_keys()

let powmod x e m =
  let rec powmod (x, e, m, acc) = 
    if equal e zero
    then acc
    else powmod (x, pred e, m, mod_big_int (acc*x) m) in
  powmod (x, e, m, one)

let encrypt k m = 
  powmod m k.e k.n

let decrypt k m = 
  powmod m k.d (k.p*k.q)

