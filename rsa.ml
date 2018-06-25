open BatBig_int

type private_key = {
  p : big_int; (* prime number *)
  q : big_int; (* prime number *)
  d : big_int} (* private key exponent *)

type public_key = {
  n : big_int; (* primes number product *)
  e : big_int} (* public key exponent *)

type crypted_msg = bytes list

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

let encrypt_big_int k m = 
  powmod m k.e k.n

let decrypt_big_int k m = 
  powmod m k.d (k.p*k.q)

let bytes_of_big_int big_int2conv = 
  let rec bytes_of_big_int2 big_int2conv bytes_converted =
    if lt_big_int big_int2conv (of_int 255) then
      Bytes.cat 
        bytes_converted
        (Bytes.make 1 
           (char_of_int 
              (int_of_big_int big_int2conv)))
    else
      bytes_of_big_int2 
        (big_int2conv / (of_int 256))
        (Bytes.cat 
           bytes_converted
           (Bytes.make 1 
              (char_of_int 
                 (int_of_big_int
                    (mod_big_int big_int2conv (of_int 256))))))
  in
  bytes_of_big_int2 big_int2conv (Bytes.create 0)

let big_int_of_bytes bytes2conv =
  let rec big_int_of_bytes2 bytes2conv converted_big_int =
    if Bytes.length bytes2conv = 0 then
      converted_big_int
    else
      big_int_of_bytes2 
        (Bytes.sub bytes2conv 1 
           (Pervasives.pred
              (Bytes.length bytes2conv)))
        converted_big_int*(of_int 256) + (of_int(int_of_char (Bytes.get bytes2conv 0)))
  in
  big_int_of_bytes2 bytes2conv zero

let encrypt key bytes2encrypt = 
  let block_bytes_size = Pervasives.(/) (num_bits_big_int key.n) 8 in
  let rec encrypt_blocks begin_index encrypted_blocks =
    let encrypt_one_block = fun end_index ->
      bytes_of_big_int 
        (encrypt_big_int key 
           (big_int_of_bytes 
              (Bytes.sub bytes2encrypt begin_index end_index))) in
    if begin_index >= Pervasives.(-) (Bytes.length bytes2encrypt) block_bytes_size
    then 
      encrypt_one_block 
        (Pervasives.(-) (Bytes.length bytes2encrypt) begin_index)
      :: encrypted_blocks
    else 
      encrypt_blocks 
        (Pervasives.(+)  begin_index block_bytes_size) 
        ((encrypt_one_block block_bytes_size)::encrypted_blocks)
  in
  encrypt_blocks 0 []

let decrypt = fun key blocks2decrypt ->
  let rec decrypt_blocks = fun blocks2decrypt decrypted_bytes ->
    let decrypt_one_block = fun block ->
      bytes_of_big_int 
        (decrypt_big_int key 
           (big_int_of_bytes block)) 
    in
    match blocks2decrypt with
    | [] -> decrypted_bytes
    | head::tail -> decrypt_blocks tail (Bytes.cat (decrypt_one_block head) decrypted_bytes)
  in
  decrypt_blocks blocks2decrypt (Bytes.create 0)
