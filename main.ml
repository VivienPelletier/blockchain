let (k_priv, k_pub) = Rsa.generate_keys()

let secret_of_life = BatBig_int.of_int 42
let cipher_msg = Rsa.encrypt k_pub secret_of_life
let recovered_msg = Rsa.decrypt k_priv cipher_msg 

let () = Printf.printf "cipher msg : %d recovered msg : %d\n"
    (BatBig_int.int_of_big_int cipher_msg) 
    (BatBig_int.int_of_big_int recovered_msg)
