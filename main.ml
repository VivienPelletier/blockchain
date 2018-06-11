let (k_priv, k_pub) = Rsa.generate_keys()

let secret_of_life = 42
let cipher_msg = Rsa.encrypt k_pub secret_of_life
let recovered_msg = Rsa.decrypt k_priv cipher_msg ;;

let _ = Printf.printf "cipher msg : %d recovered msg : %d" cipher_msg recovered_msg
