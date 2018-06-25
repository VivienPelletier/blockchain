let (k_priv, k_pub) = Rsa.generate_keys()

(*let secret_of_life = Bytes.of_string "cuc cuc cuc cuc cuc cuc  what does the fox say ?" *)
let secret_of_life = Bytes.of_string "cuc" 
let cipher_msg = Rsa.encrypt k_pub secret_of_life
let recovered_msg = Rsa.decrypt k_priv cipher_msg 

let () = Printf.printf "recovered msg : %s\n"
    (Bytes.to_string recovered_msg)
