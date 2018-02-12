library(openssl)
hash1 <- sha256('Cesare sends one bitcoin to Shimon')

nonce <- 0
hash2 <- 0
while(substr(hash2,1,3) != '000') {

  
unhash <- paste(hash1, "85738f8f9a7f1b04b5329c590ebcb9e425925c6d0984089c43a022de4f19c281", 
               "2018-01-07 21:05:34",
               "3",
               nonce)
hash2 <- sha256(unhash)
nonce <- nonce + 1
}
print(substr(hash2,1,3))
print(nonce-1)
