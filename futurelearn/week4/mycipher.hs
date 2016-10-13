import Data.Char

shouldcipher :: Char -> Bool
shouldcipher c = isLetter(c) && isAscii(c)

cipherchar :: Int -> Char ->Char
cipherchar shift c
  | shouldcipher c = chr(ord(c)+shift)
  | otherwise = c

cipher :: Int -> [Char] -> [Char]
cipher shift plaintext = map (cipherchar shift) plaintext

decipher shift ciphertext = cipher (-shift) ciphertext
