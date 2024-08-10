object CiesarCipher{
  
  def encrypt(word:String, shiftAmount:Int):String = {
    word.map {
      case x if x.isLetter =>
        val m = if (x.isUpper) 'A' else 'a'
        ((x - m + shiftAmount) % 26 + m).toChar
      case x => x 
    }
  }

  def decrypt(word:String, shiftAmount:Int):String = {
    encrypt(word,26-shiftAmount)
  }

  def cipher(word:String, shiftAmount:Int, func: (String,Int) => String):String = {
    func(word,shiftAmount)
  }
}



object Main extends App {
  val text = "IAmPawani"
  val shiftAmount = 3

  val encryptedText = CiesarCipher.cipher(text,shiftAmount,CiesarCipher.encrypt)
  println(s"Encrypted text: $encryptedText")

  val decryptedText = CiesarCipher.cipher(encryptedText,shiftAmount,CiesarCipher.decrypt)
  println(s"Decrypted text: $decryptedText")
}