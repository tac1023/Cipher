class Cipher {
  val asciiArray: Array[Char] = Array(65)

  def getArray: Array[Char] = asciiArray
  val defaultKey2 = "]09cv8eA 478uyTR`~=( ADJ OD  *^t"
  val modulus = 128

  def encodeString(str: String, key1: String, key2: String = defaultKey2): String = {
    val key1Size = key1.length
    val key2Size = key2.length
    val strList = str.toList



    encrypt(strList, key1, key1Size, 0, "")
  }

  private def encrypt(strList: List[Char], key: String, keyLength: Int, i: Int, ret: String): String = strList match {
    case Nil => ret
    case _ =>
      val x = (strList.head + key.charAt(i)) % modulus
      //println(x.asInstanceOf[Char])
      encrypt(strList.tail, key, keyLength, (i + 1) % keyLength, x.asInstanceOf[Char] + ret)
  }


  def decodeString(str: String, key1: String, key2: String = defaultKey2): String = {
    val key1Size = key1.length
    val key2Size = key2.length
    val strList = str.toList.reverse


    decrypt(strList, key1, key1Size, 0, "")
  }

  private def decrypt(strList: List[Char], key: String, keyLength: Int, i: Int, ret: String): String = strList match {
    case Nil => ret.reverse
    case _ =>
      var x = strList.head - key.charAt(i)
      if(x < 0) x = modulus + x
      decrypt(strList.tail, key, keyLength, (i + 1) % keyLength, x.asInstanceOf[Char] + ret)
  }


}


object Cipher {

  def main(args: Array[String]): Unit = {
    //println("Hi, I'm working")
    val cipher = new Cipher
    println("Plain text: Master of Puppets")
    val cipherText = cipher.encodeString("Master of Puppets", "sayaka")
    println("Cipher text: " + cipherText)
    val decryptText = cipher.decodeString(cipherText, "sayaka")
    println("Decrypted text: " + decryptText)
  }
}