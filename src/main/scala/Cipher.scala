import scala.annotation.tailrec

/**
  * Provides methods to encrypt or decrypt a string or file.
  * Based on the Vigenere cipher, uses two keys to encrypt the
  * input twice and then shuffles the string or file
  *
  * This class has only immutable state variables and thus is
  * thread safe.
  */
class Cipher {
  val defaultKey2 = "]09agvn cv8eA ino;av 478uyTR`~=( ADJ OD *^t"
  val modulus = 128

  /**
    * Encrypt a string by first applying a Vigenere cipher twice with two keys
    * and then shuffling the results
    *
    * @param str input string to encrypt
    * @param key1 required input key for first encryption
    * @param key2 option second input key (default key used if none) for second encryption
    * @return cipher text
    */
  def encodeString(str: String, key1: String, key2: String = defaultKey2): String = {
    val key1Size = key1.length
    val key2Size = key2.length

    val mid = encrypt(str.toList, key1, key1Size, 0, "")
    val fin = encrypt(mid.toList, key2, key2Size, 0, "")

    var half1 = ""
    var half2 = ""
    var counter = 0

    for(letter <- fin) {
      if(counter == 0) half1 = letter + half1
      else half2 = letter + half2
      counter = (counter + 1) % 2
    }

    half1 + half2
  }

  /**
    * Tail recursive function that applies a Vigenere cipher to an input string
    *
    * @param strList Input string as a list of chars
    * @param key The key to use for the encryption
    * @param keyLength The length of the key
    * @param i The index of the key
    * @param ret The string to return
    * @return An encrypted string
    */
  @tailrec
  private def encrypt(strList: List[Char], key: String, keyLength: Int, i: Int, ret: String): String = strList match {
    case Nil => ret
    case _ =>
      val x = (strList.head + key.charAt(i)) % modulus
      encrypt(strList.tail, key, keyLength, (i + 1) % keyLength, x.asInstanceOf[Char] + ret)
  }

  /**
    * Decrypt a string by un-shuffling it and then twice un-applying a Vingenere
    * cipher with two keys
    *
    * @param str The string to decrypt
    * @param key1 Required input key for first encryption (second decryption)
    * @param key2 Option second input key (default used if none) for second encryption (first decryption)
    * @return plain text
    */
  def decodeString(str: String, key1: String, key2: String = defaultKey2): String = {
    val key1Size = key1.length
    val key2Size = key2.length
    val strLength = str.length

    var middle = 0

    if(strLength % 2 == 0) middle = strLength / 2
    else middle = (strLength / 2) + 1

    var half1 = str.slice(0, middle)
    val half2 = str.slice(middle, strLength)

    var res = ""
    if(strLength % 2 == 1) {
      res = half1.slice(0, 1)
      half1 = half1.slice(1, middle)
    }

    var count = 0
    while(count < middle) {
      res = half1.slice(count, count + 1) + half2.slice(count, count + 1) + res
      count += 1
    }

    val mid = decrypt(res.toList.reverse, key2, key2Size, 0, "")
    val fin = decrypt(mid.toList, key1, key1Size, 0, "")

    fin.reverse
  }

  /**
    * Tail recursive function that un-applies a Vigenere cipher to an input string
    *
    * @param strList Input string as a list of chars
    * @param key key used for encryption
    * @param keyLength length of key
    * @param i index of key
    * @param ret string to return
    * @return a decrypted string
    */
  @tailrec
  private def decrypt(strList: List[Char], key: String, keyLength: Int, i: Int, ret: String): String = strList match {
    case Nil => ret
    case _ =>
      var x = strList.head - key.charAt(i)
      if(x < 0) x = modulus + x
      decrypt(strList.tail, key, keyLength, (i + 1) % keyLength, x.asInstanceOf[Char] + ret)
  }


}

/**
  * Companion object for Cipher class
  */
object Cipher {

  private def demo(): Unit = {
    val cipher = new Cipher
    println("Plain text: Master of Puppets")
    val cipherText = cipher.encodeString("Master of Puppets", "sayaka")
    println("Cipher text: " + cipherText)
    val decryptText = cipher.decodeString(cipherText, "sayaka")
    println("Decrypted text: " + decryptText)
  }

  /**
    * Accept a string and print out its encryption/decryption
    *
    * @param args program arguments
    * @param cipher Cipher object
    */
  private def processString(args: Array[String], cipher: Cipher): Unit = {
    var ret = ""

    if(args(2).toLowerCase == "e") {
      if(args.length == 4) {
        ret = cipher.encodeString(args(0), args(3))
      }
      else {
        ret = cipher.encodeString(args(0), args(3), args(4))
      }
    }
    else if(args(2).toLowerCase == "d") {
      if(args.length == 4) {
        ret = cipher.decodeString(args(0), args(3))
      }
      else {
        ret = cipher.decodeString(args(0), args(3), args(4))
      }
    }
    else {
      println("Invalid Encryption flag")
      println("Use E for encryption or D for decryption")
      System.exit(-1)
    }

    println(ret)
  }

  /**
    * Accept a pathname and create an encrypted/decrypted file
    *
    * @param args program arguments
    * @param cipher Cipher object
    */
  private def processFile(args: Array[String], cipher: Cipher): Unit = {

  }

  /**
    * Main method
    *
    * use: Cipher [string or pathname] [sting or file flag] [key1] [optional key2]
    *
    * @param args arguments
    */
  def main(args: Array[String]): Unit = {
    if(args.length == 1) {
      val arg = args(0).toLowerCase
      if(arg == "help" || arg == "h") {
        println("insert help info here") //update to include help info
        System.exit(0)
      }
      if(arg == "demo" || arg == "d") {
        demo()
        System.exit(0)
      }
    }

    if(!(args.length == 4 || args.length == 5)) {
      println("Invalid Arguments\nFor help enter \"help\"")
      System.exit(-1)
    }

    val cipher = new Cipher
    if(args(1).toLowerCase == "s") {
      processString(args, cipher)
    }
    else if(args(1).toLowerCase == "f") {
      processFile(args, cipher)
    }
    else {
      println("Invalid Arguments\nFor help enter \"help\"")
      System.exit(-1)
    }
  }
}