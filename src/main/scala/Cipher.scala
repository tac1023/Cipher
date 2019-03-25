import java.io.File

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
  //Default second key to use if user doesn't give a second key
  val defaultKey2 = "]09agvn cv8eA ino;av 478uyTR`~=( ADJ OD *^t"

  //size of alphabet. 128 because ASCII
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

    encryptString(str.toList, key1, key2, key1Size, key2Size, 0, 0, "", "", 0)
  }

  /**
    * Encrypt the contents of a given file by first applying a Vignere cipher twice
    * with two keys then shuffling the results. The encrypted contents are stored
    * into a new file.
    *
    * @param file The file to encrypt
    * @param key1 Required input key for first encryption
    * @param key2 Option input key (default key used if none) for second encryption
    */
  def encodeFile(file: File, key1: String, key2:String = defaultKey2): Unit = {
    println(file.getPath)
  }

  /**
    * Tail recursive function that applies a Vigenere cipher to an input string
    *
    * Deprecated
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
    * Tail recursive function that applies a Vigenere cipher to an input string
    *
    * @param strList Input string as a list of chars
    * @param key1 The first key to use for the encryption
    * @param key2 The second key to use for the encryption
    * @param keyLength1 The length of key1
    * @param keyLength2 The length of key2
    * @param i The index of key1
    * @param j The index of key2
    * @param ret1 The first half of the string to return
    * @param ret2 The second half of the string to return
    * @param toggle Select whether to write to ret1 or ret2
    * @return An encrypted string
    */
  @tailrec
  private def encryptString(strList: List[Char], key1: String, key2: String, keyLength1: Int, keyLength2: Int,
                            i: Int, j: Int, ret1: String, ret2: String, toggle: Int): String = (strList, toggle) match {
    case (Nil, _) => ret1 + ret2
    case (_, 0) =>
      val x = (strList.head + key1.charAt(i)) % modulus
      val y = (x + key2.charAt(j)) % modulus
      encryptString(strList.tail, key1, key2, keyLength1, keyLength2, (i + 1) % keyLength1, (j + 1) % keyLength2,
        y.asInstanceOf[Char] + ret1, ret2, 1)
    case (_, 1) =>
      val x = (strList.head + key1.charAt(i)) % modulus
      val y = (x + key2.charAt(j)) % modulus
      encryptString(strList.tail, key1, key2, keyLength1, keyLength2, (i + 1) % keyLength1, (j + 1) % keyLength2,
        ret1, y.asInstanceOf[Char] + ret2, 0)
  }

  /**
    * Decrypt a string by un-shuffling it and then twice un-applying a Vingenere
    * cipher with two keys
    *
    * @param str The string to decrypt
    * @param key1 Required input key for first encryption (second decryption)
    * @param key2 Optional second input key (default used if none) for second encryption (first decryption)
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

    decryptString(res.toList, key1, key2, key1Size, key2Size, 0, 0, "").reverse
  }

  /**
    * Decrypt a file by first un-shuffling the contents then twice un-applying
    * a Vingenere cipher with two keys. The results are stored into a new file
    *
    * @param file File to decrypt
    * @param key1 Required input key for firs encryption (second decryption)
    * @param key2 Optional input key (default used if none) for second encryption (first decryption)
    */
  def decodeFile(file: File, key1: String, key2: String = defaultKey2): Unit = {

  }

  /**
    * Tail recursive function that un-applies a Vigenere cipher to an input string
    *
    * Deprecated
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

  /**
    * Tail recursive function that un-applies a Vigenere cipher to an input string
    *
    * @param strList Input string as a list of chars
    * @param key1 first key used for encryption
    * @param key2 second key used for encryption
    * @param keyLength1 length of key1
    * @param keyLength2 length of key2
    * @param i index of key1
    * @param j index of key2
    * @param ret string to return
    * @return a decrypted string
    */
  @tailrec
  private def decryptString(strList: List[Char], key1: String, key2: String, keyLength1: Int, keyLength2: Int,
                            i: Int, j: Int, ret: String): String = strList match {
    case Nil => ret
    case _ =>
      var x = strList.head - key2.charAt(j)
      if(x < 0) x = modulus + x
      var y = x - key1.charAt(i)
      if(y < 0) y = modulus + y
      decryptString(strList.tail, key1, key2, keyLength1, keyLength2, (i + 1) % keyLength1, (j + 1) % keyLength2,
        y.asInstanceOf[Char] + ret)
  }


}

/**
  * Companion object for Cipher class
  */
object Cipher {

  private def demo(): Unit = {
    val cipher = new Cipher
    println("Plain text: Master of Puppets")
    val cipherText = cipher.encodeString("Master of Puppets, The New Order, Rust In Peace", "sayaka")
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
    val file = new File(args(0))
    if(!file.exists()) {
      println("Could not open file")
      System.exit(-1)
    }

    if(args(2).toLowerCase == "e") {
      if(args.length == 4) {
        cipher.encodeFile(file, args(3))
      }
      else {
        cipher.encodeFile(file, args(3), args(4))
      }
    }
    else if (args(2) == "d") {
      if(args.length == 4) {
        cipher.decodeFile(file, args(3))
      }
      else {
        cipher.decodeFile(file, args(3), args(4))
      }
    }
    else {
      println("Invalid Encryption flag")
      println("Use E for encryption or D for decryption")
      System.exit(-1)
    }
  }

  /**
    * Main method
    *
    * use: Cipher [string or pathname] [sting or file flag] [encrypt or decrypt flag] [key1] [optional key2]
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

    //Debug
    /*for( arg <- args) {
      println(arg)
    }*/

    if(!(args.length == 4 || args.length == 5)) {
      println("Invalid Arguments\nToo few or too many\nFor help enter \"help\"")
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