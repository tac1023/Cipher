import java.io._

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
  private def encodeFile(reader: FileReader, writer: FileWriter, key1: String, key2:String = defaultKey2): Unit = {
    val key1Size = key1.length
    val key2Size = key2.length
    try {
      encryptFile(reader, writer, key1, key2, key1Size, key2Size, 0, 0)
    }
    catch {
      case _: IOException => println("IOException caused encryption to abort")
    }
  }

  @tailrec
  private def encryptFile(reader: FileReader, writer: FileWriter, key1: String, key2: String,
                  keyLength1: Int, keyLength2: Int, i: Int, j: Int): Unit = reader.read() match {
    case -1 =>
    case c =>
      writer.write(encryptChar(c.asInstanceOf[Char], key1.charAt(i), key2.charAt(j)))
      encryptFile(reader, writer, key1, key2, keyLength1, keyLength2, (i + 1) % keyLength1, (j + 1) % keyLength2)
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
      val c = encryptChar(strList.head, key1.charAt(i), key2.charAt(j))
      encryptString(strList.tail, key1, key2, keyLength1, keyLength2, (i + 1) % keyLength1, (j + 1) % keyLength2,
        c + ret1, ret2, 1)
    case (_, 1) =>
      val c = encryptChar(strList.head, key1.charAt(i), key2.charAt(j))
      encryptString(strList.tail, key1, key2, keyLength1, keyLength2, (i + 1) % keyLength1, (j + 1) % keyLength2,
        ret1, c + ret2, 0)
  }

  private def encryptChar(c: Char, keyChar1: Char, keyChar2: Char): Char = {
    val x = (c + keyChar1) % modulus
    val y = (x + keyChar2) % modulus
    y.asInstanceOf[Char]
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
    * Decrypt a file by twice un-applying
    * a Vingenere cipher with two keys. The results are stored into a new file
    *
    * @param file File to decrypt
    * @param key1 Required input key for firs encryption (second decryption)
    * @param key2 Optional input key (default used if none) for second encryption (first decryption)
    */
  private def decodeFile(fileReader: FileReader, fileWriter: FileWriter, key1: String, key2: String = defaultKey2): Unit = {
    val key1Size = key1.length
    val key2Size = key2.length
    try {
      decryptFile(fileReader, fileWriter, key1, key2, key1Size, key2Size, 0, 0)
    }
    catch {
      case _: IOException => println("IOException caused decryption to abort")
    }
  }

  /**
    * Decrypt a file
    *
    * @param reader FileReader of input file
    * @param writer FileWriter of output file
    * @param key1 First key used to encrypt
    * @param key2 Second key used to encrypt
    * @param keyLength1 Length of key1
    * @param keyLength2 Length of key2
    * @param i Index of key1
    * @param j Index of key2
    */
  @tailrec
  private def decryptFile(reader: FileReader, writer: FileWriter, key1: String, key2: String, keyLength1: Int,
                  keyLength2: Int, i: Int, j: Int): Unit = reader.read() match {
    case -1 =>
    case c =>
      writer.write(decryptChar(c.asInstanceOf[Char], key1.charAt(i), key2.charAt(j)))
      decryptFile(reader, writer, key1, key2, keyLength1, keyLength2, (i + 1) % keyLength1, (j + 1) % keyLength2)
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
      val c = decryptChar(strList.head, key1.charAt(i), key2.charAt(j))
      decryptString(strList.tail, key1, key2, keyLength1, keyLength2, (i + 1) % keyLength1, (j + 1) % keyLength2,
        c + ret)
  }

  /**
    * Takes a character along with the corresponding characters of two keys and
    * decrypts the character
    *
    * @param c Character to be decrypted
    * @param keyChar1 Character from key1
    * @param keyChar2 Character from key2
    * @return Decrypted Character
    */
  private def decryptChar(c: Char, keyChar1: Char, keyChar2: Char): Char = {
    var x = c - keyChar2
    if(x < 0) x = modulus + x
    var y = x - keyChar1
    if(y < 0) y = modulus + y
    y.asInstanceOf[Char]
  }


}

/**
  * Companion object for Cipher class
  */
object Cipher {

  /**
    * Quick demo String encryption/decryption for testing purposes
    */
  private def demo(): Unit = {
    val cipher = new Cipher
    println("Plain text: Master of Puppets, The New Order, Rust In Peace")
    val cipherText = cipher.encodeString("Master of Puppets, The New Order, Rust In Peace", "sayaka")
    println("Cipher text: " + cipherText)
    val decryptText = cipher.decodeString(cipherText, "sayaka")
    println("Decrypted text: " + decryptText)
  }

  /**
    * Quick demo File encryption/decryption for testing purposes
    *
    * note* This is hardcoded to work on my development machine
    */
  private def demoFile(): Unit = {
    val cipher = new Cipher
    println("Input: example.txt")
    val urlEncrypt = "D:\\Users\\Tyler\\Documents\\School\\sideProjects\\Cipher\\example.txt"
    val urlDecrypt = "D:\\Users\\Tyler\\Documents\\School\\sideProjects\\Cipher\\example_encrypted.txt"
    val key = "sayaka"
    val argsEncrypt: Array[String] = Array(urlEncrypt, "f", "e", key)
    val argsDecrypt: Array[String] = Array(urlDecrypt, "f", "d", key)
    processFile(argsEncrypt, cipher)
    println("Encrypted File")
    processFile(argsDecrypt, cipher)
    println("Decrypted File")
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

    val filename = file.getName
    val directory = file.getAbsolutePath.replace(filename, "")
    val splice = filename.lastIndexOf(".")
    val name = filename.substring(0, splice)
    val extension = filename.substring(splice)
    var outputFilename: String = ""
    if(args(2).toLowerCase == "e") outputFilename = directory + name + "_encrypted" + extension
    if(args(2).toLowerCase == "d") outputFilename = directory + name + "_decrypted" + extension

    var fileReader: FileReader = null
    var fileWriter: FileWriter = null
    try {
      fileReader = new FileReader(file)
      fileWriter = new FileWriter(outputFilename)
    }
    catch {
      case _: FileNotFoundException => println("Could not open input file")
        System.exit(-1)
      case _: IOException => println("Could not open output file")
    }

    if(args(2).toLowerCase == "e") {
      if(args.length == 4) {
        cipher.encodeFile(fileReader, fileWriter, args(3))
      }
      else {
        cipher.encodeFile(fileReader, fileWriter, args(3), args(4))
      }
    }
    else if (args(2).toLowerCase == "d") {
      if(args.length == 4) {
        cipher.decodeFile(fileReader, fileWriter, args(3))
      }
      else {
        cipher.decodeFile(fileReader, fileWriter, args(3), args(4))
      }
    }
    else {
      println("Invalid Encryption flag")
      println("Use E for encryption or D for decryption")
      System.exit(-1)
    }

    fileReader.close()
    fileWriter.close()
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
      println(arg)
      if(arg == "help" || arg == "h") {
        println("insert help info here") //update to include help info
        System.exit(0)
      }
      else if(arg == "demo" || arg == "d") {
        demo()
        System.exit(0)
      }
      else if(arg == "demofile" || arg == "df") {
        demoFile()
        System.exit(0)
      }
    }

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