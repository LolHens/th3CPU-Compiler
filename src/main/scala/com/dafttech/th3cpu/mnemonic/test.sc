import com.dafttech.th3cpu.mnemonic.MnemonicParser

val parser = new MnemonicParser

parser.parseAll(parser.insnParser,
  """jmp
    |label(test)
    |const(ptr, test)
    |jmp
  """.stripMargin).get.foreach((b) => println(Integer.toBinaryString(b.toInt)))