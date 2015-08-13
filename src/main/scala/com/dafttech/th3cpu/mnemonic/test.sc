import com.dafttech.th3cpu.mnemonic.MnemonicParser

val parser = new MnemonicParser

parser.parseAll(parser.instructions,
  """jmp
    |label(test)
    |const(ptr, test)
    |jmp
  """.stripMargin).get.foreach((b) => println(Integer.toBinaryString(b.toInt)))