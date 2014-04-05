package org.arnoldc.ast

import org.objectweb.asm.{Label, MethodVisitor}
import org.objectweb.asm.Opcodes._
import org.arnoldc.SymbolTable
import org.parboiled.errors.ParsingException


case class BreakNode(operand: OperandNode) extends StatementNode {
  def generate(mv: MethodVisitor, symbolTable: SymbolTable) {
    val loopEnd = symbolTable.getLastLabel().getOrElse(
      throw new ParsingException("WHAT THE FUCK DID I DO WRONG:\nBreak statement cannot be placed outside of loop\n")
    )
    operand.generate(mv, symbolTable)
    mv.visitIntInsn(BIPUSH, 1)
    mv.visitJumpInsn(IF_ICMPEQ, loopEnd)
    mv.visitLabel(loopEnd)
  }
}
