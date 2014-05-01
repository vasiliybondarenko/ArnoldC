package org.arnoldc.ast

import org.objectweb.asm.{Label, MethodVisitor}
import org.objectweb.asm.Opcodes._
import org.arnoldc.SymbolTable


case class ForNode(loopVariableName: String, minValue: OperandNode, maxValue: OperandNode, statements: List[AstNode]) extends StatementNode{
  override def generate(mv: MethodVisitor, symbolTable: SymbolTable){
    val loopStart = new Label()
    val loopEnd = new Label()
    symbolTable.putLabel(loopEnd)

    minValue.generate(mv, symbolTable)
    mv.visitVarInsn(ISTORE, symbolTable.getVariableAddress(loopVariableName))

    mv.visitLabel(loopStart)
    mv.visitFrame(F_FULL, symbolTable.size(), symbolTable.getStackFrame, 0, null)

    mv.visitVarInsn(ILOAD, symbolTable.getVariableAddress(loopVariableName))
    maxValue.generate(mv, symbolTable)
    mv.visitJumpInsn(IF_ICMPGT, loopEnd)

    val loopVarAddress = symbolTable.getVariableAddress(loopVariableName)
    statements.foreach((statement:AstNode) => {
      statement.generate(mv, symbolTable)
      mv.visitIincInsn(loopVarAddress, 1)
    })

    mv.visitJumpInsn(GOTO, loopStart)
    mv.visitLabel(loopEnd)
    mv.visitFrame(F_SAME, 0, null, 0, null)
  }
}
