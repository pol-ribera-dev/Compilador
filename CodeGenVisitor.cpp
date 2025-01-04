//////////////////////////////////////////////////////////////////////
//
//    CodeGenVisitor - Walk the parser tree to do
//                     the generation of code
//
//    Copyright (C) 2020-2030  Universitat Politecnica de Catalunya
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU General Public License
//    as published by the Free Software Foundation; either version 3
//    of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Affero General Public License for more details.
//
//    You should have received a copy of the GNU Affero General Public
//    License along with this library; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
//
//    contact: José Miguel Rivero (rivero@cs.upc.edu)
//             Computer Science Department
//             Universitat Politecnica de Catalunya
//             despatx Omega.110 - Campus Nord UPC
//             08034 Barcelona.  SPAIN
//
//////////////////////////////////////////////////////////////////////

#include "CodeGenVisitor.h"
#include "antlr4-runtime.h"

#include "../common/TypesMgr.h"
#include "../common/SymTable.h"
#include "../common/TreeDecoration.h"
#include "../common/code.h"

#include <string>
#include <cstddef> // std::size_t

// uncomment the following line to enable debugging messages with DEBUG*
// #define DEBUG_BUILD
#include "../common/debug.h"

// using namespace std;

// Constructor
CodeGenVisitor::CodeGenVisitor(TypesMgr &Types,
                               SymTable &Symbols,
                               TreeDecoration &Decorations) : Types{Types},
                                                              Symbols{Symbols},
                                                              Decorations{Decorations}
{
}

// Accessor/Mutator to the attribute currFunctionType
TypesMgr::TypeId CodeGenVisitor::getCurrentFunctionTy() const
{
  return currFunctionType;
}

void CodeGenVisitor::setCurrentFunctionTy(TypesMgr::TypeId type)
{
  currFunctionType = type;
}

// Methods to visit each kind of node:
//
antlrcpp::Any CodeGenVisitor::visitProgram(AslParser::ProgramContext *ctx)
{
  DEBUG_ENTER();
  code my_code;
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  for (auto ctxFunc : ctx->function())
  {
    subroutine subr = visit(ctxFunc);
    my_code.add_subroutine(subr);
  }
  Symbols.popScope();
  DEBUG_EXIT();
  return my_code;
}

antlrcpp::Any CodeGenVisitor::visitFunction(AslParser::FunctionContext *ctx)
{
  DEBUG_ENTER();
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  subroutine subr(ctx->ID()->getText());
  codeCounters.reset();
  setCurrentFunctionTy(getTypeDecor(ctx->basic_type()));
  if (ctx->basic_type())
    subr.add_param("_result", Types.to_string(getTypeDecor(ctx->basic_type())), false);

  for (auto &real : ctx->params())
  {
    std::vector<std::pair<std::pair<std::string, std::string>, bool>> &&param = visit(real);
    for (auto &one : param)
    {
      if (one.second)
      {
        subr.add_param(one.first.first, one.first.second, true);
      }
      else
      {
        subr.add_param(one.first.first, one.first.second, false);
      }
    }
  }
  std::vector<var> &&lvars = visit(ctx->declarations());
  for (auto &onevar : lvars)
  {
    subr.add_var(onevar);
  }

  instructionList &&code = visit(ctx->statements());
  code = code || instruction(instruction::RETURN());
  subr.set_instructions(code);
  Symbols.popScope();
  DEBUG_EXIT();
  return subr;
}

antlrcpp::Any CodeGenVisitor::visitParams(AslParser::ParamsContext *ctx)
{
  DEBUG_ENTER();
  TypesMgr::TypeId t1 = getTypeDecor(ctx->type());
  std::vector<std::pair<std::pair<std::string, std::string>, bool>> params;
  for (auto &param : ctx->ID())
  {
    if (Types.isArrayTy(t1))
    {
      params.push_back({{param->getText(), Types.to_string(Types.getArrayElemType(t1))}, true});
    }
    else
    {
      params.push_back({{param->getText(), Types.to_string(t1)}, false});
    }
  }
  DEBUG_EXIT();
  return params;
}

antlrcpp::Any CodeGenVisitor::visitReturn(AslParser::ReturnContext *ctx)
{
  DEBUG_ENTER();
  instructionList code;

  if (ctx->expr())
  {
    CodeAttribs &&codAtExpr = visit(ctx->expr());
    TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
    TypesMgr::TypeId t2 = getCurrentFunctionTy();
    std::string addrExpr = codAtExpr.addr;
    instructionList &codeExpr = codAtExpr.code;
    if (Types.isIntegerTy(t1) and Types.isFloatTy(t2))
    {
      std::string tempFloat = "%" + codeCounters.newTEMP();
      codeExpr = codeExpr || instruction::FLOAT(tempFloat, addrExpr);
      addrExpr = tempFloat;
    }
    code = codeExpr || instruction::LOAD("_result", addrExpr) || instruction::RETURN();
  }
  else
  {
    code = instruction::RETURN();
  }
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitDeclarations(AslParser::DeclarationsContext *ctx)
{
  DEBUG_ENTER();
  std::vector<var> lvars;
  for (auto &varDeclCtx : ctx->variable_decl())
  {
    std::vector<var> vars = visit(varDeclCtx);
    for (size_t i = 0; i < vars.size(); i++)
      lvars.push_back(vars[i]);
  }
  DEBUG_EXIT();
  return lvars;
}

antlrcpp::Any CodeGenVisitor::visitVariable_decl(AslParser::Variable_declContext *ctx)
{
  DEBUG_ENTER();
  TypesMgr::TypeId t1 = getTypeDecor(ctx->type());
  std::size_t size = Types.getSizeOfType(t1);
  std::vector<var> vars;
  if (ctx->type()->type_array())
  {
    size = std::stol(ctx->type()->type_array()->INTVAL()->getText());
    t1 = getTypeDecor(ctx->type()->type_array()->basic_type());
  }

  for (size_t i = 0; i < ctx->ID().size(); i++)
  {
    var onevar = var{ctx->ID(i)->getText(), Types.to_string(t1), size};
    vars.push_back(onevar);
  }
  DEBUG_EXIT();
  return vars;
}

antlrcpp::Any CodeGenVisitor::visitStatements(AslParser::StatementsContext *ctx)
{
  DEBUG_ENTER();
  instructionList code;
  for (auto stCtx : ctx->statement())
  {
    instructionList &&codeS = visit(stCtx);
    code = code || codeS;
  }
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitAssignStmt(AslParser::AssignStmtContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs &&codAtsE1 = visit(ctx->left_expr());
  std::string addr1 = codAtsE1.addr;
  std::string offs1 = codAtsE1.offs;
  instructionList &code1 = codAtsE1.code;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());

  CodeAttribs &&codAtsE2 = visit(ctx->expr());
  std::string addr2 = codAtsE2.addr;
  std::string offs2 = codAtsE2.offs;
  instructionList &code2 = codAtsE2.code;
  TypesMgr::TypeId tid2 = getTypeDecor(ctx->expr());
  instructionList &&code = code1 || code2;
  if (Types.isArrayTy(tid1) and Types.isArrayTy(tid1))
  {
    if (not Symbols.isLocalVarClass(addr1))
    {
      std::string temp = "%" + codeCounters.newTEMP();
      code = code || instruction::LOAD(temp, addr1);
      addr1 = temp;
    }
    if (not Symbols.isLocalVarClass(addr2))
    {
      std::string temp1 = "%" + codeCounters.newTEMP();
      code = code || instruction::LOAD(temp1, addr2);
      addr2 = temp1;
    }

    int numElems = Types.getArraySize(tid1);
    std::string temp2 = "%" + codeCounters.newTEMP();
    std::string toffset = "%" + codeCounters.newTEMP();

    std::string val;
    for (int i = 0; i <= numElems; i++)
    {
      val = std::to_string(i);
      code = code || instruction::ILOAD(toffset, val) ||
             instruction::LOADX(temp2, addr2, toffset) ||
             instruction::XLOAD(addr1, toffset, temp2);
    }
  }
  else
  {

    if (Types.isFloatTy(tid1) and not Types.isFloatTy(tid2))
    {
      std::string tempFloat = "%" + codeCounters.newTEMP();
      code = code || instruction::FLOAT(tempFloat, addr2);
      addr2 = tempFloat;
    }

    if (ctx->left_expr()->expr())
    {
      code = code || instruction::XLOAD(addr1, offs1, addr2);
    }
    else
    {
      code = code || instruction::LOAD(addr1, addr2);
    }
  }
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWhile(AslParser::WhileContext *ctx)
{
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs &&codAtsE = visit(ctx->expr());
  std::string addr1 = codAtsE.addr;
  instructionList &code1 = codAtsE.code;
  instructionList &&code2 = visit(ctx->statements());
  std::string label = codeCounters.newLabelWHILE();
  std::string labelEnd = "endwhile" + label;
  std::string labelStart = "startwhile" + label;

  code = instruction::LABEL(labelStart) || code1 || instruction::FJUMP(addr1, labelEnd) ||
         code2 || instruction::UJUMP(labelStart) || instruction::LABEL(labelEnd);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitIfStmt(AslParser::IfStmtContext *ctx)
{
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs &&codAtsE = visit(ctx->expr());
  std::string addr1 = codAtsE.addr;
  instructionList &code1 = codAtsE.code;
  instructionList &&code2 = visit(ctx->statements(0));
  std::string label = codeCounters.newLabelIF();
  std::string labelEndIf = "endif" + label;

  if (ctx->ELSE())
  {
    instructionList &&codeElse = visit(ctx->statements(1));
    std::string labelElse = "else" + label;
    code = code1 || instruction::FJUMP(addr1, labelElse) ||
           code2 || instruction::UJUMP(labelEndIf) ||
           instruction::LABEL(labelElse) || codeElse ||
           instruction::LABEL(labelEndIf);
  }
  else
  {
    code = code1 || instruction::FJUMP(addr1, labelEndIf) ||
           code2 || instruction::LABEL(labelEndIf);
  }

  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitProcCall(AslParser::ProcCallContext *ctx)
{
  DEBUG_ENTER();
  TypesMgr::TypeId tId = getTypeDecor(ctx->ident());
  instructionList code;
  std::string name = ctx->ident()->getText();
  auto tipos = Types.getFuncParamsTypes(getTypeDecor(ctx->ident()));
  if (not Types.isVoidFunction(tId))
  {
    code = code || instruction::PUSH();
  }
  for (size_t i = 0; i < (ctx->expr()).size(); ++i)
  {
    CodeAttribs &&codAtParam = visit(ctx->expr(i));
    TypesMgr::TypeId tipoP = getTypeDecor(ctx->expr(i));
    code = code || codAtParam.code;
    std::string temp = codAtParam.addr;
    if (Types.isFloatTy(tipos[i]) and Types.isIntegerTy(tipoP))
    {
      temp = "%" + codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp, codAtParam.addr);
    }
    else if (Types.isArrayTy(tipoP) and Symbols.isLocalVarClass(codAtParam.addr))
    {
      temp = "%" + codeCounters.newTEMP();
      code = code || instruction::ALOAD(temp, codAtParam.addr);
    }

    code = code || instruction::PUSH(temp);
  }

  code = code || instruction::CALL(name);

  if (not Types.isVoidFunction(tId))
  {
    code = code || instruction::POP();
  }
  for (size_t i = 0; i < (ctx->expr()).size(); ++i)
  {
    code = code || instruction::POP();
  }

  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitFcall(AslParser::FcallContext *ctx)
{
  DEBUG_ENTER();
  instructionList code;
  std::string name = ctx->ident()->getText();
  code = code || instruction::PUSH();
  auto tipos = Types.getFuncParamsTypes(getTypeDecor(ctx->ident()));
  for (size_t i = 0; i < (ctx->expr()).size(); ++i)
  {
    CodeAttribs &&codAtParam = visit(ctx->expr(i));
    TypesMgr::TypeId tipoP = getTypeDecor(ctx->expr(i));
    std::string temp = codAtParam.addr;
    code = code || codAtParam.code;
    if (Types.isFloatTy(tipos[i]) and Types.isIntegerTy(tipoP))
    {
      temp = "%" + codeCounters.newTEMP();
      code = code || instruction::FLOAT(temp, codAtParam.addr);
    }
    else if (Types.isArrayTy(tipoP))
    {
      temp = "%" + codeCounters.newTEMP();
      code = code || instruction::ALOAD(temp, codAtParam.addr);
    }
    code = code || instruction::PUSH(temp);
  }
  code = code || instruction::CALL(name);

  for (size_t i = 0; i < (ctx->expr()).size(); ++i)
  {
    code = code || instruction::POP();
  }

  std::string temp = "%" + codeCounters.newTEMP();
  code = code || instruction::POP(temp);

  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}
antlrcpp::Any CodeGenVisitor::visitIndex(AslParser::IndexContext *ctx)
{
  DEBUG_ENTER();

  CodeAttribs &&codAtsID = visit(ctx->ident());

  CodeAttribs &&codAtIndex = visit(ctx->expr());

  instructionList &&code = codAtsID.code || codAtIndex.code;

  std::string temp = "%" + codeCounters.newTEMP();
  if (Symbols.isLocalVarClass(codAtsID.addr))
  {
    code = code || instruction::LOADX(temp, codAtsID.addr, codAtIndex.addr);
  }
  else
  {
    std::string temp2 = "%" + codeCounters.newTEMP();
    code = code || instruction::LOAD(temp2, codAtsID.addr) || instruction::LOADX(temp, temp2, codAtIndex.addr);
  }

  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitReadStmt(AslParser::ReadStmtContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs &&codAtsE = visit(ctx->left_expr());
  std::string addr1 = codAtsE.addr;
  std::string offs1 = codAtsE.offs;
  instructionList &code1 = codAtsE.code;
  instructionList &code = code1;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());
  bool &&isArray = ctx->left_expr()->expr();
  std::string temp = isArray ? "%" + codeCounters.newTEMP() : addr1;

  if (Types.isFloatTy(tid1))
    code = code || instruction::READF(temp);
  else if (Types.isCharacterTy(tid1))
    code = code || instruction::READC(temp);
  else
    code = code || instruction::READI(temp);

  if (isArray)
  {
    code = code || instruction::XLOAD(addr1, offs1, temp);
  }
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWriteExpr(AslParser::WriteExprContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs &&codAt1 = visit(ctx->expr());
  std::string addr1 = codAt1.addr;
  // std::string offs1 = codAt1.offs;
  instructionList &code1 = codAt1.code;
  instructionList &code = code1;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->expr());
  if (Types.isFloatTy(tid1))
    code = code || instruction::WRITEF(addr1);
  else if (Types.isCharacterTy(tid1))
    code = code || instruction::WRITEC(addr1);
  else
    code = code || instruction::WRITEI(addr1);

  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitWriteString(AslParser::WriteStringContext *ctx)
{
  DEBUG_ENTER();
  instructionList code;
  std::string s = ctx->STRING()->getText();
  code = code || instruction::WRITES(s);
  DEBUG_EXIT();
  return code;
}

antlrcpp::Any CodeGenVisitor::visitLeft_expr(AslParser::Left_exprContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs &&codAtsID = visit(ctx->ident());
  std::string offset = "";
  instructionList &code = codAtsID.code;
  if (ctx->expr())
  {
    CodeAttribs &&codAtsIX = visit(ctx->expr());
    offset = codAtsIX.addr;
    code = code || codAtsIX.code;

    if (Symbols.isParameterClass(codAtsID.addr))
    {
      std::string temp = "%" + codeCounters.newTEMP();
      code = code || instruction::LOAD(temp, codAtsID.addr);
      codAtsID.addr = temp;
    }
  }
  CodeAttribs codAts(codAtsID.addr, offset, code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitParen(AslParser::ParenContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs &&codAt1 = visit(ctx->expr());
  DEBUG_EXIT();
  return codAt1;
}

antlrcpp::Any CodeGenVisitor::visitArithmetic(AslParser::ArithmeticContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs &&codAt1 = visit(ctx->expr(0));
  std::string addr1 = codAt1.addr;
  instructionList &code1 = codAt1.code;
  CodeAttribs &&codAt2 = visit(ctx->expr(1));
  std::string addr2 = codAt2.addr;
  instructionList &code2 = codAt2.code;
  instructionList &&code = code1 || code2;
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  TypesMgr::TypeId t = getTypeDecor(ctx);
  std::string temp = "%" + codeCounters.newTEMP();
  if (Types.isIntegerTy(t))
  {
    if (ctx->MUL())
      code = code || instruction::MUL(temp, addr1, addr2);
    else if (ctx->PLUS())
      code = code || instruction::ADD(temp, addr1, addr2);
    else if (ctx->MENOS())
      code = code || instruction::SUB(temp, addr1, addr2);
    else if (ctx->DIV())
      code = code || instruction::DIV(temp, addr1, addr2);
    else
      code = code || instruction::DIV(temp, addr1, addr2) || instruction::MUL(temp, temp, addr2) || instruction::SUB(temp, addr1, temp);
  }
  else
  {
    std::string F1 = addr1;
    std::string F2 = addr2;

    if (Types.isIntegerTy(t1))
    {
      F1 = "%" + codeCounters.newTEMP();
      code = code || instruction::FLOAT(F1, addr1);
    }
    else if (Types.isIntegerTy(t2))
    {
      F2 = "%" + codeCounters.newTEMP();
      code = code || instruction::FLOAT(F2, addr2);
    }

    if (ctx->MUL())
      code = code || instruction::FMUL(temp, F1, F2);
    else if (ctx->DIV())
      code = code || instruction::FDIV(temp, F1, F2);
    else if (ctx->PLUS())
      code = code || instruction::FADD(temp, F1, F2);
    else if (ctx->MENOS())
      code = code || instruction::FSUB(temp, F1, F2);
  }
  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitLogic(AslParser::LogicContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs &&codAt1 = visit(ctx->expr(0));
  std::string addr1 = codAt1.addr;
  instructionList &code1 = codAt1.code;
  CodeAttribs &&codAt2 = visit(ctx->expr(1));
  std::string addr2 = codAt2.addr;
  instructionList &code2 = codAt2.code;
  instructionList &&code = code1 || code2;
  // TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  // TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  // TypesMgr::TypeId  t = getTypeDecor(ctx);
  std::string temp = "%" + codeCounters.newTEMP();
  if (ctx->AND())
    code = code || instruction::AND(temp, addr1, addr2);
  else if (ctx->OR())
    code = code || instruction::OR(temp, addr1, addr2);
  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitUnari(AslParser::UnariContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs &&codAt1 = visit(ctx->expr());
  std::string addr1 = codAt1.addr;
  instructionList &code = codAt1.code;
  std::string temp = "%" + codeCounters.newTEMP();

  if (ctx->NOT())
    code = code || instruction::NOT(temp, addr1);
  else if (ctx->MENOS())
  {
    TypesMgr::TypeId t = getTypeDecor(ctx);
    if (Types.isIntegerTy(t))
    {
      code = code || instruction::NEG(temp, addr1);
    }
    else
    {
      std::string F = addr1;
      TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
      if (Types.isIntegerTy(t1))
      {
        F = "%" + codeCounters.newTEMP();
        code = code || instruction::FLOAT(F, addr1);
      }
      code = code || instruction::FNEG(temp, F);
    }
  }
  else
  {
    temp = addr1;
  }
  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitRelational(AslParser::RelationalContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs &&codAt1 = visit(ctx->expr(0));
  std::string addr1 = codAt1.addr;
  instructionList &code1 = codAt1.code;
  CodeAttribs &&codAt2 = visit(ctx->expr(1));
  std::string addr2 = codAt2.addr;
  instructionList &code2 = codAt2.code;
  instructionList &&code = code1 || code2;
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  std::string temp = "%" + codeCounters.newTEMP();
  if (not Types.isFloatTy(t1) and not Types.isFloatTy(t2))
  {
    if (ctx->EQUAL())
    {
      code = code || instruction::EQ(temp, addr1, addr2);
    }
    else if (ctx->NOEQ())
    {
      std::string temp2 = "%" + codeCounters.newTEMP();
      code = code || instruction::EQ(temp2, addr1, addr2);
      code = code || instruction::NOT(temp, temp2);
    }
    else if (ctx->MG())
    {
      code = code || instruction::LT(temp, addr2, addr1);
    }
    else if (ctx->MGE())
    {
      code = code || instruction::LE(temp, addr2, addr1);
    }
    else if (ctx->MP())
    {
      code = code || instruction::LT(temp, addr1, addr2);
    }
    else
    {
      code = code || instruction::LE(temp, addr1, addr2);
    }
  }
  else
  {
    std::string F1 = addr1;
    std::string F2 = addr2;

    if (Types.isIntegerTy(t1))
    {
      F1 = "%" + codeCounters.newTEMP();
      code = code || instruction::FLOAT(F1, addr1);
    }
    else if (Types.isIntegerTy(t2))
    {
      F2 = "%" + codeCounters.newTEMP();
      code = code || instruction::FLOAT(F2, addr2);
    }

    if (ctx->EQUAL())
    {
      code = code || instruction::FEQ(temp, F1, F2);
    }
    else if (ctx->NOEQ())
    {
      std::string temp2 = "%" + codeCounters.newTEMP();
      code = code || instruction::FEQ(temp2, F1, F2);
      code = code || instruction::NOT(temp, temp2);
    }
    else if (ctx->MG())
    {
      code = code || instruction::FLT(temp, F2, F1);
    }
    else if (ctx->MGE())
    {
      code = code || instruction::FLE(temp, F2, F1);
    }
    else if (ctx->MP())
    {
      code = code || instruction::FLT(temp, F1, F2);
    }
    else
    {
      code = code || instruction::FLE(temp, F1, F2);
    }
  }

  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitValue(AslParser::ValueContext *ctx)
{
  DEBUG_ENTER();
  instructionList code;
  std::string temp = "%" + codeCounters.newTEMP();
  if (ctx->INTVAL())
    code = instruction::ILOAD(temp, ctx->getText());
  else if (ctx->FLOATVAL())
    code = instruction::FLOAD(temp, ctx->getText());
  else if (ctx->BOOLVAL())
  {
    std::string temp2 = "%" + codeCounters.newTEMP();
    std::string temp3 = "%" + codeCounters.newTEMP();
    if (ctx->getText() == "true")
    {
      code = instruction::ILOAD(temp2, "1") || instruction::ILOAD(temp3, "1") || instruction::EQ(temp, temp2, temp3);
    }
    else
    {
      code = instruction::ILOAD(temp2, "1") || instruction::ILOAD(temp3, "2") || instruction::EQ(temp, temp2, temp3);
    }
  }

  else
  {
    std::string token = ctx->getText();
    code = instruction::CHLOAD(temp, token.substr(1, token.size() - 2));
  }
  CodeAttribs codAts(temp, "", code);
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitExprIdent(AslParser::ExprIdentContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs &&codAts = visit(ctx->ident());
  DEBUG_EXIT();
  return codAts;
}

antlrcpp::Any CodeGenVisitor::visitIdent(AslParser::IdentContext *ctx)
{
  DEBUG_ENTER();
  CodeAttribs codAts(ctx->ID()->getText(), "", instructionList());
  DEBUG_EXIT();
  return codAts;
}

// Getters for the necessary tree node atributes:
//   Scope and Type
SymTable::ScopeId CodeGenVisitor::getScopeDecor(antlr4::ParserRuleContext *ctx) const
{
  return Decorations.getScope(ctx);
}
TypesMgr::TypeId CodeGenVisitor::getTypeDecor(antlr4::ParserRuleContext *ctx) const
{
  return Decorations.getType(ctx);
}

// Constructors of the class CodeAttribs:
//
CodeGenVisitor::CodeAttribs::CodeAttribs(const std::string &addr,
                                         const std::string &offs,
                                         instructionList &code) : addr{addr}, offs{offs}, code{code}
{
}

CodeGenVisitor::CodeAttribs::CodeAttribs(const std::string &addr,
                                         const std::string &offs,
                                         instructionList &&code) : addr{addr}, offs{offs}, code{code}
{
}
