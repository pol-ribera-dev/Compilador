//////////////////////////////////////////////////////////////////////
//
//    Asl - Another simple language (grammar)
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

grammar Asl;

//////////////////////////////////////////////////
/// Parser Rules
//////////////////////////////////////////////////

// A program is a list of functions
program : function+ EOF
        ;

// A function has a name, a list of parameters and a list of statements
function
        : FUNC ID '(' params* ')' (':' basic_type)? declarations statements ENDFUNC
        ;
params 
        :ID (COMA ID)* ':' type (','|)
        ;

declarations
        : (variable_decl)*
        ;

variable_decl
        : VAR ID (COMA ID)* ':' type
        ;

type
        : basic_type
        | type_array
        ;

type_array
        : ARRAY CLAUE INTVAL CLAUD 'of' basic_type
        ;

basic_type    
        : INT
        | BOOL
        | CHAR
        | FLOAT
        ;

statements
        : (statement)*
        ;

// The different types of instructions
statement
          // Assignment
        : left_expr ASSIGN expr ';'           # assignStmt
        | WHILE expr DO statements ENDWHILE    # while
          // if-then-else statements (else is optional)
        | IF expr THEN statements (|ELSE statements) ENDIF       # ifStmt
          // A function/procedure call has a list of arguments in parenthesis (possibly empty)
        | ident PARE (expr(','expr)*)? PARD ';'                   # procCall
          // Read a variable
        | READ left_expr ';'                  # readStmt
          // Write an expression
        | WRITE expr ';'                      # writeExpr
          // Write a string
        | WRITE STRING ';'                    # writeString
        | RETURN (|expr)';'                   # return
        ;

// Grammar for left expressions (l-values in C++)
left_expr
        : ident ('[' expr ']')?
        ;

// Grammar for expressions with boolean, relational and aritmetic operators
expr    : op = PARE expr PARD                 # paren 
        | ident CLAUE expr CLAUD              # index
        | ident PARE (expr(','expr)*)? PARD   # fcall
        | op=(NOT|PLUS|MENOS) expr            # unari
        | expr op=(MUL|DIV|MODUL) expr        # arithmetic
        | expr op=(PLUS|MENOS) expr           # arithmetic
        | expr op=(NOEQ|EQUAL|MG|MGE|MP|MPE) expr  # relational
        | expr op=AND expr                    #logic
        | expr op=OR expr                     #logic
        | (INTVAL| BOOLVAL|CHARVAL|FLOATVAL)  # value
        | ident                               # exprIdent
        ;

// Identifiers
ident   : ID
        ;

//////////////////////////////////////////////////
/// Lexer Rules
//////////////////////////////////////////////////

COMA       : ','; 
CLAUE      : '[';
CLAUD      : ']';
PARE       : '(';
PARD       : ')';
NOT        : 'not';
MODUL      : '%';
ASSIGN    : '=' ;
EQUAL     : '==' ;
NOEQ      : '!=';
MG        : '>';
MP        : '<';
MGE       : '>=';
MPE       : '<=';
PLUS      : '+' ;
MENOS     : '-';
MUL       : '*';
DIV       : '/';
AND       : 'and';
OR        : 'or';
VAR       : 'var';
ARRAY     : 'array';
INT       : 'int';
BOOL      : 'bool';
CHAR      : 'char';
FLOAT     : 'float';
WHILE     : 'while';
DO        : 'do';
ENDWHILE  : 'endwhile'; 
RETURN    : 'return';
IF        : 'if' ;
THEN      : 'then' ;
ELSE      : 'else' ;
ENDIF     : 'endif' ;
FUNC      : 'func' ;
ENDFUNC   : 'endfunc' ;
READ      : 'read' ;
WRITE     : 'write' ;
INTVAL    : ('0'..'9')+ ;
BOOLVAL   : ('true'|'false');
FLOATVAL  : ('0'..'9')+ '.' ('0'..'9')+;
CHARVAL   : '\'' (ESC_SEQ|~('\\'|'\'')) '\'';
ID        : ('a'..'z'|'A'..'Z') ('a'..'z'|'A'..'Z'|'_'|'0'..'9')* ;

// Strings (in quotes) with escape sequences
STRING    : '"' ( ESC_SEQ | ~('\\'|'"') )* '"' ;

fragment
ESC_SEQ   : '\\' ('b'|'t'|'n'|'f'|'r'|'"'|'\''|'\\') ;

// Comments (inline C++-style)
COMMENT   : '//' ~('\n'|'\r')* '\r'? '\n' -> skip ;

// White spaces
WS        : (' '|'\t'|'\r'|'\n')+ -> skip ;
// Alternative description
// WS        : [ \t\r\n]+ -> skip ;
