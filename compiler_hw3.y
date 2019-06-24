/*	Definition section */
%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#define BUF_SIZE 1024

extern int yylineno;
extern int yylex();
extern void yyerror(char *s);
extern char* yytext;   // Get current token from lex
extern char buf[BUF_SIZE];  // Get current code line from lex
extern char target[BUF_SIZE];
extern int error;
/* Symbol table function - you can add new function if needed. */
extern int current_level; 
int current_level = 0;
int symIndex = 0;
int counter = 0;
int error = 0;
char target[BUF_SIZE];

char buff[BUF_SIZE];
char p_tmp[BUF_SIZE];
char func_name[BUF_SIZE];
char file_cache[BUF_SIZE][BUF_SIZE];
int file_cache_no = 0;
char type_cache[BUF_SIZE][BUF_SIZE];
int type_no=0;
int branch_no = 0;
int exit_no = 0;
int flag = 0;
int while_tmp = 0;
int while_tmp2 = 0;
int func_no  = 0;
int relation_op = -1;

typedef struct symbolEntry{
    int id;
    char name[BUF_SIZE];
    char type[BUF_SIZE];
    char dataType[BUF_SIZE];
    int scopeLevel;
    char parameters[BUF_SIZE];
} symbolEntry;
symbolEntry symbolTable[BUF_SIZE];
/* Symbol table function - you can add new function if needed. */
int lookup_symbol(char *name);
void insert_symbol(char *name, char *type, char *dataType, char* parameters);
void insert_symbol_level(char *name, char *type, char *dataType, char* parameters, int level);

int exists_symbol(char* name);
void increase_scope();
void decrease_scope();
void dump_symbol();
FILE *file; // To generate .j file for Jasmin
void gen_declaration(char* type, char* ident);
void gen_func_dec(char* type, char* name, char* parameters);
void gen_func_dec_empty(char* type, char* name, char* parameters);
void gen_space();
void gen_initialization(char* type, char* name);
void gen_space();
void gen_assign(char* name, char*  op);
void gen_arithmetic_operator(char* ops);
int is_float(char* id);
int is_int(char* id);
int is_bool(char* id);
int is_str(char* id);
void query_type_from_table(char* id);
int _get_index(char* name, int ind);
void gen_inc_dec(char* id, char* op);
void _load_variable(char* name);
void gen_func_call(char* name);
void gen_func_para(char* name);
int get_index_of_variable(char* name);
int get_argument_length(char* func);
void validate_parameter_type(char* func, int num);

%}

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */

%union {
    int i_val;
    double f_val;
    char* string;
}

%token OP_PLUS OP_MINUS OP_MUL OP_DIV OP_MOD OP_INC OP_DEC
%token OP_GE OP_LE OP_EQ OP_NE  OP_GT OP_LT 
%token <string> ASSIGN ADD_ASSIGN MINUS_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token OP_ADD OP_OR OP_NOT
%token LEFT_P RIGHT_P LEFT_BRACET RIGHT_BRACET LEFT_CUR RIGHT_CUR
%token COMMA
%token PRINT  IF ELSE  FOR WHILE TRUE FALSE RETURN
%token SEMICOLON 

%token <i_val> I_CONST
%token <f_val> F_CONST
%token <string> VOID INT FLOAT BOOL STRING Ident STR_CONST 
%type <string> term TRUE FALSE
%type <string> assign_op
%type <string> f_name

%left  OP_EQ OP_NE
%left  OP_GT OP_LT OP_LE OP_GE
%left  OP_PLUS OP_MINUS
%left  OP_DIV OP_MOD OP_MUL
%left  OP_INC OP_DEC 

/* Nonterminal with return, which need to sepcify type */
/*
%type <f_val> stat
*/
%type <string> type func_declaration func_declaration_list

/* Yacc will start at this nonterminal */
%start program

/* Grammar section */
%%

program  : statement
         | program statement
;


declaration : type Ident SEMICOLON 
    {  
      if(exists_symbol($2)){
          error = 3;
      }
      if(error>0) {
        strcpy(target,$2);
      } else {
        insert_symbol($2,"variable", $1, "");
      }
      gen_declaration($1, $2);
    }
            | type Ident ASSIGN initializer SEMICOLON 
    { 
      if(exists_symbol($2)){
          error = 3;
      }
      if(error>0) {
        strcpy(target,$2);
      } else {
        insert_symbol($2,"variable",$1, "");
      }
      gen_initialization($1, $2);
    }
 
;

statement      : if 
               | while  
               | block 
               | function_decl 
               | assign 
               | function_call 
               | declaration  
               | return 
               | expr_st 
               | print 
;

expr_st        : expr SEMICOLON;
; 

function_call  : func_expr  SEMICOLON
;
if_expr        : IF LEFT_P expr RIGHT_P  {
                  if(relation_op ==  0) {
                     sprintf(file_cache[file_cache_no++], "ifgt BRANCH_%d\n", branch_no);
                  } else if(relation_op ==  1)  {
                    sprintf(file_cache[file_cache_no++], "iflt BRANCH_%d\n", branch_no);
                  } else if(relation_op  == 2) {
                    sprintf(file_cache[file_cache_no++], "ifle BRANCH_%d\n", branch_no);
                  } else if(relation_op  == 3) {
                     sprintf(file_cache[file_cache_no++], "ifge BRANCH_%d\n", branch_no);
                  } else if(relation_op == 4) {
                     sprintf(file_cache[file_cache_no++], "ifeq BRANCH_%d\n", branch_no);
                  } else if(relation_op == 5) {
                     sprintf(file_cache[file_cache_no++], "ifne BRANCH_%d\n", branch_no);
                  }  else {
                    sprintf(file_cache[file_cache_no++], "ifne BRANCH_%d\n", branch_no);
                  } 
                  sprintf(file_cache[file_cache_no++], "goto BRANCH_%d\n", branch_no+1);
                  sprintf(file_cache[file_cache_no++], "BRANCH_%d:\n", branch_no++);
               }
;
if           : if_expr block {
                  flag ++;
                sprintf(file_cache[file_cache_no++], "goto EXIT_%d\n", exit_no);
                sprintf(file_cache[file_cache_no++], "EXIT_%d:\n", exit_no++);
                sprintf(file_cache[file_cache_no++], "BRANCH_%d:\n", branch_no++);
             }
             |  if_expr block else_stat {
                flag ++;
             } 
;
el_ex         : ELSE {
                 sprintf(file_cache[file_cache_no++], "goto EXIT_%d\n", exit_no);
                 sprintf(file_cache[file_cache_no++], "BRANCH_%d:\n", branch_no++);
}
else_stat     : el_ex statement {
               if(!flag) {
                  sprintf(file_cache[file_cache_no++], "goto EXIT_%d\n", exit_no);
                  sprintf(file_cache[file_cache_no++], "EXIT_%d:\n", exit_no++);
                  branch_no++;
               } else {
                 flag --;
               }
            } 
;

wil          : WHILE {
             sprintf(file_cache[file_cache_no++], "BRANCH_%d:\n", branch_no++);
}            
;
w_exp        : wil LEFT_P expr RIGHT_P {
                if(relation_op ==  0) {
                     sprintf(file_cache[file_cache_no++], "ifgt BRANCH_%d\n", branch_no);
                  } else if(relation_op ==  1)  {
                    sprintf(file_cache[file_cache_no++], "iflt BRANCH_%d\n", branch_no);
                  } else if(relation_op  == 2) {
                    sprintf(file_cache[file_cache_no++], "ifle BRANCH_%d\n", branch_no);
                  } else if(relation_op  == 3) {
                     sprintf(file_cache[file_cache_no++], "ifge BRANCH_%d\n", branch_no);
                  } else if(relation_op == 4) {
                     sprintf(file_cache[file_cache_no++], "ifeq BRANCH_%d\n", branch_no);
                  } else if(relation_op == 5) {
                     sprintf(file_cache[file_cache_no++], "ifne BRANCH_%d\n", branch_no);
                  }  else {
                    sprintf(file_cache[file_cache_no++], "ifne BRANCH_%d\n", branch_no);
                  } 
               sprintf(file_cache[file_cache_no++], "goto EXIT_%d\n", exit_no++);  
               sprintf(file_cache[file_cache_no++], "BRANCH_%d:\n", branch_no++);
               while_tmp++;
               while_tmp2++;
             }
;
while        : w_exp block {
              while_tmp--;
              sprintf(file_cache[file_cache_no++], "goto BRANCH_%d\n", branch_no-(2*(while_tmp2-while_tmp)));
              sprintf(file_cache[file_cache_no++], "EXIT_%d:\n", (exit_no) - (while_tmp2 - while_tmp));
              if(while_tmp == 0) {
                while_tmp2 = 0;
              }
             }
;

function_decl   : type Ident LEFT_P func_declaration_list RIGHT_P SEMICOLON {
                gen_func_dec_empty($1, $2, $4);
                bzero(p_tmp, BUF_SIZE);
}
                | type Ident LEFT_P func_declaration_list RIGHT_P block
    { 
      if(error == 0) {
          if(lookup_symbol($2)){
              error = 4;
          }
      }
      if(error>0) {
        strcpy(target,$2);
      }else {
        insert_symbol($2, "function", $1, $4);  
      }

      gen_func_dec($1, $2, $4);
      bzero(p_tmp, BUF_SIZE);


    }
                | type Ident LEFT_P RIGHT_P SEMICOLON
    {
          gen_func_dec_empty($1, $2, "");
    }
                | type Ident LEFT_P RIGHT_P block
    {       
       if(error == 0) {
          if(lookup_symbol($2)){
              error = 4;
          }
      }
      if(error>0) {
        strcpy(target,$2);
      }else {
        insert_symbol($2, "function", $1, "");  
      }
      gen_func_dec($1, $2, "");

    }
;

return     : RETURN SEMICOLON 
           | RETURN expr SEMICOLON
           | RETURN assign_expr SEMICOLON 
;
    
parameter_list
    : parameter_list COMMA expr {
      func_no++;
      validate_parameter_type(func_name, func_no);
    }
    | expr {
      func_no++;
      validate_parameter_type(func_name, func_no);
    }
;

func_declaration_list       :   func_declaration {
    sprintf(p_tmp, "%s", $1);
    $$ = $1;
}
                            |   func_declaration_list COMMA func_declaration
    {
      sprintf(p_tmp, "%s%s", $$, $3);
      $$ = p_tmp;
    }

;

func_declaration    : type Ident
    {
      $$ = $1;
      insert_symbol_level($2,  "parameter", $1, "", current_level+1);
    }
;

block        : LEFT_CUR RIGHT_CUR 
             | LEFT_CUR stat_list RIGHT_CUR 
;

stat_list    : statement
             | stat_list statement
;

assign_expr  : Ident assign_op expr {
       if(!lookup_symbol($1)){
          error = 1;
      }
      if(error>0) {
        strcpy(target,$1);
      }
      gen_assign($1, $2);
}
 
;

assign   : assign_expr SEMICOLON 
         | Ident OP_INC SEMICOLON
    { 
      if(!lookup_symbol($1)){
          error = 1;
      }
      if(error>0) {
        strcpy(target,$1);
      }
      gen_inc_dec($1, "inc");
    }
               | Ident OP_DEC SEMICOLON 
    { 
      if(!lookup_symbol($1)){
          error = 1;
      }
      if(error>0) {
        strcpy(target,$1);
      }
      gen_inc_dec($1, "dec");
    }

;

assign_op     : ASSIGN  {
                 $$ = "=";
              }
              | ADD_ASSIGN {
                $$ = "+=";
              } 
              | MINUS_ASSIGN  {
                $$ = "-=";
              }
              | MUL_ASSIGN {
                $$ = "*=";
              }
              | DIV_ASSIGN {
                $$ = "/=";
              }
              | MOD_ASSIGN {
                $$ = "%=";
              }
;

initializer    : expr
;

expr         : expr OP_PLUS expr  {
                gen_arithmetic_operator("+");
             }
             | expr OP_MINUS expr {
                gen_arithmetic_operator("-");
             }
             | expr OP_MUL expr {
                gen_arithmetic_operator("*");
             }
             | expr OP_DIV expr {
                gen_arithmetic_operator("/");
             }
             | expr OP_MOD expr {
                gen_arithmetic_operator("%");
             }
             | expr OP_GT expr  { 
               if(strcmp(type_cache[type_no-1], "F") == 0) {
                  sprintf(file_cache[file_cache_no++], "fsub\n");
               } else {
                 sprintf(file_cache[file_cache_no++], "isub\n");
               }
               strcpy(type_cache[type_no++], "Z");
               relation_op = 0;
               
             }
             | expr OP_LT expr {
               if(strcmp(type_cache[type_no-1], "F") == 0) {
                  sprintf(file_cache[file_cache_no++], "fsub\n");
               } else {
                 sprintf(file_cache[file_cache_no++], "isub\n");
               }
               strcpy(type_cache[type_no++], "Z");
               relation_op = 1;
             }
             | expr OP_LE expr {
                if(strcmp(type_cache[type_no-1], "F") == 0) {
                  sprintf(file_cache[file_cache_no++], "fsub\n");
                } else {
                 sprintf(file_cache[file_cache_no++], "isub\n");
                }
               strcpy(type_cache[type_no++], "Z");
               relation_op = 2;
             }
             | expr OP_GE expr {
                if(strcmp(type_cache[type_no-1], "F") == 0) {
                  sprintf(file_cache[file_cache_no++], "fsub\n");
                 } else {
                  sprintf(file_cache[file_cache_no++], "isub\n");
                 }             
                 strcpy(type_cache[type_no++], "Z");
                 relation_op = 3;
              }
             | expr OP_EQ expr {
                if(strcmp(type_cache[type_no-1], "F") == 0) {
                  sprintf(file_cache[file_cache_no++], "fsub\n");
                } else {
                 sprintf(file_cache[file_cache_no++], "isub\n");
                }
                strcpy(type_cache[type_no++], "Z");
                relation_op = 4;
              }
             | expr OP_NE expr {
                if(strcmp(type_cache[type_no-1], "F") == 0) {
                  sprintf(file_cache[file_cache_no++], "fsub\n");
                } else {
                 sprintf(file_cache[file_cache_no++], "isub\n");
                }
                strcpy(type_cache[type_no++], "Z");
                relation_op = 5;
             }
             | func_expr 
             | Ident OP_INC 
    { 
      if(error == 0) {
          if(!lookup_symbol($1)){
              error = 1;
          }
      }
      if(error>0) {
        strcpy(target,$1);
      }
            gen_inc_dec($1, "inc");
    }
               | Ident OP_DEC
    { 
      if(error == 0) {
          if(!lookup_symbol($1)){
              error = 1;
          }
      }
      if(error>0) {
        strcpy(target,$1);
      }
      gen_inc_dec($1, "dec");
    }
                | OP_PLUS  expr
                | OP_MINUS expr {
                   if(strcmp(type_cache[type_no-1], "F") == 0) {
                  sprintf(file_cache[file_cache_no++], "ldc -1.0\n");
                  sprintf(file_cache[file_cache_no++], "fmul\n");
               } else {
                 sprintf(file_cache[file_cache_no++], "ldc -1\n");
                 sprintf(file_cache[file_cache_no++], "imul\n");
               }
               strcpy(type_cache[type_no], type_cache[type_no-1]);
               type_no++;
             }
                | term
                | LEFT_P expr RIGHT_P
;

f_name : Ident LEFT_P {
        strcpy(func_name,  $1);
}
;
func_expr       : Ident LEFT_P RIGHT_P
    {
     if(error == 0) {
          if(!lookup_symbol($1)){
              error = 2;
          }
      }
      if(error>0) {
        strcpy(target,$1); 
        }

      gen_func_call($1);
      int expected_len = get_argument_length($1);
      if(expected_len != 0) {
        yyerror("The number of actual parameters must be identical to the function declaration.");
      }
    }

              | f_name parameter_list RIGHT_P 
    {
      if(error == 0) {
          if(!lookup_symbol($1)){
              error = 2;
          }
      }
      if(error>0) {
        strcpy(target,$1); 
      }

      gen_func_call($1);
      int expected_len = get_argument_length($1);
      if(expected_len != func_no) {
        yyerror("The number of actual parameters must be identical to the function declaration.");
      }
      func_no = 0;
      bzero(func_name, BUF_SIZE);
   }
;
print   : PRINT LEFT_P Ident RIGHT_P SEMICOLON 
    { 
      if(error == 0){
          if(!lookup_symbol($3)){
              error = 1;
          }
      }  
      if(error>0) {
        strcpy(target,$3);
      }
      _load_variable($3);
      sprintf(file_cache[file_cache_no++], "getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\n");
      if(is_float($3)) {
        sprintf(file_cache[file_cache_no++], "invokevirtual java/io/PrintStream/println(F)V\n");
      } else if(is_int($3)) {
        sprintf(file_cache[file_cache_no++], "invokevirtual java/io/PrintStream/println(I)V\n");
      } else {
        sprintf(file_cache[file_cache_no++], "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
      }
    }
        | PRINT LEFT_P STR_CONST RIGHT_P SEMICOLON 
        {
          sprintf(file_cache[file_cache_no++], "ldc \"%s\"\n", $3);
          sprintf(file_cache[file_cache_no++], "getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\n");
          sprintf(file_cache[file_cache_no++], "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
        }
        | PRINT LEFT_P I_CONST RIGHT_P SEMICOLON
        {
          sprintf(file_cache[file_cache_no++], "ldc %d\n", $3);
          sprintf(file_cache[file_cache_no++], "getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\n");
          sprintf(file_cache[file_cache_no++], "invokevirtual java/io/PrintStream/println(I)V\n");
        }
        | PRINT LEFT_P F_CONST RIGHT_P SEMICOLON
        {
          sprintf(file_cache[file_cache_no++], "ldc %f\n", $3);
          sprintf(file_cache[file_cache_no++], "getstatic java/lang/System/out Ljava/io/PrintStream;\nswap\n");
          sprintf(file_cache[file_cache_no++], "invokevirtual java/io/PrintStream/println(F)V\n");

        }
;

term    : F_CONST {
          if(current_level == 0) {
            sprintf(buff, "%f" , $1);
          } else {
            strcpy(type_cache[type_no++], "F");
            sprintf(file_cache[file_cache_no++], "ldc %.1f\n", $1); 
          }
        }
        | I_CONST  {
          if(current_level == 0) {
            sprintf(buff, "%d" , $1);
          } else {
            strcpy(type_cache[type_no++], "I");
            sprintf(file_cache[file_cache_no++], "ldc %d\n", $1); 
          }
        }
        | STR_CONST  {
          if(current_level == 0) {
             sprintf(buff, "%s" , $1);
          } else {
             strcpy(type_cache[type_no++], "Ljava/lang/String;");
             sprintf(file_cache[file_cache_no++], "ldc \"%s\"\n", $1); 
          }
        }
        | TRUE {
          if(current_level == 0) {
            sprintf(buff, "1");
          } else {
            strcpy(type_cache[type_no++], "Z");
            sprintf(file_cache[file_cache_no++], "ldc 1\n"); 

          }
        }
        | FALSE  {
          if(current_level == 0) {
            sprintf(buff, "0");
          } else {
            strcpy(type_cache[type_no++], "Z");
            sprintf(file_cache[file_cache_no++], "ldc 0\n"); 
          }
        }
        | Ident {
          if(current_level == 0) {
            sprintf(buff, "%s" , $1);
          } else  {
            _load_variable($1);
          }
          $$ = $1;
        }
;

/* actions can be taken when meet the token or rule */
type    : INT {
          $$ = "I";
        }
        | FLOAT {
          $$ = "F";
        }
        | BOOL  {
          $$ = "Z";
        }
        | STRING {
          $$ = "Ljava/lang/String;";
        }
        | VOID  {
          $$ = "V";
        }
;


%%

/* C code section */
int main(int argc, char** argv)
{
    yylineno = 0;

    file = fopen("compiler_hw3.j","w");
    fprintf(file,   ".class public compiler_hw3\n"
                    ".super java/lang/Object\n");
    yyparse();

    fclose(file);

    return 0;
}

void yyerror(char *s)
{
    printf("%d: %s\n",yylineno, buf);
    printf("\n|-----------------------------------------------|\n");
    printf("| Error found in line %d: %s\n", yylineno, buf);
    printf("| %s", s);
    printf("\n|-----------------------------------------------|\n\n");
    exit(-1);
}
void insert_symbol_level(char *name, char *type, char *dataType, char* parameters, int level) {
    symIndex++;
        counter++;

  
    strcpy(symbolTable[symIndex].name, name);
    strcpy(symbolTable[symIndex].type, type);
    strcpy(symbolTable[symIndex].dataType, dataType);
    strcpy(symbolTable[symIndex].parameters, parameters);
    symbolTable[symIndex].id = counter;
    symbolTable[symIndex].scopeLevel = level;

 }
void insert_symbol(char *name, char *type, char *dataType, char* parameters) {
    symIndex++;
    counter++;
    strcpy(symbolTable[symIndex].name, name);
    strcpy(symbolTable[symIndex].type, type);
    strcpy(symbolTable[symIndex].dataType, dataType);
    strcpy(symbolTable[symIndex].parameters, parameters);
    symbolTable[symIndex].id = counter;
    symbolTable[symIndex].scopeLevel = current_level;

 }
void increase_scope(){
    current_level++;
}
void decrease_scope(){
    current_level--;
    counter = 0;
}
void gen_space() {
  for(int i = 0; i <  current_level; i++) {
      fprintf(file, "  ");
  }
}
int lookup_symbol(char* name) {
    for(int i= 0; i < 1024; i++){
        if(symbolTable[i].scopeLevel <= current_level && strcmp(symbolTable[i].name, name) == 0){
            return 1;
        }
    }
    return 0;
}
//exists in current level
int exists_symbol(char* name){
  for(int i= 0; i < 1024; i++){
        if(symbolTable[i].scopeLevel == current_level && strcmp(symbolTable[i].name, name) == 0){
            return 1;
        }
    }
    return 0;
}

void _load_variable(char* name) {
  query_type_from_table(name);
  int ind = get_index_of_variable(name);
  if(ind != -1) {
     if(is_float(name)){
        sprintf(file_cache[file_cache_no++], "fload %d\n", ind); 
     } else if(is_int(name)){
        sprintf(file_cache[file_cache_no++], "iload %d\n", ind); 
    } else if(is_bool(name)) {
        sprintf(file_cache[file_cache_no++], "iload %d\n", ind); 
    } else if(is_str(name)){
        sprintf(file_cache[file_cache_no++], "aload %d\n", ind); 
    }
 } else {
    sprintf(file_cache[file_cache_no++], "getstatic  compiler_hw3/%s %s\n", name, type_cache[type_no-1]);
  }
}

void gen_inc_dec(char* name, char* op) {
  if(strcmp(op, "inc") == 0) {
    if(is_float(name)) {
      sprintf(file_cache[file_cache_no++], "ldc 1.0\n");
    } else if(is_int(name)){
     sprintf(file_cache[file_cache_no++], "ldc 1\n");
    } else {
      yyerror("wrong type for inc operator");
    }
  } else {
    if(is_float(name)) {
      sprintf(file_cache[file_cache_no++], "ldc -1.0\n");
    } else if(is_int(name)){
      sprintf(file_cache[file_cache_no++], "ldc -1\n");
    } else {
      yyerror("wrong type for dec operator");
    }
  }
  _load_variable(name);
  int ind = get_index_of_variable(name);
  if(strcmp(type_cache[type_no-1], "F") == 0) {
    sprintf(file_cache[file_cache_no++], "fadd \n");
    sprintf(file_cache[file_cache_no++], "fstore %d\n", ind);
  } else if (strcmp(type_cache[type_no-1], "I") == 0){
     sprintf(file_cache[file_cache_no++], "iadd \n");
     sprintf(file_cache[file_cache_no++], "istore %d\n", ind);
  } 
}
 

 void gen_declaration(char* type, char* id){
   if(current_level == 0){
     fprintf(file, ".field public static %s %s\n", id, type);
   } else {
     int ind = get_index_of_variable(id);
     if(strcmp(type, "F") == 0) {
       strcpy(file_cache[file_cache_no++], "ldc 0.0\n");
       sprintf(file_cache[file_cache_no++], "fstore %d\n", ind); 
     }  else if(strcmp(type, "I") == 0 ){
       strcpy(file_cache[file_cache_no++], "ldc 0\n");
       sprintf(file_cache[file_cache_no++], "istore %d\n", ind); 
     } else if(strcmp(type, "Z") == 0){
       strcpy(file_cache[file_cache_no++], "ldc 0\n");
       sprintf(file_cache[file_cache_no++], "istore %d\n", ind); 
     }else if(strcmp(type, "Ljava/lang/String;") == 0) {
       strcpy(file_cache[file_cache_no++], "ldc \"\"\n");
       sprintf(file_cache[file_cache_no++], "istore %d\n", ind);
     }
   }
 }

 
 void gen_func_dec(char* type, char* name, char* parameters){
   if(strcmp(name, "main") == 0)  {
     fprintf(file, ".method public static main([Ljava/lang/String;)V\n");
     fprintf(file, ".limit stack 50\n");
     fprintf(file, ".limit locals 50\n");
   } else {
     if(strcmp(parameters, "") == 0) {
         fprintf(file, ".method public static %s()%s\n", name, type);
 
     } else {
         fprintf(file, ".method public static %s(%s)%s\n", name, parameters,  type);
     }
             fprintf(file, ".limit stack 50\n");
         fprintf(file, ".limit locals 50\n");
  }
  if(strcmp(type, "F") ==0  && strcmp(type_cache[type_no-1], "F") != 0 ){
      yyerror("The type of the return statement of the generated code must match the return type of the function declaration.");
  } else if( strcmp(type, "I") ==0 && strcmp(type_cache[type_no-1], "I") != 0 ){
      yyerror("The type of the return statement of the generated code must match the return type of the function declaration.");
  } else if(strcmp(type, "Z") == 0 && strcmp(type_cache[type_no-1], "Z") != 0) {
    yyerror("The type of the return statement of the generated code must match the return type of the function declaration.");
  } else if(strcmp(type, "Ljava/lang/String;") == 0 && strcmp(type_cache[type_no-1], "Ljava/lang/String;") != 0) {
    yyerror("The type of the return statement of the generated code must match the return type of the function declaration.");
  }
 
  for(int i = 0; i < file_cache_no; i++) {
    fprintf(file, "%s", file_cache[i]);
    bzero(file_cache[i], BUF_SIZE);
  }
  file_cache_no = 0;
  if(strcmp(type, "I") == 0 && strcmp(type_cache[type_no-1], "F") ==0){
    fprintf(file, "f2i\n");
  } else if(strcmp(type, "F") == 0 && strcmp(type_cache[type_no-1], "I") ==0) {
    fprintf(file, "i2f\n");
  }
  if(strcmp(type, "F") == 0) {
    fprintf(file, "freturn\n");
  } else if(strcmp(type, "I")  == 0)  {
    fprintf(file, "ireturn\n");
  } else if(strcmp(type, "Z") == 0){
    fprintf(file, "ireturn\n");
  } else {
    fprintf(file, "return\n");
  }
  fprintf(file, ".end method\n");
 }

 void gen_func_dec_empty(char* type, char* name, char* parameters){
     if(strcmp(parameters, "") == 0) {
         fprintf(file, ".method public static %s()%s\n", name, type);
     } else {
         fprintf(file, ".method public static %s(%s)%s\n", name, parameters,  type);
     } 
 }

 void gen_initialization(char* type, char* id) {
   if(current_level == 0) {
     fprintf(file, ".field public static %s %s = %s\n", id, type, buff);
     bzero(buff, BUF_SIZE);
   } else {
     gen_assign(id, "=");
   }
 }


 void gen_func_call(char* name) {
    sprintf(file_cache[file_cache_no++], "invokestatic compiler_hw3/%s", name);
    gen_func_para(name);
    query_type_from_table(name);
    sprintf(file_cache[file_cache_no++], "%s\n", type_cache[type_no-1]);
 }

 void gen_func_para(char* name) {
    for(int i= 0; i < 1024; i++){
        if(symbolTable[i].scopeLevel == 0 && 
        strcmp(symbolTable[i].name,"") != 0  ){ 
          if(strcmp(name, symbolTable[i].name) == 0) {
            sprintf(file_cache[file_cache_no++], "(%s)", symbolTable[i].parameters);
            return;
          } 
        }
    } 
 }

 void gen_assign(char* id, char* ops) {
   int ind = get_index_of_variable(id);
   int exp_float = 0;
   if(strcmp(type_cache[type_no-1], "F") == 0){
      exp_float = 1;
   } 
   query_type_from_table(id);
   if(strcmp(ops, "=") != 0) {
     if(is_float(id)) {
        if(!exp_float) {
          sprintf(file_cache[file_cache_no++], "i2f\n");
          exp_float = 1;
        }
        sprintf(file_cache[file_cache_no++], "fload %d\n", ind); 
     } else if(is_int(id)) {
        sprintf(file_cache[file_cache_no++], "iload %d\n", ind); 
        if(exp_float) {
          sprintf(file_cache[file_cache_no++], "i2f\n");
        }
     }
   }
   if(strcmp(ops, "+=") == 0){ 
     if(exp_float || is_float(id)){
        sprintf(file_cache[file_cache_no++], "fadd\n");
     } else  {
        sprintf(file_cache[file_cache_no++], "iadd\n");
     }
  } else if(strcmp(ops, "-=") == 0) {
     if(exp_float || is_float(id)) {
        sprintf(file_cache[file_cache_no++], "fsub\n");
     } else{
        sprintf(file_cache[file_cache_no++], "isub\n");
     }
 
   } else if(strcmp(ops, "*=") == 0) {
     if(exp_float || is_float(id)) {
        sprintf(file_cache[file_cache_no++], "fmul\n");
     } else{
        sprintf(file_cache[file_cache_no++], "imul\n");
     }
 
   }  else if(strcmp(ops, "/=") == 0) {
     if(exp_float || is_float(id)) {
        if(strcmp(file_cache[file_cache_no-3], "ldc 0\n") == 0 || 
         strcmp(file_cache[file_cache_no-2], "ldc 0.0\n") == 0
        ){
          yyerror("Variables of numbers can not be divided by zero.");
        }
        sprintf(file_cache[file_cache_no++], "fdiv\n");
     } else{
        if(strcmp(file_cache[file_cache_no-2], "ldc 0\n") == 0){
          yyerror("Variables of numbers can not be divided by zero.");
        }
        sprintf(file_cache[file_cache_no++], "idiv\n");
     }
 
   } else if(strcmp(ops, "%=") == 0) {
     if(is_float(id)) {
        yyerror("Modulo operator (%) can not be with floating point operands.");
     } else if(is_int(id)) {
        sprintf(file_cache[file_cache_no++], "irem\n");
     }
 
   }

   if(exp_float == 1 && is_int(id)) {
     sprintf(file_cache[file_cache_no++], "f2i\n");
   } else if(exp_float == 0 && is_float(id)) {
      sprintf(file_cache[file_cache_no++], "i2f\n");
   }

   if(is_float(id)) {
     sprintf(file_cache[file_cache_no++], "fstore %d\n", ind); 
   } else if(is_int(id)){
      sprintf(file_cache[file_cache_no++], "istore %d\n", ind); 
   } else if(is_bool(id)){
      sprintf(file_cache[file_cache_no++], "istore %d\n", ind); 
   } else if(is_str(id)){
      sprintf(file_cache[file_cache_no++], "astore %d\n", ind); 
   }
 }

void dump_symbol(int level) {
    printf("\n%-10s%-10s%-12s%-10s%-10s%-10s\n\n",
"Index", "Name", "Kind", "Type", "Scope", "Attribute");
   int counter = 0;
   for(int i= 0; i < 1024; i++){
        if(symbolTable[i].scopeLevel == level && 
        strcmp(symbolTable[i].name,"") != 0  ){ 
          if(strcmp(symbolTable[i].parameters, "") == 0){
            printf("%-10d%-10s%-12s%-10s%-10d\n",
             counter++, symbolTable[i].name, symbolTable[i].type,
             symbolTable[i].dataType, symbolTable[i].scopeLevel);
             strcpy(symbolTable[i].name,"");
          } else {
             printf("%-10d%-10s%-12s%-10s%-10d%s\n",
             counter++, symbolTable[i].name, symbolTable[i].type,
             symbolTable[i].dataType, symbolTable[i].scopeLevel, symbolTable[i].parameters);
             strcpy(symbolTable[i].name,""); 
          }
        }
   }
   printf("\n");
 } 


 void query_type_from_table(char* variable) {
   int l = current_level;
   while(l >= 0){
    for(int i= 0; i < 1024; i++){
        if(symbolTable[i].scopeLevel == l && 
        strcmp(symbolTable[i].name,"") != 0  ){ 
          if(strcmp(variable, symbolTable[i].name) == 0) {
            strcpy(type_cache[type_no++], symbolTable[i].dataType);
            return;
          } 
        }
    } 
    l--;
   }
 }

void validate_parameter_type(char* func, int num) {
   for(int i= 0; i < 1024; i++){
     if(symbolTable[i].scopeLevel == 0 && 
     strcmp(symbolTable[i].name,"") != 0  ){ 
        if(strcmp(func, symbolTable[i].name) == 0) {
           int j = 0;
           int counter = 0;
           while(symbolTable[i].parameters[j] != '\0') {
              if(symbolTable[i].parameters[j] != ',') {
                counter++;
                if(counter == num){
                  if(symbolTable[i].parameters[j] != type_cache[type_no-1][0]){
                      yyerror("The types of the actual parameters must be identical to the formal parameters in the function declaration.");
                  }
                  return;
                }
              } 
              j++;
           }
        }
      } 
  } 
}
int is_float(char* variable) {
  int l  = current_level;
  while(l != 0) {
    for(int i= 0; i < 1024; i++){
        if(symbolTable[i].scopeLevel == l && 
        strcmp(symbolTable[i].name,"") != 0  ){ 
          if(strcmp(variable, symbolTable[i].name) == 0) {
            if(strcmp(symbolTable[i].dataType, "F") == 0) {
              if(l == 0) {
                return -1;
              } else {
                return 1;
              }
            } else {
              return 0;
            }
          } 
        }
    }   
    l --;
  }
   return -100;
}

int is_int(char* variable) {
    int l  = current_level;
    while(l != 0) {
      for(int i= 0; i < 1024; i++){
        if(symbolTable[i].scopeLevel == l && 
        strcmp(symbolTable[i].name,"") != 0  ){ 
          if(strcmp(variable, symbolTable[i].name) == 0) {
            if(strcmp(symbolTable[i].dataType, "I") == 0) {
              if(l == 0) {
                return -1;
              } else {
                return 1;
              }
            } else {
              return 0;
            }
          } 
        }
     } 
     l--;
  }
   return -100;
}


int is_bool(char* variable) {
    int l  = current_level;
    while(l != 0) {
      for(int i= 0; i < 1024; i++){
        if(symbolTable[i].scopeLevel == l && 
        strcmp(symbolTable[i].name,"") != 0  ){ 
          if(strcmp(variable, symbolTable[i].name) == 0) {
            if(strcmp(symbolTable[i].dataType, "Z") == 0) {
              if(l == 0) {
                return -1;
              } else {
                return 1;
              }
            } else {
              return 0;
            }
          } 
        }
     } 
     l--;
  }
   return -100;
}


int is_str(char* variable) {
    int l  = current_level;
    while(l != 0) {
      for(int i= 0; i < 1024; i++){
        if(symbolTable[i].scopeLevel == l && 
        strcmp(symbolTable[i].name,"") != 0  ){ 
          if(strcmp(variable, symbolTable[i].name) == 0) {
            if(strcmp(symbolTable[i].dataType, "Ljava/lang/String;") == 0) {
              if(l == 0) {
                return -1;
              } else {
                return 1;
              }
            } else {
              return 0;
            }
          } 
        }
     } 
     l--;
  }
   return -100;
}

int get_argument_length(char* func) {
  for(int i= 0; i < 1024; i++){
     if(symbolTable[i].scopeLevel == 0 && 
     strcmp(symbolTable[i].name,"") != 0  ){ 
        if(strcmp(func, symbolTable[i].name) == 0) {
           int j = 0;
           int counter = 0;
           if(strcmp(symbolTable[i].parameters,"") == 0 ) {
             return counter;
           } 
           while(symbolTable[i].parameters[j] != '\0') {
              if(symbolTable[i].parameters[j] == ',') {
                counter ++;
              }
              j++;
           }
           return counter+1;
        }
      } 
  }
  return -1;
}
int get_index_of_variable(char* variable){
  int l = current_level;
  int ind  = -1;
  while(l >= 0)  { 
     ind = _get_index(variable, l);
     if(ind >= 0)  {
       if(l == 0)  {
         return -1;
       } else {
          return ind;
       }
     } 
     l--;
  }
  return -100;
}

int _get_index(char* variable, int level)  {
   int counter = 0;
  for(int i= 0; i < 1024; i++){
        if(symbolTable[i].scopeLevel == level && 
        strcmp(symbolTable[i].name,"") != 0  ){ 
          if(strcmp(variable, symbolTable[i].name) == 0) {
            return counter;
          } 
          counter ++;
        }
   }
   return -1; 
}

void gen_arithmetic_operator(char* ops) {
  int is_f = 0;
  if(strcmp(type_cache[type_no-1], "F") == 0 && strcmp(type_cache[type_no-2], "I") == 0) {
         sprintf(file_cache[file_cache_no++], "%s", "f2i\n");
  } else if(strcmp(type_cache[type_no-1], "I") == 0 && strcmp(type_cache[type_no-2], "F") == 0) {
         sprintf(file_cache[file_cache_no++], "%s", "i2f\n");
         is_f =  1;
  } else if(strcmp(type_cache[type_no-1], "F") == 0 || strcmp(type_cache[type_no-2], "F") == 0){
         is_f = 1;
  }
  if(is_f) {
    sprintf(type_cache[type_no++], "F");
  } else {
    sprintf(type_cache[type_no++], "I");
  }
  if(strcmp(ops, "+") == 0) {
    if(is_f) {
       sprintf(file_cache[file_cache_no++], "fadd\n");
    } else {
       sprintf(file_cache[file_cache_no++], "iadd\n");
    }
  } else   if(strcmp(ops, "-") == 0) {
    if(is_f) {
      sprintf(file_cache[file_cache_no++], "fsub\n");
    } else {
      sprintf(file_cache[file_cache_no++], "isub\n");
    }
  } else   if(strcmp(ops, "*") == 0) {
    if(is_f) {
      sprintf(file_cache[file_cache_no++], "fmul\n");
    } else {
      sprintf(file_cache[file_cache_no++], "imul\n");
    }
  } else  if(strcmp(ops, "/") == 0) {
    if(is_f) {
       if(strcmp(file_cache[file_cache_no-2], "ldc 0\n") == 0 || 
         strcmp(file_cache[file_cache_no-1], "ldc 0.0\n") == 0
        ){
          yyerror("Variables of numbers can not be divided by zero.");
        }
      sprintf(file_cache[file_cache_no++], "fdiv\n");
    } else {
       if(strcmp(file_cache[file_cache_no-1], "ldc 0\n") == 0){
          yyerror("Variables of numbers can not be divided by zero.");
        }
      sprintf(file_cache[file_cache_no++], "idiv\n");
    }
  } else if(strcmp(ops, "%")  == 0) {
    if(is_f) {
        yyerror("Modulo operator (%) can not be with floating point operands.");
    } else {
       sprintf(file_cache[file_cache_no++], "irem\n");
    }
  }


}