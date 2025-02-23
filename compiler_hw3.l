/* Definition section */
%{
#include "y.tab.h"	/* header file generated by bison */
#include <stdbool.h>

#define BUF_SIZE 1024

extern YYSTYPE yylval; //YYSTYPE
char buf[BUF_SIZE];
extern void dump_symbol();
bool dump_table = false;
extern int error;
extern char target[BUF_SIZE];
char error_message[BUF_SIZE];
extern int current_level;

void increase_scope();
void decrease_scope();
int line = 0;
/* This macro is used to record code line */
#define CONCAT { strncat(buf, yytext, BUF_SIZE - strlen(buf) - 1); }
%}

/* Define regular expression label */
digit		[0-9]
letter		[a-zA-Z]
integer     {digit}+
float       {digit}+"."{digit}+
id			{letter}+({letter}|{digit})*
%option yylineno
%x COMMENT STRING_STATE
/* Rules section */
%%

 /* Arithmetic */
"+"     { CONCAT; return OP_PLUS; }
"-" 	{ CONCAT; return OP_MINUS; }
"*" 	{ CONCAT; return OP_MUL; }
"/" 	{ CONCAT; return OP_DIV; }
"%"     { CONCAT; return OP_MOD; }
"++"    { CONCAT; return OP_INC; }
"--"    { CONCAT; return OP_DEC; }

 /* Relational */
">"		{ CONCAT; return OP_GT; }
"<"		{ CONCAT; return OP_LT; }
">="	{ CONCAT; return OP_GE; }
"<="	{ CONCAT; return OP_LE; }
"=="	{ CONCAT; return OP_EQ; }
"!="	{ CONCAT; return OP_NE; }

 /* Assignment */
"="		{ CONCAT; return ASSIGN; }
"+="	{ CONCAT; return ADD_ASSIGN; }
"-="	{ CONCAT; return MINUS_ASSIGN; }
"*="	{ CONCAT; return MUL_ASSIGN; }
"/="	{ CONCAT; return DIV_ASSIGN; }
"%="	{ CONCAT; return MOD_ASSIGN; }

 /* Logical */
"&&"	{ CONCAT; return OP_ADD; }
"||"	{ CONCAT; return OP_OR; }
"!"		{ CONCAT; return OP_NOT; }

 /* Delimiters */
"("		{ CONCAT; return LEFT_P; }
")"		{ CONCAT; return RIGHT_P; }
"{"		{ CONCAT; 
          increase_scope();
          return LEFT_CUR;
        }
"}"		{ CONCAT; 
            decrease_scope();
            dump_table = true; 
            return RIGHT_CUR; 
        }
"["		{ CONCAT; return LEFT_BRACET; }
"]"		{ CONCAT; return RIGHT_BRACET; }
","		{ CONCAT; return COMMA; }

 /* Print Keywords */	
"print"		{ CONCAT; return PRINT; }

 /* Condition and Loop Keywords */
"if"		{ CONCAT; return IF; }
"else"		{ CONCAT; return ELSE; }
"for"		{ CONCAT; return FOR; }
"while"		{ CONCAT; return WHILE; }

 /* Declaration Keywords */
"void"		{ CONCAT; yylval.string = strdup(yytext); return VOID; }
"int"		{ CONCAT; yylval.string = strdup(yytext); return INT; }
"float"  	{ CONCAT; yylval.string = strdup(yytext); return FLOAT; }
"string"  	{ CONCAT; yylval.string = strdup(yytext); return STRING; }
"bool"  	{ CONCAT; yylval.string = strdup(yytext); return BOOL; }

 /* boolean Keywords */
"true"      { CONCAT; yylval.string = strdup(yytext); return TRUE; }
"false"     { CONCAT; yylval.string = strdup(yytext); return FALSE; }

"return"    { CONCAT; return RETURN; } 

 /* String Constant */
\"				        { CONCAT; BEGIN STRING_STATE; }
<STRING_STATE>\"		{ CONCAT; BEGIN INITIAL; return STR_CONST; }
<STRING_STATE>[^"]*		{ CONCAT; yylval.string = strdup(yytext); } 


 /* Number Constant */
{integer}	{ CONCAT; yylval.i_val = atoi(yytext); return I_CONST; }
{float}	    { CONCAT; yylval.f_val = atof(yytext); return F_CONST; }

 /* C type Comment */
"/*"				{ CONCAT; BEGIN COMMENT; }
<COMMENT>[^*\n]+	{ CONCAT; }
<COMMENT>\n			{ printf("%d: %s\n",yylineno,buf); bzero(buf,BUF_SIZE); }
<COMMENT>"*"		{ CONCAT; }
<COMMENT>"*/"		{ CONCAT; BEGIN INITIAL;}

 /* C++ type Comment */
\/\/.*	{ CONCAT; }

 /* Variable ID */
 {id}       { CONCAT; yylval.string = strdup(yytext); return Ident; }

 /* others */

[;]         { CONCAT; return SEMICOLON; }
[\n]        { if(buf[0] != '\0') {
                printf("%d: %s\n",yylineno, buf);
              } else {
                printf("%d:\n",yylineno);
              }
              if(error == 1){
                    strcpy(error_message, "Undeclared variable");
              } else if (error == 2){
                    strcpy(error_message, "Undeclared function");
              } else if (error == 3){
                    strcpy(error_message,"Redeclared variable");
              } else if (error == 4){
                    strcpy(error_message, "Redeclared function");
              }
              if(error){
                printf("\n|-----------------------------------------------|\n");
                printf("| Error found in line %d: %s\n", yylineno, buf);
                printf("| %s %s", error_message, target);
                printf("\n|-----------------------------------------------|\n\n");
                exit(-1);
              } else if(dump_table && current_level == 0) {
                 dump_symbol(1+current_level);
                 dump_table = false;
              } 
              error=0;
              bzero(buf,BUF_SIZE);
              bzero(target, BUF_SIZE);
              bzero(error_message, BUF_SIZE);
            }
[ \t]		{ CONCAT; } /* Ignore */
[^ \t\n] 	{ CONCAT; } /* Ignore other charactor sets */
<<EOF>>     { dump_symbol(0);
              printf("\nTotal lines: %d \n",yylineno);
              return 0;
            }

%%

int yywrap()
{
    return 1;
}