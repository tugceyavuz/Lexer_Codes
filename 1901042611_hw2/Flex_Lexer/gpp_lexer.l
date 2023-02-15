%{
    #include<stdio.h>

    char* divide(char *str){
      char *result = str+1; // removes first character
      result[strlen(result)-1] = '\0'; // removes last character
      return result;
    }

%}

%option noyywrap 

%%

[ \n\t\r]+ {}
";;".* {printf("COMMENT\n");}
and {printf("%s: KW_AND\n",yytext);}
or {printf("%s: KW_OR\n",yytext);}
not {printf("%s: KW_NOT\n",yytext);}
equal {printf("%s: KW_EQUAL\n",yytext);}
less {printf("%s: KW_LESS\n",yytext);}
nil {printf("%s: KW_NIL\n",yytext);}
list {printf("%s: KW_LIST\n",yytext);}
append {printf("%s: KW_APPEND\n",yytext);}
concat {printf("%s: KW_CONCAT\n",yytext);}
set {printf("%s: KW_SET\n",yytext);}
deffun {printf("%s: KW_DEFFUN\n",yytext);}
for {printf("%s: KW_FOR\n",yytext);}
if {printf("%s: KW_IF\n",yytext);}
exit {printf("%s: KW_EXIT\n",yytext);}
load {printf("%s: KW_LOAD\n",yytext);}
disp {printf("%s: KW_DISP\n",yytext);}
true {printf("%s: KW_TRUE\n",yytext);}
false {printf("%s: KW_TRUE\n",yytext);}

[(] {printf("%s: OP_OP\n",yytext);}
[)] {printf("%s: OP_CP\n",yytext);}
[+] {printf("%s: OP_PLUS\n",yytext);}
[-] {printf("%s: OP_MINUS\n",yytext);}
[/] {printf("%s: OP_DIV\n",yytext);}
[*] {printf("%s: OP_MULT\n",yytext);}
"**" {printf("%s: OP_DBLMULT\n",yytext);}
[,] {printf("%s: OP_COMMA\n",yytext);}
["] {printf("%s: OP_OC\n",yytext);}

["]([^"\\\n]|\\(.|\n))*["]	{printf("\" : OP_OC\n%s : VALUESTR\n\": OP_CC\n",divide(yytext));}
[A-Za-z_][A-Za-z_0-9]* {printf("%s: IDENTIFIER\n",yytext);}
[1-9]*[0-9] {printf("%s: VALUEI\n",yytext);}
[1-9]*[f][0-9]* {printf("%s: VALUEF\n",yytext);}

[1-9]*[A-Za-z_][A-Za-z_0-9]* {fprintf(yyout,"%s: LEXICAL_ERROR identifier cannot start with a number\n",yytext); return 0;}
[0][[:digit:]]+	{fprintf(yyout,"SYNTAX ERROR %s cannot be tokenized\n",yytext);return 0;}
[[:digit:]]+[[:alpha:]]+    {fprintf(yyout,"SYNTAX ERROR %s cannot be tokenized\n",yytext);return 0;}
[[:alnum:]]*[[:alnum:][:punct:]]{-}[()"]+	{fprintf(yyout,"SYNTAX ERROR %s cannot be tokenized\n",yytext); return 0;}
. {fprintf(yyout,"SYNTAX ERROR %s cannot bo tekonized\n",yytext); return 0;}

%%

int main(int argc, char *argv[])
{
    if( argc > 1 && !(yyin = fopen(argv[1],"r"))){
		  fprintf(stderr,"ERROR: File couldn't be opened: %s\n",argv[1]);	
      printf("Start entering lines =>\n");
    }
    else if( argc <= 1)
      printf("Start entering lines =>\n");	
    else yyin = fopen(argv[1], "r");
    yylex(); 
    fclose(yyin);
    return 0;
}

