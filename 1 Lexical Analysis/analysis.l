%{
#include<stdlib.h>
#include<stdio.h>
int count=1,max_length=12;
char buff[100],*s;

%}
%option caseless
%x COMMENT OPEN
%%
program {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
to {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
downto {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
if {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
then {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
else {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
of {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
int {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
real {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
boolean {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
char {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
var {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
for {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
do {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
while {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
array {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
and {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
or {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
not {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
begin {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
end {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
read {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
write {printf("%-*d %-*s Keyword\n",max_length,count,max_length,yytext);}
[-<>=+*/%] {printf("%-*d %-*s Operator\n",max_length,count,max_length,yytext);}
"<>"|"<="|">="|"<"|">" {printf("%-*d %-*s Operator\n",max_length,count,max_length,yytext);}
[a-zA-Z][a-zA-Z0-9_]* {printf("%-*d %-*s Identifier\n",max_length,count,max_length,yytext);}
[0-9]+ {printf("%-*d %-*s Number\n",max_length,count,max_length,yytext);}
"[" {printf("%-*d %-*s Punctuator\n",max_length,count,max_length,yytext);}
"]" {printf("%-*d %-*s Punctuator\n",max_length,count,max_length,yytext);}
"{" {printf("%-*d %-*s Punctuator\n",max_length,count,max_length,yytext);}
"}" {printf("%-*d %-*s Punctuator\n",max_length,count,max_length,yytext);}
"(" {printf("%-*d %-*s Punctuator\n",max_length,count,max_length,yytext);}
")" {printf("%-*d %-*s Punctuator\n",max_length,count,max_length,yytext);}
";" {printf("%-*d %-*s Punctuator\n",max_length,count,max_length,yytext);}
":" {printf("%-*d %-*s Punctuator\n",max_length,count,max_length,yytext);}
"." {printf("%-*d %-*s Punctuator\n",max_length,count,max_length,yytext);}
"," {printf("%-*d %-*s Punctuator\n",max_length,count,max_length,yytext);}
"\'" {printf("%-*d %-*s Punctuator\n",max_length,count,max_length,yytext);}
"\"" {printf("%-*d %-*s Punctuator\n",max_length,count,max_length,yytext); s = buff;BEGIN OPEN;}
<OPEN>\" {*s=0;printf("%-*d %-*s Literal\n",max_length,count,max_length,buff);BEGIN INITIAL;}
<OPEN>. {*s++ = *yytext;}
"//" {BEGIN COMMENT;}
<COMMENT>"\n" {count++;BEGIN INITIAL;}
<COMMENT>. ;
\n {count++;}
" " ;
"\t" ;
. printf("%s ",yytext);
%%
int main() {
	printf("----------------------------------");
	printf("Line Number Lexeme      Token type");
	printf("----------------------------------");
	yyin=fopen("Sample.txt","r");
	yylex();
	return 0;
}
