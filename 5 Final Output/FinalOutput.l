%{
#include "y.tab.h"
#include<stdlib.h>
#include<stdio.h>
#include<string.h>
char buff[100],*s;
%}
%x OPEN COMMENT 
%option caseless noyywrap
%%
program {return PROGRAM;}
integer {return INT;}
real {return REAL;}
boolean {return BOOLEAN;}
char {return CHAR;}
var {return VAR;}
to {return TO;}
downto {return DOWNTO;}
then {return THEN;}
if {return IF;}
else {return ELSE;}
for {return FOR;}
of {return OF;}
do {return DO;}
while {return WHILE;}
array {return ARRAY;}
and {return AND;}
or {return OR;}
not {return NOT;}
begin {return BEG;}
end {return END;}
read {return READ;}
write {return WRITE;}
[a-zA-Z]([a-zA-Z]|[0-9]|[_])*  {yylval.t.varname=yytext;return ID;}
":=" {return ASSIGN;}
";" {return SEMI;}
":" {return COL;}
\. {return DOT;}
"(" {return CBO;}
")" {return CBC;}
"<" {return LT;}
">" {return GT;}
"=" {return EQ;}
[0-9]+ {yylval.t.v.ival=atoi(yytext);return NUMBER;}
"<=" {return LTE;}
">=" {return GTE;}
"<>" {return NEQ;}
[,] {return COMMA;}
"\"" {BEGIN OPEN;return DOUBLEQ;}
<OPEN>\" {BEGIN INITIAL;return DOUBLEQ;}
<OPEN>. {yylval.t.varname = yytext;return INWORDS;}
"//" {BEGIN COMMENT;}
<COMMENT>"\n" {BEGIN INITIAL;}
<COMMENT>. ;
"+" {return PLUS;}
"-" {return MINUS;}
"*" {return MUL;}
"/" {return DIV;}
"%" {return REM;}
\[ {return BO;}
\] {return BC;}
"\n" ;
" " ;
"\t" ;
%%
