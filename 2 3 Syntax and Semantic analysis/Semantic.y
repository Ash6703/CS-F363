%{
#include<stdio.h>
#include<string.h>
#include<math.h>
#include<stdlib.h>
#include<string.h>

extern FILE *yyin; 


int countn=0,qind=0,tos=-1,temp_char=0;

struct quadruple{
	char operator[5];
	char operand1[10];
	char operand2[10];
	char result[10];
} quad[25];

struct stack{
	char c[10];
} stk[25];

char tempcond[100];

void addQuadruple(char op1[],char op[], char op2[], char result[]) ;

void display_Quad();

void push(char *c) ;

char* pop() ;

void L();

void Lcond();

void whilend();
void printend();
void ifCond();
void ifElseCond();
void printElse();
void printforto();
void printfordownto();
void endfor();



/* interface for drawing (can be replaced by "real" graphic using GD or other) */
struct dataType {
        char * id_name;
        char * data_type;
		char * value;
		// struct{
		// 	char * cval;
		// 	int ival;
		// } s;
} symbol_table[40];

void add();
void yyerror(char * error);
int yylex();

int getValue(char* name);

void setValue(char *, int);
void check(char *);
void insert_type(char *otype);
int search(char *);
void addWord(const char *word);
int count=0, q, max_length=15, tempint;
char type[10], temp[15];
extern int countn;
char **words = NULL; // Pointer to an array of strings to make list of words in variable declaration
int num_words = 0;  // in array of words
%}


%left PLUS MINUS
%left MUL DIV REM
%left LT GT LTE GTE NEQ EQ
%left AND OR
%right NOT
%nonassoc CBO
%union{
    struct{
        struct nodeTypeTag *nptr;
    } p;

	struct{
		char type[10];
        char subtype[10];
		char* varname;
		union{
			int ival;
			char* cval;
		} v;
	} t;
}

%token ASSIGN ID NUMBER INWORDS THEN DOUBLEQ CBC BO BC COMMA DOT COL PROGRAM INT REAL BOOLEAN CHAR VAR TO DOWNTO IF ELSE FOR DO WHILE BEG END READ WRITE SEMI ARRAY OF
%type <p> downto elsecond prog declarations list_of_variables variable_declaration list_of_identifiers type body list_of_statements statement assignment vari reading toset writing towrite toprint simple_if if_then_else while_do for_do blocks cond rel_expr bool_expr bool_term expression
%%
start : prog SEMI declarations body{return 0;}
	;
	
prog : PROGRAM ID
	;

declarations : VAR list_of_variables 
	;

list_of_variables : variable_declaration 
	| list_of_variables variable_declaration
	;

variable_declaration : list_of_identifiers COL type SEMI 

list_of_identifiers : ID {addWord($<t.varname>1);}
	| list_of_identifiers COMMA ID {addWord($<t.varname>3);}
	;
	
type : INT { insert_type("Integer");add(); }
	| REAL { insert_type("Real");add(); }
	| BOOLEAN { insert_type("Boolean");add(); }
	| CHAR { insert_type("Char");add(); }
    | ARRAY { insert_type("Array");add(); } BO NUMBER DOT DOT NUMBER BC OF type 
	;
	
body : BEG list_of_statements END DOT 
	;

list_of_statements : statement SEMI list_of_statements 
	| statement SEMI 
	;
	
statement : assignment 
	| reading
	| writing
	| blocks
	| simple_if 
	| if_then_else 
	| while_do 
	| for_do 
	;
	
assignment : vari ASSIGN expression {check(temp);setValue(temp,$<t.v.ival>3);addQuadruple(temp,"","",pop());display_Quad();pop()} 
	;

vari : ID {strcpy(temp,$<t.varname>1);push($<t.varname>1);}
	;

reading : READ CBO toset CBC
	;

toset : toset COMMA ID {
		check($<t.varname>3);
		int get;
		 // does not work, need to figure out how to take user input
		setValue($<t.varname>1,get);}
	| ID {
		check($<t.varname>1);
		int get;
		
		setValue($<t.varname>1,get);}
	;

writing : WRITE CBO towrite CBC 
	| WRITE CBO DOUBLEQ toprint DOUBLEQ CBC
	;

towrite : towrite COMMA ID {
		check($<t.varname>3);}

	| ID {
		check($<t.varname>1);
    }

toprint : INWORDS 
	| toprint INWORDS 
    ;

simple_if : IF cond{Lcond();} THEN blocks {printend();}
	;

if_then_else : IF cond{ifElseCond();} THEN blocks elsecond {printend();}
	;

elsecond: ELSE{printElse();} blocks 
	;

while_do : WHILE{L();} cond{Lcond();} DO blocks {whilend();}
	;

for_do : FOR assignment{L();} to expression{printforto();} DO blocks {endfor();}
    | FOR assignment{L();} downto expression{printfordownto();} DO blocks {endfor();}
	;

to : TO 
    ;
downto : DOWNTO
    ;

blocks : BEG list_of_statements END 
	;

cond : CBO cond CBC 
    | rel_expr 
    | bool_expr 
    ;

rel_expr : expression LT expression {char str[5],str1[5]="t"; 
                                            sprintf(str,"%d",temp_char++);
                                            strcat(str1,str);addQuadruple(pop(),"<",pop(),str1); 
                                            display_Quad();push(str1);}
    | expression GT expression {char str[5],str1[5]="t"; 
                                            sprintf(str,"%d",temp_char++);
                                            strcat(str1,str);addQuadruple(pop(),">",pop(),str1); 
                                            display_Quad();push(str1);}
    | expression GTE expression {char str[5],str1[5]="t"; 
                                            sprintf(str,"%d",temp_char++);
                                            strcat(str1,str);addQuadruple(pop(),">=",pop(),str1); 
                                            display_Quad();push(str1);}
    | expression LTE expression {char str[5],str1[5]="t"; 
                                            sprintf(str,"%d",temp_char++);
                                            strcat(str1,str);addQuadruple(pop(),"<=",pop(),str1); 
                                            display_Quad();push(str1);}
    | expression NEQ expression {char str[5],str1[5]="t"; 
                                            sprintf(str,"%d",temp_char++);
                                            strcat(str1,str);addQuadruple(pop(),"<>",pop(),str1); 
                                            display_Quad();push(str1);}
    | expression EQ expression {char str[5],str1[5]="t"; 
                                            sprintf(str,"%d",temp_char++);
                                            strcat(str1,str);addQuadruple(pop(),"==",pop(),str1); 
                                            display_Quad();push(str1);}
    ; 

bool_expr : bool_expr AND bool_term {char str[5],str1[5]="t"; 
                                            sprintf(str,"%d",temp_char++);
                                            strcat(str1,str);addQuadruple(pop(),"AND",pop(),str1); 
                                            display_Quad();push(str1);}
    | bool_expr OR bool_term {char str[5],str1[5]="t"; 
                                            sprintf(str,"%d",temp_char++);
                                            strcat(str1,str);addQuadruple(pop(),"OR",pop(),str1); 
                                            display_Quad();push(str1);}
    | NOT bool_term {char str[5],str1[5]="t"; 
                                            sprintf(str,"%d",temp_char++);
                                            strcat(str1,str);addQuadruple("","!",pop(),str1); 
                                            display_Quad();push(str1);}
    | bool_term 
    ;
bool_term : rel_expr 
    | CBO bool_expr CBC 
    ;


expression : expression PLUS expression {$<t.v.ival>$ = $<t.v.ival>1 + $<t.v.ival>3;char str[5],str1[5]="t"; 
                                            sprintf(str,"%d",temp_char++);
                                            strcat(str1,str);addQuadruple(pop(),"+",pop(),str1); 
                                            display_Quad();push(str1);}
    | expression MINUS expression   {$<t.v.ival>$ = $<t.v.ival>1 - $<t.v.ival>3;char str[5],str1[5]="t"; 
                                            sprintf(str,"%d",temp_char++);
                                            strcat(str1,str);addQuadruple(pop(),"-",pop(),str1); 
                                            display_Quad();push(str1);}
    | expression DIV expression     {$<t.v.ival>$ = $<t.v.ival>1 / $<t.v.ival>3;char str[5],str1[5]="t"; 
                                            sprintf(str,"%d",temp_char++);
                                            strcat(str1,str);addQuadruple(pop(),"*",pop(),str1); 
                                            display_Quad();push(str1);}
    | expression REM expression     {$<t.v.ival>$ = $<t.v.ival>1 % $<t.v.ival>3;char str[5],str1[5]="t"; 
                                            sprintf(str,"%d",temp_char++);
                                            strcat(str1,str);addQuadruple(pop(),"/",pop(),str1); 
                                            display_Quad();push(str1);}
    | expression MUL expression     {$<t.v.ival>$ = $<t.v.ival>1 * $<t.v.ival>3;char str[5],str1[5]="t"; 
                                            sprintf(str,"%d",temp_char++);
                                            strcat(str1,str);addQuadruple(pop(),"/",pop(),str1); 
                                            display_Quad();push(str1);}
    | CBO expression CBC            {$<t.v.ival>$ = $<t.v.ival>2;char str[5],str1[5]="t"; 
                                            sprintf(str,"%d",temp_char++);
                                            strcat(str1,str);addQuadruple(pop(),"/",pop(),str1); 
                                            display_Quad();push(str1);}
    | ID                            {$<t.v.ival>$ = getValue($<t.varname>1);push($<t.varname>1);}
    | NUMBER                        {$<t.v.ival>$ = $<t.v.ival>1;char str[100];sprintf(str, "%d", $<t.v.ival>1);
                                        char c[5]; 
                                        sprintf(c,"%d",$<t.v.ival>1);
                                        push(c);}
    ;


%%
int main(int argc, char *argv[]){
        char* filename;
        filename=argv[1];
        yyin=fopen(filename, "r");
    yyparse();
	int i=0;
	for(i=0;i<count;i++) {
		free(symbol_table[i].id_name);
		free(symbol_table[i].data_type);
	}
	printf("\n********DONE******\n");
	return 0;
}

void yyerror(char * error) {
	printf("%s",error);
	exit(0);
}

int search(char *variable_name) {
	int i;
	for(i=count-1; i>=0; i--) {
		if(strcmp(symbol_table[i].id_name, variable_name)==0) {
			return -1;
			break;
		}
	}
	return 0;
}

void insert_type(char * oftype) {
	strcpy(type, oftype);
}

void add() {
    for (int i = 0; i < num_words; i++) {
        q=search(words[i]);
  		if(!q) {
			symbol_table[count].id_name=strdup(words[i]);
			symbol_table[count].data_type=strdup(type);
			symbol_table[count].value="-";
			count++;
		} else {
			printf("Error : \'%s\'\n",words[i]);
			yyerror("Error : Multiple declerations of the same variable\n");
			exit(0);
		}
    }
	**words = NULL;
	num_words = 0;
}

void addWord(const char *word) {
        // Increase the size of the array of pointers
        words = (char**)realloc(words, (num_words + 1) * sizeof(char*));
        if (words == NULL) {
            fprintf(stderr, "Memory allocation failed\n");
            exit(EXIT_FAILURE);
        }
        // Allocate memory for the new word and copy it
        words[num_words] = (char*)malloc(strlen(word) + 1);
        if (words[num_words] == NULL) {
            fprintf(stderr, "Memory allocation failed\n");
            exit(EXIT_FAILURE);
        }
        strcpy(words[num_words], word);
        // Increment the count of words in the list
        num_words++;
}

void check(char * declared) {
	q=search(declared);
	if(!q) {
		printf("Error : \'%s\'\n",declared);
		yyerror("Error : Variable has not been declared previously\n");
		exit(0);
	}
}

void setValue(char* name, int valu) {
	int i;
	for(i=count-1; i>=0; i--) {
		if(strcmp(symbol_table[i].id_name, name)==0) {
			char int_str[20];
    		sprintf(int_str, "%d", valu);
			symbol_table[i].value = strdup(int_str);
			return ;
			break;
		}
	}
	printf("huh");
}

int getValue(char* name) {
	int i;
	for(i=count-1; i>=0; i--) {
		if(strcmp(symbol_table[i].id_name, name)==0) {
			if(symbol_table[i].value=="-") yyerror("Error : Variable is used before assignment");
			return atoi(symbol_table[i].value);
		}
	}
	printf("coulndt find\n");
}

void addQuadruple(char op1[],char op[], char op2[], char result[]) {
	strcpy(quad[qind].operator,op);
	strcpy(quad[qind].operand1,op1);
	strcpy(quad[qind].operand2,op2);
	strcpy(quad[qind].result,result);
	qind++;
}

void display_Quad(){
	printf("%s",quad[qind-1].result);
	printf(" = ");
	printf("%s",quad[qind-1].operand1);
	printf(" %s",quad[qind-1].operator);
	printf(" %s\n",quad[qind-1].operand2);
}

void push(char *c) {
	strcpy(stk[++tos].c,c);
} 

char* pop() {
	char* c=stk[tos].c;
	tos=tos-1;
	return c;
}

void L(){
    printf("\nL%d: \n",++countn);
}

void Lcond(){
    printf("t%d = not %s\n",temp_char,stk[tos].c);
    printf("if t%d goto END\n",temp_char);
    temp_char++;
}

void whilend(){
    printf("Goto L%d\n",countn);
    printf("END\n");

}

void ifElseCond(){
    printf("t%d = not %s\n",temp_char,stk[tos].c);
    printf("if t%d goto ELSE\n",temp_char);
    temp_char++;   
}

void printElse(){
    printf("Goto END\n");
    printf("ELSE\n");
}

void printforto(){
    printf("t%d = %s > %s\n",temp_char,temp,stk[tos].c);
    printf("if t%d goto END\n",temp_char);
    temp_char++;
}

void printfordownto(){
    printf("t%d = %s < %s\n",temp_char,temp,stk[tos].c);
    printf("if t%d goto END\n",temp_char);
    temp_char++;
}

void endfor(){
    printf("Goto L%d\n",countn);
    printf("END\n");
}

void printend(){
    printf("END\n");
}
