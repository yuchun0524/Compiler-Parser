/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;
    int AddressNum = -1; /*count the number of address*/
    int ScopeNum = -1; /*record the number of scope*/
    int i;             /* the variable of for loop */
    int id_flag = 0;  /*determine if data type is ident*/
    int str_flag = 0; /*determine if data type is string*/
    int int_flag = 0; /*determine if data type is int*/
    int float_flag = 0; /*determine if data type is float*/
    extern int decl_flag; /*determine if the statement is declaration*/
    extern int for_flag;  /*determine if the statement is for loop*/
    int StoreLineno = 0, StoreAddress = 0; /*store the lineno and address of IDENT*/
    char *StoreName = ""; /*store the name of IDENT*/
    char *StoreType = ""; /*store the type of IDENT*/
    char *StoreElementType = ""; /*store the ElementType of IDENT*/
    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }
    void sematic_error(char const *s, int number)
    {
        if(decl_flag)
        {
            decl_flag = 0;
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n", yylineno, s, number);
        }
        else 
            printf("error:%d: undefined: %s\n", yylineno+1, s);
    }
    void type_error(char *str1, char *str2, char *operation)
    {
        if((strcmp(str1, "int32") == 0) || (strcmp(str2, "int32") == 0) || (strcmp(str1, "float32") == 0) || (strcmp(str2, "float32") == 0))
        {
            if((strcmp(str1, "int32") == 0) && (strcmp(str2, "float32") == 0))
                printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n", yylineno, operation, str1, str2);
            else if((strcmp(str1, "float32") == 0) && (strcmp(str2, "int32") == 0))
                printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n", yylineno, operation, str1, str2);
        }
        else
        {
            char *type1;
            char *type2;
            if(lookup_symbol(str1) == 1)
                type1 = StoreType;
            if(lookup_symbol(str2) == 1)
                type2 = StoreType;
            if((strcmp(type1, "int32") == 0) && (strcmp(type2, "float32") == 0))
                printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n", yylineno, operation, type1, type2);
            else if((strcmp(type1, "float32") == 0) && (strcmp(type2, "int32") == 0))
                printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n", yylineno, operation, type1, type2);
        }
    }
    /* doubly linked list */
    struct SymbolTable{
        char *name;
        char *type;
        int address;
        int lineno;
        char *ElementType;   
    };

    struct Symbol{
        int SymbolNum;
        struct SymbolTable s[10];
        struct Symbol *pre;
        struct Symbol *next;
    };
    typedef struct Symbol symbol;

    symbol *head = NULL, *temp = NULL, *tail = NULL;
    /* Symbol table function - you can add new function if needed. */
    static void create_symbol();
    static void insert_symbol(char *name, char *type, int address, int lineno, char *ElementType);
    int lookup_symbol(char *name);
    static void dump_symbol();
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    /* ... */
}

/* Token without return */
%token INT FLOAT STRING BOOL TRUE FALSE VAR 
%token '>' '<' GEQ LEQ EQL NEQ
%token '+' '-' '*' '/' '%' INC DEC
%token '=' ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token LAND LOR '!'
%token '(' ')' '[' ']' '{' '}' '"'
%token ';' ',' NEWLINE
%token PRINT PRINTLN IF ELSE FOR

%right '='
%left LOR
%left LAND 
%left '>' '<' GEQ LEQ EQL NEQ
%left '+' '-'
%left '*' '/' '%'
%left '!'
%nonassoc UMINUS
%left '[' ']'
%left '(' ')'

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT
%token <s_val> IDENT

/* Nonterminal with return, which need to sepcify type */
%type <s_val> Type
%type <s_val> SimpleStmt
%type <s_val> Expression
%type <s_val> Literal
/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%

Program
    : StatementList
;

StatementList
    : StatementList Statement
    | Statement
;

Statement
    : DeclarationStmt NEWLINE
    | SimpleStmt NEWLINE
    | '{'  { create_symbol(); }
    | '}'  { dump_symbol(); }
    | IfStmt StatementList
    | IfStmt StatementList ELSE StatementList
    | ForStmt StatementList {for_flag = 0;}
    | PrintStmt NEWLINE
    | NEWLINE
;

DeclarationStmt
    : VAR IDENT Type { 
        if(lookup_symbol($2) == 0){
            AddressNum++;
            insert_symbol($2, $3, AddressNum, yylineno, "-");
            lookup_symbol($2);
            decl_flag = 0;
        }
        else
        {   
            sematic_error($2, StoreLineno);
        }
    }
    | VAR IDENT Type '=' Expression {
        if(lookup_symbol($2) == 0){
            AddressNum++;
            insert_symbol($2, $3, AddressNum, yylineno, "-");
            lookup_symbol($2);
            decl_flag = 0;
        }
        else
        {   
            sematic_error($2, StoreLineno);
        }
    }
    | VAR IDENT '[' Expression ']' Type {
        if(lookup_symbol($2) == 0){
            AddressNum++;
            insert_symbol($2, "array", AddressNum, yylineno+1, $6);
            decl_flag = 0;
        }
        else
        {   
            sematic_error($2, StoreLineno);
        }
    }
;

SimpleStmt
    : Expression
;

IfStmt
    : IF SimpleStmt {
        if(strcmp($2, "bool") !=0)
        {
            if((strcmp($2, "int32") ==0) || (strcmp($2, "float32") ==0))
                printf("error:%d: non-bool (type %s) used as for condition\n",yylineno+1, $2);
            else
            {
                if(lookup_symbol($2) == 1)
                    printf("error:%d: non-bool (type %s) used as for condition\n",yylineno+1, StoreType);
            }
        }
    } 
;

ForStmt
    : FOR SimpleStmt {
        if(strcmp($2, "bool") !=0)
        {
            if((strcmp($2, "int32") ==0) || (strcmp($2, "float32") ==0))
                printf("error:%d: non-bool (type %s) used as for condition\n",yylineno+1, $2);
            else
            {
                if(lookup_symbol($2) == 1)
                    printf("error:%d: non-bool (type %s) used as for condition\n",yylineno+1, StoreType);
            }
        }
    }
    | FOR ForClause  
;

ForClause
    : SimpleStmt ';' SimpleStmt ';' SimpleStmt
;

PrintStmt
    : PRINT '(' Expression ')'{
        if(str_flag)
            printf("PRINT string\n");
    }
    | PRINT '(' IDENT ')' { if(lookup_symbol($3) == 1)
            {
                
                printf("%s (name=%s, address=%d)\n", "IDENT", StoreName, StoreAddress);
                printf("PRINT %s\n",StoreType);
            }}
    | PRINTLN '(' IDENT ')' { 
        if(!for_flag){
            if(lookup_symbol($3) == 1)
            {
                
                printf("%s (name=%s, address=%d)\n", "IDENT", StoreName, StoreAddress);
                printf("PRINTLN %s\n",StoreType);
            }
        }
        else{
             printf("%s (name=%s, address=%d)\n", "IDENT", StoreName, StoreAddress);
             printf("PRINTLN %s\n",StoreType);
        }
    }
    | PRINTLN '(' Expression '[' Expression ']' ')'{if(lookup_symbol($3) == 1)
            {
                printf("PRINTLN %s\n",StoreElementType);
            }}
    | PRINTLN '(' Expression ')' { 
        
        if(id_flag)
        {
            if(lookup_symbol($3) == 1)
            {
                printf("PRINTLN %s\n",StoreType);
            }
        }
        else if(str_flag)
            printf("PRINTLN string\n");
        else if(float_flag)
            printf("PRINTLN float32\n");
        else if(int_flag)
            printf("PRINTLN int32\n");
        else
            printf("PRINTLN bool\n");
    }
;

Expression
    : '(' Expression ')' { $$ = $2;}
    | Type '(' Expression ')' {
        if(id_flag)
        {
        if(lookup_symbol($3) == 1){
            if(strcmp(StoreType, "float32") == 0)
                printf("F to ");
            else if(strcmp(StoreType, "int32") == 0)
                printf("I to ");
        }
        }
        else
        {
            if(float_flag)
                printf("F to ");
            else if(int_flag)
                printf("I to ");
        }
        if(strcmp($1, "float32") == 0)
                printf("F\n");
        else if(strcmp($1, "int32") == 0)
                printf("I\n");
    }
    | Expression '[' Expression ']'
    | Expression '=' Expression     {
        type_error($1, $3, "ASSIGN");
        printf("%s\n", "ASSIGN");  
    }
    | Expression ADD_ASSIGN Expression     {
        if(strcmp($1,"int32") == 0)
            printf("error:%d: cannot assign to int32\n", yylineno);
        else if(strcmp($1,"float32") == 0)
            printf("error:%d: cannot assign to float32\n", yylineno);
        printf("%s\n", "ADD_ASSIGN");  
    }
    | Expression SUB_ASSIGN Expression     {
        if(strcmp($1,"int32") == 0)
            printf("error:%d: cannot assign to int32\n", yylineno);
        else if(strcmp($1,"float32") == 0)
            printf("error:%d: cannot assign to float32\n", yylineno);
        printf("%s\n", "SUB_ASSIGN");  
    }
    | Expression MUL_ASSIGN Expression     {
        if(strcmp($1,"int32") == 0)
            printf("error:%d: cannot assign to int32\n", yylineno);
        else if(strcmp($1,"float32") == 0)
            printf("error:%d: cannot assign to float32\n", yylineno);
        printf("%s\n", "MUL_ASSIGN");  
    }
    | Expression QUO_ASSIGN Expression     {
        if(strcmp($1,"int32") == 0)
            printf("error:%d: cannot assign to int32\n", yylineno);
        else if(strcmp($1,"float32") == 0)
            printf("error:%d: cannot assign to float32\n", yylineno);
        printf("%s\n", "QUO_ASSIGN");  
    }
    | Expression REM_ASSIGN Expression     {
        if(strcmp($1,"int32") == 0)
            printf("error:%d: cannot assign to int32\n", yylineno);
        else if(strcmp($1,"float32") == 0)
            printf("error:%d: cannot assign to float32\n", yylineno);
        printf("%s\n", "REM_ASSIGN");  
    }
    | Expression '+' Expression     { 
        type_error($1, $3, "ADD");
        printf("%s\n", "ADD"); 
    } 
    | Expression '-' Expression     { 
        type_error($1, $3, "SUB");
        printf("%s\n", "SUB"); 
    }
    | Expression '*' Expression     { 
        printf("%s\n", "MUL"); 
    }
    | Expression '/' Expression     { 
        printf("%s\n", "QUO"); 
    }
    | Expression '%' Expression     { 
        if((strcmp($1, "float32") == 0) || (strcmp($3, "float32") == 0))
            printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "REM","float32");
        else
        {
            if(lookup_symbol($1) == 1)
            {
                if((strcmp(StoreType, "float32") == 0))
                    printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "REM","float32");
                else
                {
                    if(lookup_symbol($3) == 1)
                    {
                        if((strcmp(StoreType, "float32") == 0))
                            printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "REM","float32");
                    }
                }
            }
        }
        printf("%s\n", "REM"); 
    }
    | Expression '>' Expression     { 
        $$ = "bool";
        printf("%s\n", "GTR"); 
    }
    | Expression '<' Expression     { 
        $$ = "bool";
        printf("%s\n", "LSS"); 
    }
    | Expression EQL Expression     { 
        $$ = "bool";
        printf("%s\n", "EQL"); 
    }
    | Expression NEQ Expression     { 
        $$ = "bool";
        printf("%s\n", "NEQ"); 
    }
    | Expression LEQ Expression     { 
        $$ = "bool";
        printf("%s\n", "LEQ"); 
    }
    | Expression GEQ Expression     { 
        $$ = "bool";
        printf("%s\n", "GEQ"); 
    }
    | Expression LOR Expression     { 
        if((strcmp($1, "int32") == 0) || (strcmp($3, "int32") == 0))
            printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "LOR","int32");
        else
        {
            if(lookup_symbol($1) == 1)
            {
                if((strcmp(StoreType, "int32") == 0))
                    printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "LOR","int32");
                else
                {
                    if(lookup_symbol($3) == 1)
                    {
                        if((strcmp(StoreType, "int32") == 0))
                            printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "LOR","int32");
                    }
                }
            }
        }
        printf("%s\n", "LOR"); 
    }
    | Expression LAND Expression    { 
        if((strcmp($1, "int32") == 0) || (strcmp($3, "int32") == 0))
            printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "LAND","int32");
        else
        {
            if(lookup_symbol($1) == 1)
            {
                if((strcmp(StoreType, "int32") == 0))
                    printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "LAND","int32");
                else
                {
                    if(lookup_symbol($3) == 1)
                    {
                        if((strcmp(StoreType, "int32") == 0))
                            printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "LAND","int32");
                    }
                }
            }
        }
        printf("%s\n", "LAND"); 
    }
    | '-' Expression %prec UMINUS   { 
        $$ = $2;
        printf("%s\n", "NEG"); 
    }
    | '+' Expression %prec UMINUS    { 
        $$ = $2;
        printf("%s\n", "POS"); 
    }
    | '!' Expression %prec UMINUS   { 
        $$ = $2;
        printf("%s\n", "NOT"); 
    }
    | Expression INC                {
        printf("%s\n", "INC");
    }
    | Expression DEC                {
        printf("%s\n", "DEC");   
    }
    | Literal
;

Literal
    : INT_LIT       { printf("%s %d\n", "INT_LIT", $<i_val>1); $<s_val>$ = "int32"; id_flag = 0; str_flag = 0; int_flag = 1; float_flag = 0; }
    | FLOAT_LIT     { printf("%s %f\n", "FLOAT_LIT", $<f_val>1); $<s_val>$ = "float32";id_flag = 0; str_flag = 0; int_flag = 0; float_flag = 1;}
    | '"' STRING_LIT '"'   { printf("%s %s\n", "STRING_LIT", $<s_val>2); id_flag = 0; str_flag = 1; int_flag = 0; float_flag = 0;}
    | TRUE          { printf("%s\n", "TRUE"); $<s_val>$ = "TRUE";id_flag = 0; str_flag = 0; int_flag = 0; float_flag = 0;}
    | FALSE         { printf("%s\n", "FALSE"); $<s_val>$ = "FALSE";id_flag = 0; str_flag = 0; int_flag = 0; float_flag = 0;}
    | IDENT         { 
        id_flag = 1; str_flag = 0; int_flag = 0; float_flag = 0;
        if(!for_flag)
        {
            if(lookup_symbol($1) == 0){
                sematic_error($1, 0);
            }
            else{           
                printf("%s (name=%s, address=%d)\n", "IDENT", StoreName, StoreAddress);
            
            }
        }
        else{
            lookup_symbol($1);
            printf("%s (name=%s, address=%d)\n", "IDENT", StoreName, StoreAddress);
        }
    }
;

Type
    : INT   { $$ = "int32";}
    | FLOAT { $$ = "float32";}
    | STRING  { $$ = "string";}
    | BOOL    { $$ = "bool";}
;

%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }
    yylineno = 0;
    create_symbol();
    yyparse();
    dump_symbol();
	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}

static void create_symbol() {
    temp = (symbol *)malloc(sizeof(symbol));
    temp->next = NULL;
    temp->SymbolNum = -1;
    for(i = 0; i < 10; i++){
        temp->s[i].address = 0;
        temp->s[i].ElementType = "-";
        temp->s[i].lineno = 0;
        temp->s[i].name = "";
        temp->s[i].type = "";
    }
    if (head == NULL){
        head = temp;
        head->pre = NULL;
        tail = head;
    }
    else{
        temp->pre = tail;
        tail->next = temp;
        tail = temp;
    }
    ScopeNum++;
}

static void insert_symbol(char *name, char *type, int address, int lineno, char *ElementType) {
    tail->SymbolNum++;
    tail->s[tail->SymbolNum].name = name;
    tail->s[tail->SymbolNum].type = type;
    tail->s[tail->SymbolNum].address = address;
    tail->s[tail->SymbolNum].lineno = lineno;
    tail->s[tail->SymbolNum].ElementType = ElementType;
    printf("> Insert {%s} into symbol table (scope level: %d)\n", name, ScopeNum);
}

int lookup_symbol(char *name) {
    for(i = 0; i < 10; i++){
        if(strcmp(tail->s[i].name, name) == 0)
        {
            StoreLineno = tail->s[i].lineno;
            StoreName = tail->s[i].name;
            StoreAddress = tail->s[i].address;
            StoreType = tail->s[i].type;
            StoreElementType = tail->s[i].ElementType;
            return 1;
        }
    }
    return 0;
}

static void dump_symbol() {
    if( tail != NULL){
        printf("> Dump symbol table (scope level: %d)\n", ScopeNum);
        printf("%-10s%-10s%-10s%-10s%-10s%s\n",
           "Index", "Name", "Type", "Address", "Lineno", "Element type");
        for(i = 0; i <= tail->SymbolNum; i++){
            printf("%-10d%-10s%-10s%-10d%-10d%s\n",
                i, tail->s[i].name, tail->s[i].type, tail->s[i].address, tail->s[i].lineno, tail->s[i].ElementType);           
        }
        temp = tail;
        if(tail->pre != NULL)
            tail = tail->pre;
        tail->next = NULL;
        free(temp);
        ScopeNum--;
    }
}