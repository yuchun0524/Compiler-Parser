/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 2 "compiler_hw2.y" /* yacc.c:339  */

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

#line 146 "y.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    INT = 258,
    FLOAT = 259,
    STRING = 260,
    BOOL = 261,
    TRUE = 262,
    FALSE = 263,
    VAR = 264,
    GEQ = 265,
    LEQ = 266,
    EQL = 267,
    NEQ = 268,
    INC = 269,
    DEC = 270,
    ADD_ASSIGN = 271,
    SUB_ASSIGN = 272,
    MUL_ASSIGN = 273,
    QUO_ASSIGN = 274,
    REM_ASSIGN = 275,
    LAND = 276,
    LOR = 277,
    NEWLINE = 278,
    PRINT = 279,
    PRINTLN = 280,
    IF = 281,
    ELSE = 282,
    FOR = 283,
    UMINUS = 284,
    INT_LIT = 285,
    FLOAT_LIT = 286,
    STRING_LIT = 287,
    IDENT = 288
  };
#endif
/* Tokens.  */
#define INT 258
#define FLOAT 259
#define STRING 260
#define BOOL 261
#define TRUE 262
#define FALSE 263
#define VAR 264
#define GEQ 265
#define LEQ 266
#define EQL 267
#define NEQ 268
#define INC 269
#define DEC 270
#define ADD_ASSIGN 271
#define SUB_ASSIGN 272
#define MUL_ASSIGN 273
#define QUO_ASSIGN 274
#define REM_ASSIGN 275
#define LAND 276
#define LOR 277
#define NEWLINE 278
#define PRINT 279
#define PRINTLN 280
#define IF 281
#define ELSE 282
#define FOR 283
#define UMINUS 284
#define INT_LIT 285
#define FLOAT_LIT 286
#define STRING_LIT 287
#define IDENT 288

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 87 "compiler_hw2.y" /* yacc.c:355  */

    int i_val;
    float f_val;
    char *s_val;
    /* ... */

#line 259 "y.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 276 "y.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  45
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   545

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  52
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  13
/* YYNRULES -- Number of rules.  */
#define YYNRULES  64
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  125

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   288

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    31,    38,     2,     2,    20,     2,     2,
      32,    33,    18,    16,    40,    17,     2,    19,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    39,
      11,    23,    10,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    34,     2,    35,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    36,     2,    37,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    12,    13,    14,    15,    21,
      22,    24,    25,    26,    27,    28,    29,    30,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   133,   133,   137,   138,   142,   143,   144,   145,   146,
     147,   148,   149,   150,   154,   166,   178,   192,   196,   211,
     223,   227,   231,   235,   241,   255,   259,   280,   281,   303,
     304,   308,   315,   322,   329,   336,   343,   347,   351,   354,
     357,   378,   382,   386,   390,   394,   398,   402,   423,   444,
     448,   452,   456,   459,   462,   466,   467,   468,   469,   470,
     471,   491,   492,   493,   494
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "INT", "FLOAT", "STRING", "BOOL", "TRUE",
  "FALSE", "VAR", "'>'", "'<'", "GEQ", "LEQ", "EQL", "NEQ", "'+'", "'-'",
  "'*'", "'/'", "'%'", "INC", "DEC", "'='", "ADD_ASSIGN", "SUB_ASSIGN",
  "MUL_ASSIGN", "QUO_ASSIGN", "REM_ASSIGN", "LAND", "LOR", "'!'", "'('",
  "')'", "'['", "']'", "'{'", "'}'", "'\"'", "';'", "','", "NEWLINE",
  "PRINT", "PRINTLN", "IF", "ELSE", "FOR", "UMINUS", "INT_LIT",
  "FLOAT_LIT", "STRING_LIT", "IDENT", "$accept", "Program",
  "StatementList", "Statement", "DeclarationStmt", "SimpleStmt", "IfStmt",
  "ForStmt", "ForClause", "PrintStmt", "Expression", "Literal", "Type", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
      62,    60,   265,   266,   267,   268,    43,    45,    42,    47,
      37,   269,   270,    61,   271,   272,   273,   274,   275,   276,
     277,    33,    40,    41,    91,    93,   123,   125,    34,    59,
      44,   278,   279,   280,   281,   282,   283,   284,   285,   286,
     287,   288
};
# endif

#define YYPACT_NINF -38

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-38)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     120,   -38,   -38,   -38,   -38,   -38,   -38,   -37,   169,   169,
     169,   169,   -38,   -38,   -35,   -38,    -9,    -3,   169,   169,
     -38,   -38,   -38,    30,   120,   -38,   -10,    -6,   120,   120,
       2,   461,   -38,    23,     4,   168,   168,   168,   361,    18,
     205,   241,   -38,    31,   -38,   -38,   -38,   -38,   -38,    71,
     120,   -38,   169,   169,   169,   169,   169,   169,   169,   169,
     169,   169,   169,   -38,   -38,   169,   169,   169,   169,   169,
     169,   169,   169,   169,   169,   169,    48,   -38,   -38,    39,
     386,    40,   411,   169,   120,     0,     0,     0,     0,     0,
       0,   121,   121,   168,   168,   168,   461,   461,   461,   461,
     461,   461,   511,   486,   283,   436,   309,   169,   -38,   -38,
     -38,   -38,   169,    42,   120,   -38,   -38,    36,   461,   335,
     169,   -38,    49,   -38,   -38
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    61,    62,    63,    64,    58,    59,     0,     0,     0,
       0,     0,     7,     8,     0,    13,     0,     0,     0,     0,
      55,    56,    60,     0,     2,     4,     0,     0,     0,     0,
       0,    17,    54,     0,     0,    50,    49,    51,     0,     0,
       0,     0,    18,    19,    20,     1,     3,     5,     6,     9,
      11,    12,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    52,    53,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    27,    57,    60,
       0,    60,     0,     0,     0,    41,    42,    46,    45,    43,
      44,    36,    37,    38,    39,    40,    30,    31,    32,    33,
      34,    35,    48,    47,     0,     0,     0,     0,    23,    22,
      24,    26,     0,     0,    10,    29,    28,     0,    15,     0,
       0,    16,    29,    21,    25
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -38,   -38,   -16,   -13,   -38,   -14,   -38,   -38,   -38,   -38,
      -8,   -38,   -28
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,    23,    24,    25,    26,    27,    28,    29,    44,    30,
      31,    32,    33
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      35,    36,    37,    38,    42,    43,    76,     1,     2,     3,
       4,    46,    49,    50,    34,    39,    58,    59,    60,    61,
      62,    63,    64,    40,    66,    67,    68,    69,    70,    41,
      45,    47,    80,    82,    73,    48,    46,    46,    75,     1,
       2,     3,     4,    51,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    74,    78,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,   106,   114,   113,
      83,   107,   108,   110,     1,     2,     3,     4,     5,     6,
       7,   120,   124,     0,     0,     0,     0,     8,     9,   121,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   118,
       0,    46,    10,    11,   119,     0,   123,    12,    13,    14,
       0,     0,    15,    16,    17,    18,    84,    19,     0,    20,
      21,     0,    22,     1,     2,     3,     4,     5,     6,     7,
       0,     0,     0,     0,     0,     0,     8,     9,     0,    60,
      61,    62,    63,    64,     0,    66,    67,    68,    69,    70,
       0,    10,    11,     0,     0,    73,    12,    13,    14,     0,
       0,    15,    16,    17,    18,     0,    19,     0,    20,    21,
       0,    22,     1,     2,     3,     4,     5,     6,     0,     0,
       0,     0,     0,     0,     0,     8,     9,     0,     0,    63,
      64,     0,    66,    67,    68,    69,    70,     0,     0,     0,
      10,    11,    73,     0,     0,     0,     0,    14,     1,     2,
       3,     4,     5,     6,     0,     0,     0,    20,    21,     0,
      22,     8,     9,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    10,    11,     0,     0,
       0,     0,     0,    14,     1,     2,     3,     4,     5,     6,
       0,     0,     0,    20,    21,     0,    79,     8,     9,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    10,    11,     0,     0,     0,     0,     0,    14,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    20,
      21,     0,    81,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,    73,   115,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,    73,   117,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,    73,
     122,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,    77,    73,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,   109,
      73,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,   111,   112,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,   116,
      73,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,    73,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    64,     0,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
      73,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,     0,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,    73
};

static const yytype_int8 yycheck[] =
{
       8,     9,    10,    11,    18,    19,    34,     3,     4,     5,
       6,    24,    28,    29,    51,    50,    16,    17,    18,    19,
      20,    21,    22,    32,    24,    25,    26,    27,    28,    32,
       0,    41,    40,    41,    34,    41,    49,    50,    34,     3,
       4,     5,     6,    41,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    32,    38,    65,    66,    67,
      68,    69,    70,    71,    72,    73,    74,    75,    84,    83,
      39,    23,    33,    33,     3,     4,     5,     6,     7,     8,
       9,    39,    33,    -1,    -1,    -1,    -1,    16,    17,   117,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,
      -1,   114,    31,    32,   112,    -1,   120,    36,    37,    38,
      -1,    -1,    41,    42,    43,    44,    45,    46,    -1,    48,
      49,    -1,    51,     3,     4,     5,     6,     7,     8,     9,
      -1,    -1,    -1,    -1,    -1,    -1,    16,    17,    -1,    18,
      19,    20,    21,    22,    -1,    24,    25,    26,    27,    28,
      -1,    31,    32,    -1,    -1,    34,    36,    37,    38,    -1,
      -1,    41,    42,    43,    44,    -1,    46,    -1,    48,    49,
      -1,    51,     3,     4,     5,     6,     7,     8,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    16,    17,    -1,    -1,    21,
      22,    -1,    24,    25,    26,    27,    28,    -1,    -1,    -1,
      31,    32,    34,    -1,    -1,    -1,    -1,    38,     3,     4,
       5,     6,     7,     8,    -1,    -1,    -1,    48,    49,    -1,
      51,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    31,    32,    -1,    -1,
      -1,    -1,    -1,    38,     3,     4,     5,     6,     7,     8,
      -1,    -1,    -1,    48,    49,    -1,    51,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    31,    32,    -1,    -1,    -1,    -1,    -1,    38,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    -1,    51,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    -1,    -1,    -1,    34,    35,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      -1,    -1,    -1,    34,    35,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    -1,    -1,    -1,    34,
      35,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    -1,    33,    34,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    -1,    -1,    33,
      34,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    -1,    33,    34,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    -1,    -1,    33,
      34,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    -1,    -1,    -1,    34,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    -1,
      24,    25,    26,    27,    28,    29,    -1,    -1,    -1,    -1,
      34,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    -1,    24,    25,    26,    27,    28,
      -1,    -1,    -1,    -1,    -1,    34
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,     6,     7,     8,     9,    16,    17,
      31,    32,    36,    37,    38,    41,    42,    43,    44,    46,
      48,    49,    51,    53,    54,    55,    56,    57,    58,    59,
      61,    62,    63,    64,    51,    62,    62,    62,    62,    50,
      32,    32,    57,    57,    60,     0,    55,    41,    41,    54,
      54,    41,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    34,    32,    34,    64,    33,    38,    51,
      62,    51,    62,    39,    45,    62,    62,    62,    62,    62,
      62,    62,    62,    62,    62,    62,    62,    62,    62,    62,
      62,    62,    62,    62,    62,    62,    62,    23,    33,    33,
      33,    33,    34,    57,    54,    35,    33,    35,    62,    62,
      39,    64,    35,    57,    33
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    52,    53,    54,    54,    55,    55,    55,    55,    55,
      55,    55,    55,    55,    56,    56,    56,    57,    58,    59,
      59,    60,    61,    61,    61,    61,    61,    62,    62,    62,
      62,    62,    62,    62,    62,    62,    62,    62,    62,    62,
      62,    62,    62,    62,    62,    62,    62,    62,    62,    62,
      62,    62,    62,    62,    62,    63,    63,    63,    63,    63,
      63,    64,    64,    64,    64
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     2,     2,     1,     1,     2,
       4,     2,     2,     1,     3,     5,     6,     1,     2,     2,
       2,     5,     4,     4,     4,     7,     4,     3,     4,     4,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     2,
       2,     2,     2,     2,     1,     1,     1,     3,     1,     1,
       1,     1,     1,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 7:
#line 144 "compiler_hw2.y" /* yacc.c:1646  */
    { create_symbol(); }
#line 1520 "y.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 145 "compiler_hw2.y" /* yacc.c:1646  */
    { dump_symbol(); }
#line 1526 "y.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 148 "compiler_hw2.y" /* yacc.c:1646  */
    {for_flag = 0;}
#line 1532 "y.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 154 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        if(lookup_symbol((yyvsp[-1].s_val)) == 0){
            AddressNum++;
            insert_symbol((yyvsp[-1].s_val), (yyvsp[0].s_val), AddressNum, yylineno, "-");
            lookup_symbol((yyvsp[-1].s_val));
            decl_flag = 0;
        }
        else
        {   
            sematic_error((yyvsp[-1].s_val), StoreLineno);
        }
    }
#line 1549 "y.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 166 "compiler_hw2.y" /* yacc.c:1646  */
    {
        if(lookup_symbol((yyvsp[-3].s_val)) == 0){
            AddressNum++;
            insert_symbol((yyvsp[-3].s_val), (yyvsp[-2].s_val), AddressNum, yylineno, "-");
            lookup_symbol((yyvsp[-3].s_val));
            decl_flag = 0;
        }
        else
        {   
            sematic_error((yyvsp[-3].s_val), StoreLineno);
        }
    }
#line 1566 "y.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 178 "compiler_hw2.y" /* yacc.c:1646  */
    {
        if(lookup_symbol((yyvsp[-4].s_val)) == 0){
            AddressNum++;
            insert_symbol((yyvsp[-4].s_val), "array", AddressNum, yylineno+1, (yyvsp[0].s_val));
            decl_flag = 0;
        }
        else
        {   
            sematic_error((yyvsp[-4].s_val), StoreLineno);
        }
    }
#line 1582 "y.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 196 "compiler_hw2.y" /* yacc.c:1646  */
    {
        if(strcmp((yyvsp[0].s_val), "bool") !=0)
        {
            if((strcmp((yyvsp[0].s_val), "int32") ==0) || (strcmp((yyvsp[0].s_val), "float32") ==0))
                printf("error:%d: non-bool (type %s) used as for condition\n",yylineno+1, (yyvsp[0].s_val));
            else
            {
                if(lookup_symbol((yyvsp[0].s_val)) == 1)
                    printf("error:%d: non-bool (type %s) used as for condition\n",yylineno+1, StoreType);
            }
        }
    }
#line 1599 "y.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 211 "compiler_hw2.y" /* yacc.c:1646  */
    {
        if(strcmp((yyvsp[0].s_val), "bool") !=0)
        {
            if((strcmp((yyvsp[0].s_val), "int32") ==0) || (strcmp((yyvsp[0].s_val), "float32") ==0))
                printf("error:%d: non-bool (type %s) used as for condition\n",yylineno+1, (yyvsp[0].s_val));
            else
            {
                if(lookup_symbol((yyvsp[0].s_val)) == 1)
                    printf("error:%d: non-bool (type %s) used as for condition\n",yylineno+1, StoreType);
            }
        }
    }
#line 1616 "y.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 231 "compiler_hw2.y" /* yacc.c:1646  */
    {
        if(str_flag)
            printf("PRINT string\n");
    }
#line 1625 "y.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 235 "compiler_hw2.y" /* yacc.c:1646  */
    { if(lookup_symbol((yyvsp[-1].s_val)) == 1)
            {
                
                printf("%s (name=%s, address=%d)\n", "IDENT", StoreName, StoreAddress);
                printf("PRINT %s\n",StoreType);
            }}
#line 1636 "y.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 241 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        if(!for_flag){
            if(lookup_symbol((yyvsp[-1].s_val)) == 1)
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
#line 1655 "y.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 255 "compiler_hw2.y" /* yacc.c:1646  */
    {if(lookup_symbol((yyvsp[-4].s_val)) == 1)
            {
                printf("PRINTLN %s\n",StoreElementType);
            }}
#line 1664 "y.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 259 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        
        if(id_flag)
        {
            if(lookup_symbol((yyvsp[-1].s_val)) == 1)
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
#line 1687 "y.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 280 "compiler_hw2.y" /* yacc.c:1646  */
    { (yyval.s_val) = (yyvsp[-1].s_val);}
#line 1693 "y.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 281 "compiler_hw2.y" /* yacc.c:1646  */
    {
        if(id_flag)
        {
        if(lookup_symbol((yyvsp[-1].s_val)) == 1){
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
        if(strcmp((yyvsp[-3].s_val), "float32") == 0)
                printf("F\n");
        else if(strcmp((yyvsp[-3].s_val), "int32") == 0)
                printf("I\n");
    }
#line 1720 "y.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 304 "compiler_hw2.y" /* yacc.c:1646  */
    {
        type_error((yyvsp[-2].s_val), (yyvsp[0].s_val), "ASSIGN");
        printf("%s\n", "ASSIGN");  
    }
#line 1729 "y.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 308 "compiler_hw2.y" /* yacc.c:1646  */
    {
        if(strcmp((yyvsp[-2].s_val),"int32") == 0)
            printf("error:%d: cannot assign to int32\n", yylineno);
        else if(strcmp((yyvsp[-2].s_val),"float32") == 0)
            printf("error:%d: cannot assign to float32\n", yylineno);
        printf("%s\n", "ADD_ASSIGN");  
    }
#line 1741 "y.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 315 "compiler_hw2.y" /* yacc.c:1646  */
    {
        if(strcmp((yyvsp[-2].s_val),"int32") == 0)
            printf("error:%d: cannot assign to int32\n", yylineno);
        else if(strcmp((yyvsp[-2].s_val),"float32") == 0)
            printf("error:%d: cannot assign to float32\n", yylineno);
        printf("%s\n", "SUB_ASSIGN");  
    }
#line 1753 "y.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 322 "compiler_hw2.y" /* yacc.c:1646  */
    {
        if(strcmp((yyvsp[-2].s_val),"int32") == 0)
            printf("error:%d: cannot assign to int32\n", yylineno);
        else if(strcmp((yyvsp[-2].s_val),"float32") == 0)
            printf("error:%d: cannot assign to float32\n", yylineno);
        printf("%s\n", "MUL_ASSIGN");  
    }
#line 1765 "y.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 329 "compiler_hw2.y" /* yacc.c:1646  */
    {
        if(strcmp((yyvsp[-2].s_val),"int32") == 0)
            printf("error:%d: cannot assign to int32\n", yylineno);
        else if(strcmp((yyvsp[-2].s_val),"float32") == 0)
            printf("error:%d: cannot assign to float32\n", yylineno);
        printf("%s\n", "QUO_ASSIGN");  
    }
#line 1777 "y.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 336 "compiler_hw2.y" /* yacc.c:1646  */
    {
        if(strcmp((yyvsp[-2].s_val),"int32") == 0)
            printf("error:%d: cannot assign to int32\n", yylineno);
        else if(strcmp((yyvsp[-2].s_val),"float32") == 0)
            printf("error:%d: cannot assign to float32\n", yylineno);
        printf("%s\n", "REM_ASSIGN");  
    }
#line 1789 "y.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 343 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        type_error((yyvsp[-2].s_val), (yyvsp[0].s_val), "ADD");
        printf("%s\n", "ADD"); 
    }
#line 1798 "y.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 347 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        type_error((yyvsp[-2].s_val), (yyvsp[0].s_val), "SUB");
        printf("%s\n", "SUB"); 
    }
#line 1807 "y.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 351 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        printf("%s\n", "MUL"); 
    }
#line 1815 "y.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 354 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        printf("%s\n", "QUO"); 
    }
#line 1823 "y.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 357 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        if((strcmp((yyvsp[-2].s_val), "float32") == 0) || (strcmp((yyvsp[0].s_val), "float32") == 0))
            printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "REM","float32");
        else
        {
            if(lookup_symbol((yyvsp[-2].s_val)) == 1)
            {
                if((strcmp(StoreType, "float32") == 0))
                    printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "REM","float32");
                else
                {
                    if(lookup_symbol((yyvsp[0].s_val)) == 1)
                    {
                        if((strcmp(StoreType, "float32") == 0))
                            printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "REM","float32");
                    }
                }
            }
        }
        printf("%s\n", "REM"); 
    }
#line 1849 "y.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 378 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        (yyval.s_val) = "bool";
        printf("%s\n", "GTR"); 
    }
#line 1858 "y.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 382 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        (yyval.s_val) = "bool";
        printf("%s\n", "LSS"); 
    }
#line 1867 "y.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 386 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        (yyval.s_val) = "bool";
        printf("%s\n", "EQL"); 
    }
#line 1876 "y.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 390 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        (yyval.s_val) = "bool";
        printf("%s\n", "NEQ"); 
    }
#line 1885 "y.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 394 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        (yyval.s_val) = "bool";
        printf("%s\n", "LEQ"); 
    }
#line 1894 "y.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 398 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        (yyval.s_val) = "bool";
        printf("%s\n", "GEQ"); 
    }
#line 1903 "y.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 402 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        if((strcmp((yyvsp[-2].s_val), "int32") == 0) || (strcmp((yyvsp[0].s_val), "int32") == 0))
            printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "LOR","int32");
        else
        {
            if(lookup_symbol((yyvsp[-2].s_val)) == 1)
            {
                if((strcmp(StoreType, "int32") == 0))
                    printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "LOR","int32");
                else
                {
                    if(lookup_symbol((yyvsp[0].s_val)) == 1)
                    {
                        if((strcmp(StoreType, "int32") == 0))
                            printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "LOR","int32");
                    }
                }
            }
        }
        printf("%s\n", "LOR"); 
    }
#line 1929 "y.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 423 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        if((strcmp((yyvsp[-2].s_val), "int32") == 0) || (strcmp((yyvsp[0].s_val), "int32") == 0))
            printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "LAND","int32");
        else
        {
            if(lookup_symbol((yyvsp[-2].s_val)) == 1)
            {
                if((strcmp(StoreType, "int32") == 0))
                    printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "LAND","int32");
                else
                {
                    if(lookup_symbol((yyvsp[0].s_val)) == 1)
                    {
                        if((strcmp(StoreType, "int32") == 0))
                            printf("error:%d: invalid operation: (operator %s not defined on %s)\n", yylineno, "LAND","int32");
                    }
                }
            }
        }
        printf("%s\n", "LAND"); 
    }
#line 1955 "y.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 444 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        (yyval.s_val) = (yyvsp[0].s_val);
        printf("%s\n", "NEG"); 
    }
#line 1964 "y.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 448 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        (yyval.s_val) = (yyvsp[0].s_val);
        printf("%s\n", "POS"); 
    }
#line 1973 "y.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 452 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        (yyval.s_val) = (yyvsp[0].s_val);
        printf("%s\n", "NOT"); 
    }
#line 1982 "y.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 456 "compiler_hw2.y" /* yacc.c:1646  */
    {
        printf("%s\n", "INC");
    }
#line 1990 "y.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 459 "compiler_hw2.y" /* yacc.c:1646  */
    {
        printf("%s\n", "DEC");   
    }
#line 1998 "y.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 466 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("%s %d\n", "INT_LIT", (yyvsp[0].i_val)); (yyval.s_val) = "int32"; id_flag = 0; str_flag = 0; int_flag = 1; float_flag = 0; }
#line 2004 "y.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 467 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("%s %f\n", "FLOAT_LIT", (yyvsp[0].f_val)); (yyval.s_val) = "float32";id_flag = 0; str_flag = 0; int_flag = 0; float_flag = 1;}
#line 2010 "y.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 468 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("%s %s\n", "STRING_LIT", (yyvsp[-1].s_val)); id_flag = 0; str_flag = 1; int_flag = 0; float_flag = 0;}
#line 2016 "y.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 469 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("%s\n", "TRUE"); (yyval.s_val) = "TRUE";id_flag = 0; str_flag = 0; int_flag = 0; float_flag = 0;}
#line 2022 "y.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 470 "compiler_hw2.y" /* yacc.c:1646  */
    { printf("%s\n", "FALSE"); (yyval.s_val) = "FALSE";id_flag = 0; str_flag = 0; int_flag = 0; float_flag = 0;}
#line 2028 "y.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 471 "compiler_hw2.y" /* yacc.c:1646  */
    { 
        id_flag = 1; str_flag = 0; int_flag = 0; float_flag = 0;
        if(!for_flag)
        {
            if(lookup_symbol((yyvsp[0].s_val)) == 0){
                sematic_error((yyvsp[0].s_val), 0);
            }
            else{           
                printf("%s (name=%s, address=%d)\n", "IDENT", StoreName, StoreAddress);
            
            }
        }
        else{
            lookup_symbol((yyvsp[0].s_val));
            printf("%s (name=%s, address=%d)\n", "IDENT", StoreName, StoreAddress);
        }
    }
#line 2050 "y.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 491 "compiler_hw2.y" /* yacc.c:1646  */
    { (yyval.s_val) = "int32";}
#line 2056 "y.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 492 "compiler_hw2.y" /* yacc.c:1646  */
    { (yyval.s_val) = "float32";}
#line 2062 "y.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 493 "compiler_hw2.y" /* yacc.c:1646  */
    { (yyval.s_val) = "string";}
#line 2068 "y.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 494 "compiler_hw2.y" /* yacc.c:1646  */
    { (yyval.s_val) = "bool";}
#line 2074 "y.tab.c" /* yacc.c:1646  */
    break;


#line 2078 "y.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 497 "compiler_hw2.y" /* yacc.c:1906  */


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
