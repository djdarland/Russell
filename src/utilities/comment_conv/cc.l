%{
/* dummy routines to make lex happy without its library */
yywrap()
{
    return(1);
}

main()
{
    yylex();
}
%}
%%
"/*"    printf("(*");
"*/"    printf("*)");
