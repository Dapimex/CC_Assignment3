bison -dy toy_lang.y
flex toy_lang.l
g++ y.tab.c lex.yy.c -o toy
toy < source.txt