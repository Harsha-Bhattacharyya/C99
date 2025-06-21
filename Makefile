install core/lexer/llexer.l:
	flex core/lexer/llexer.l
	gcc -lfl core/main.c
