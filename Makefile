all:
	stack build
	stack install --local-bin-path .

clean:
	stack clean

fclean: clean
	rm funEvalExpr

re: fclean all

.PHONY: all clean fclean re