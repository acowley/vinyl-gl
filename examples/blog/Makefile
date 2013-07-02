all:	Game2D.lhs
	ghc -O2 -i../src Game2D.lhs
	@echo "Run Game2D from the examples directory: blog/Game2D"
	cd ..; blog/Game2D

blog:	Game2D.lhs StyleTweak.css
	pandoc -s -H StyleTweak.css Game2D.lhs -o Game2D.html
	cat Game2D.html | pbcopy

.PHONY: clean blog

clean:
	rm -f Game2D.html *.o *.hi Game2D