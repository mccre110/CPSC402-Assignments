# the source files to be built
SOURCES := CPP.cf
all:
	bnfc -haskell $(SOURCES)
	happy -gcai ParCPP.y
	alex -g LexCPP.x
	ghc --make TestCPP.hs -o TestCPP

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocCPP.* LexCPP.* ParCPP.* LayoutCPP.* SkelCPP.* PrintCPP.* TestCPP.* AbsCPP.* TestCPP ErrM.* SharedString.* ComposOp.* CPP.dtd XMLCPP.*
