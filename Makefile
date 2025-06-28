# Makefile for building the parser and test program.

GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : Compiler
	happy -gca ParLinguaggio.y
	alex -g LexLinguaggio.x
	ghc --make AbsLinguaggio.hs Compiler.hs ErrM.hs LexLinguaggio.hs ParLinguaggio.hs PrettyPrinter.hs PrettyPrinterTAC.hs PrintLinguaggio.hs SkelLinguaggio.hs TACabssynt.hs TACGen.hs TACMonad.hs -o linguaggio
	-rm -f *.log *.aux *.dvi *.bak

# Rules for building the parser.

#AbsLinguaggio.hs LexLinguaggio.x ParLinguaggio.y PrettyPrinter.hs PrintLinguaggio.hs TestLinguaggioOld.hs: linguaggio.cf
#	bnfc --haskell linguaggio.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

Compiler : AbsLinguaggio.hs LexLinguaggio.hs ParLinguaggio.hs PrettyPrinter.hs PrintLinguaggio.hs
	${GHC} ${GHC_OPTS} $@

# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

distclean : clean
	-rm -f AbsLinguaggio.hs AbsLinguaggio.hs.bak ComposOp.hs ComposOp.hs.bak DocLinguaggio.txt DocLinguaggio.txt.bak ErrM.hs ErrM.hs.bak LayoutLinguaggio.hs LayoutLinguaggio.hs.bak LexLinguaggio.x LexLinguaggio.x.bak ParLinguaggio.y ParLinguaggio.y.bak PrintLinguaggio.hs PrintLinguaggio.hs.bak SkelLinguaggio.hs SkelLinguaggio.hs.bak XMLLinguaggio.hs XMLLinguaggio.hs.bak ASTLinguaggio.agda ASTLinguaggio.agda.bak ParserLinguaggio.agda ParserLinguaggio.agda.bak IOLib.agda IOLib.agda.bak Main.agda Main.agda.bak linguaggio.dtd linguaggio.dtd.bak LexLinguaggio.hs ParLinguaggio.hs ParLinguaggio.info ParDataLinguaggio.hs Makefile

# EOF