	

all : testA testB testC testD doc tex/relatorio.pdf
	
src/t1: src/t1.hs
	ghc src/t1.hs
	
src/t2: src/t2.hs
	ghc src/t2.hs
	
src/t3: src/t3.hs
	ghc src/t3.hs

src/t4: src/t4.hs
	ghc src/t4.hs

src/t5: src/t5.hs
	ghc src/t5.hs
	
testA: src/t1
	cd tests; bash runtests.sh tA ../src/t1
	
testB: src/t2
	cd tests; bash runtests.sh tB ../src/t2
	
testC: src/t3
	cd tests; bash runtests.sh tC ../src/t3

testD: src/t4
	cd tests; bash runtests.sh tD ../src/t4

doc: docA docB docC docD docE
	
docA: src/t1.hs
	haddock -h -o doc/TA src/t1.hs
	
docB: src/t2.hs
	haddock -h -o doc/TB src/t2.hs
	
docC: src/t3.hs
	haddock -h -o doc/TC src/t3.hs

docD: src/t4.hs
	haddock -h -o doc/TD src/t4.hs

docE: src/t5.hs
	haddock -h -o doc/TE src/t5.hs

tex/relatorio.pdf: tex/relatorio.tex
	cd tex; pdflatex relatorio.tex; pdflatex relatorio.tex

clean:
	rm -f src/t1.{hi,o} src/t2.{hi,o} src/t3.{hi,o} src/t4.{hi,o} src/t5.{hi,o}
	rm -f tex/relatorio.{aux,log,out,toc,lof}

realclean: clean
	rm -rf doc/TA doc/TB doc/TC doc/TD doc/TE src/t1 src/t2 src/t3 src/t4 src/t5
	rm -f tex/relatorio.pdf
