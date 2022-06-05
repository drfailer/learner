learner: learner.hs
		ghc -dynamic learner.hs

clean:
	rm -rf learner.hi
	rm -rf learner.o

clean-all:
	rm -rf learner.hi
	rm -rf learner.o
	rm -rf learner
