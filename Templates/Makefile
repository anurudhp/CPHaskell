%.out: %.hs
	ghc -o $@ $^

clean:
	rm *.o *.hi *.out -f
setup:
	cp template.hs a.hs
	cp template.hs b.hs
	cp template.hs c.hs
	cp template.hs d.hs
	cp template.hs e.hs

.PHONY: clean setup
