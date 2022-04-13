all:
	npm install
	npm run build

clean:
	rm *.js

package: all
	npm run package

PROG=node dist/benchmark.js
test:
	$(PROG)
	@($(PROG);$(PROG);$(PROG);$(PROG);$(PROG);  $(PROG);$(PROG);$(PROG);$(PROG);$(PROG) )|../avgcalc.py
