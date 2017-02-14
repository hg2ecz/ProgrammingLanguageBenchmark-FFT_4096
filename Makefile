all:
	for i in */; do make -C $$i; done

clean:
	for i in */; do make -C $$i clean; done

test:
	for i in */; do make -C $$i test; done

test2:
	for i in */; do make -C $$i test2; done
