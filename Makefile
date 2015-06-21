.PHONY: adazmq clean examples

default: adazmq

adazmq:
	gprbuild -m -s -j0 -P adazmq.gpr

clean:
	gprclean -P adazmq.gpr

examples:
	$(MAKE) -C ./examples
