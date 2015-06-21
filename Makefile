.PHONY: adazmq examples

default: adazmq

adazmq:
	gprbuild -m -s -j0 -P adazmq.gpr

examples:
	$(MAKE) -C ./examples
