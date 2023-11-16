all:
	@echo "Generating parser...\n"
	bnfc --functor -o parser Latte.cf
	@echo "Parser generated."
	@echo "Compiling parser...\n"
	@+$(MAKE) -C parser all
	@echo "Parser compiled.\n"

clean:
	@echo "Cleaning parser...\n"
	@+$(MAKE) -C parser clean
	@echo "Parser cleaned."