all: eth0

FILES=bot.ml bot2.ml

eth0: $(FILES)
	@echo "Compiling..."
	corebuild -package str bot.native
	corebuild -package str bot2.native

check: $(FILES)
	chmod u+x ../check_width
	../check_width bot.ml
	../check_width bot2.ml

clean:
	rm -rf _build *.native
