all:
	ocp-build tvshow-rename

install:
	ocp-build install tvshow-rename

clean:
	rm -rf *~ *#
	ocp-build clean
