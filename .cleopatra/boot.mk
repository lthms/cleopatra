init :
	cleopatra-run-elisp "cleopatra-gen-proc.el"
	make -f ${CLEOPATRA_DIRECTORY}/boot.mk postbuild

-include ${CLEOPATRA_DIRECTORY}/deps.mk

prebuild :
build : prebuild
postbuild : build

.PHONY : init prebuild build postbuild
