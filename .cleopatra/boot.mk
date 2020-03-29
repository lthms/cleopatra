init :
	@echo "    init  cleopatra"
	@cleopatra-run-elisp "cleopatra-gen-proc.el" 2> build.log

-include ${CLEOPATRA_DIRECTORY}/deps.mk

prebuild :
build : prebuild
postbuild : build

.PHONY : init prebuild build postbuild
