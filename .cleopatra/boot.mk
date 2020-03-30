ARTIFACTS := build.log
CONFIGURE :=

init :
	@echo "    init  cleopatra"
	@cleopatra-run-elisp "${CLEOPATRA_DIRECTORY}/emacs.d/cleopatra-gen-proc.el" 2> build.log

-include ${CLEOPATRA_DIRECTORY}/deps.mk

prebuild :
build : prebuild
postbuild : build
	@echo "  update  gitignore"
	@cleopatra-update-gitignore ${CONFIGURE} ${ARTIFACTS}

.PHONY : init prebuild build postbuild
