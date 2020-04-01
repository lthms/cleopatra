ARTIFACTS := build.log
CONFIGURE :=

init :
	@cleopatra echo "Exporting" "generation process"
	@cleopatra-run-elisp "${CLEOPATRA_DIRECTORY}/emacs.d/cleopatra-gen-proc.el" > build.log

-include ${CLEOPATRA_DIRECTORY}/deps.mk

prebuild :
build : prebuild
postbuild : build
	@cleopatra echo "Updating" ".gitignore"
	@cleopatra-update-gitignore ${CONFIGURE} ${ARTIFACTS}

.PHONY : init prebuild build postbuild
