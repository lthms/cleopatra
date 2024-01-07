ARTIFACTS := build.log
CONFIGURE := .cleopatra

PROCS := $(wildcard ${CLEOPATRA_GENERATION_PROCESSES}/*.org)

init :
	@cleopatra echo "Exporting" "generation process"
	@cleopatra-run-elisp "${CLEOPATRA_DIRECTORY}/emacs.d/cleopatra-gen-proc.el" \
	      >build.log 2>&1
	@cleopatra-gen-deps ${PROCS}

-include ${CLEOPATRA_DIRECTORY}/deps.mk

prebuild :
build : prebuild
postbuild : build

postbuild :
	@cleopatra echo "Updating" ".gitignore"
	@cleopatra-update-gitignore $(sort ${CONFIGURE} ${ARTIFACTS})
	@rm ${CLEOPATRA_DIRECTORY}/deps.mk

clean :
	@cleopatra echo Cleaning "Build artifacts"
	@rm -rf ${ARTIFACTS}
	@rm -rf .cleopatra/emacs.d/cache

cleanall : clean
	rm -rf ${CONFIGURE}

.PHONY : init prebuild build postbuild clean cleanall
