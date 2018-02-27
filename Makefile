LATEXMK = ltx2any -e xelatex

target = thesis
references := references.bib
includes := $(shell ls *.tex) ${references}

.PHONY: ${target}
${target}: ${target}.pdf ${target}.log.html

${target}.pdf: ${includes}
	${LATEXMK} ${target}

${target}.log.md: ${target}.pdf

${target}.log.html: ${target}.log.md
	pandoc $< > $@

.PHONY: preview
preview:
	${LATEXMK} -pvc ${target}

.PHONY: clean
clean:
	# ${RM} $(filter-out %.tex %.pdf,$(shell ls ${target}.*))
	# The following is occasionally necessary due to a nasty bug in biber.
	# ${RM} -r $(shell biber --cache)
	rm -rf ${target}_tmp

.PHONY: cleanall
cleanall: clean
	${RM} ${target}.pdf
