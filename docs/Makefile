# Change this to whatever lisp you'r using
LISP=openmcl
EVAL=-e
QUIT=(ccl:quit)
SYSTEM=qbook

docs: pdf html

html:
	mkdir -p html/
	${LISP} ${EVAL} "(asdf:oos 'asdf:load-op :qbook)" \
	        ${EVAL} "(asdf:oos 'asdf:load-op :${SYSTEM})" \
		${EVAL} "(asdf:oos 'qbook:publish-op :qbook \
                                   :generator (make-instance 'qbook:html-generator \
                                                             :output-directory \"./html/\" \
                                                             :title \"${SYSTEM}\"))" \
		${EVAL} "${QUIT}"

pdf:
	mkdir -p pdf/
	${LISP} ${EVAL} "(asdf:oos 'asdf:load-op :qbook)" \
                ${EVAL} "(asdf:oos 'asdf:load-op :${SYSTEM})" \
		${EVAL} "(asdf:oos 'qbook:publish-op :qbook \
                                   :generator (make-instance 'qbook:latex-generator \
                                                             :output-file \"./pdf/${SYSTEM}.tex\" \
                                                             :title \"${SYSTEM}\"))" \
		${EVAL} "${QUIT}"
	(cd pdf && pdflatex ${SYSTEM}.tex)
	(cd pdf && pdflatex ${SYSTEM}.tex)
	rm pdf/${SYSTEM}.aux pdf/${SYSTEM}.log pdf/${SYSTEM}.toc pdf/${SYSTEM}.tex
