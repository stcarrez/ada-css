NAME=cssada

-include Makefile.conf

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XCSS_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XCSS_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_BASE_BUILD=relocatable -XUTIL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable

include Makefile.defaults

sharedir=${prefix}/share
configdir=bindir?=${prefix}/share/csstools

# Build executables for all mains defined by the project.
build-test::	setup
	$(GNATMAKE) $(GPRFLAGS) -p -P$(NAME)_tests $(MAKE_ARGS)

build:: tools

tools:  tools/css-tools-configs.ads
	$(GNATMAKE) $(GPRFLAGS) -p -P$(NAME)_tools $(MAKE_ARGS)

tools/css-tools-configs.ads:   Makefile.conf tools/css-tools-configs.gpb
	gnatprep -DCONFIG_DIR='"${configdir}"' -DVERSION='"${VERSION}"' \
		  tools/css-tools-configs.gpb tools/css-tools-configs.ads

clean::
	rm tools/css-tools-configs.ads

install::
	mkdir -p $(DESTDIR)$(prefix)/bin $(DESTDIR)$(sharedir)/csstools
	$(INSTALL) bin/csstools $(DESTDIR)$(prefix)/bin/csstools
	-rm -rf $(DESTDIR)${sharedir}/csstools
	$(CP) -r config $(DESTDIR)${sharedir}/csstools

# Build and run the unit tests
test:	build
	$(GNATMAKE) $(MAKE_ARGS) -p -Pcssada_tests
	bin/css_harness -xml css-aunit.xml

clean::
	rm -f stamp-test-setup tests.log

$(eval $(call ada_library,$(NAME)))

DIST_DIRS=ada-util
dist::
	rm -f $(DIST_FILE)
	git archive -o $(DIST_DIR).tar --prefix=$(DIST_DIR)/ HEAD
	for i in $(DIST_DIRS); do \
	   cd $$i && git archive -o ../$$i.tar --prefix=$(DIST_DIR)/$$i/ HEAD ; \
           cd .. && tar --concatenate --file=$(DIST_DIR).tar $$i.tar ; \
           rm -f $$i.tar; \
        done
	gzip $(DIST_DIR).tar

# Development targets that requires ayacc and aflex to be installed.
# Do not call these unless you modify the lex/yacc grammar.
parser:	
	cd src/parser && \
	   ayacc -n 256 -k -s -e .ada css-parser-parser.y && \
	   gnatchop -w css-parser-parser.ada && \
	   rm -f css-parser-parser.ada
	#-rm -f src/parser/css-parser-lexer_io.ads
	#-rm -f src/parser/css-parser-lexer_io.adb
	-rm -f src/parser/css-parser-parser.verbose

lexer:
	cd src/parser; \
	/src/awa/aflex/bin/aflex -ms -L css-parser-lexer.l
	cd src/parser && \
	  gnatchop -w css-parser-lexer.ada && \
	  rm -f css-parser-lexer.ada

parser-tools:
	cd tools/parser && \
	   ayacc -n 256 -k -s -E -v -e .ada css-analysis-parser-parser.y && \
	   gnatchop -w css-analysis-parser-parser.ada && \
	   rm -f css-analysis-parser-parser.ada
	#-rm -f src/parser/css-parser-lexer_io.ads
	#-rm -f src/parser/css-parser-lexer_io.adb
	-rm -f tools/parser/css-analysis-parser-parser.verbose

lexer-tools:
	cd tools/parser; \
	aflex -ms -L css-analysis-parser-lexer.l
	cd tools/parser && \
	  gnatchop -w css-analysis-parser-lexer.ada && \
	  rm -f css-analysis-parser-lexer.ada

install-tool:
	${MKDIR} -p ${bindir} ${sharedir}/csstools
	$(INSTALL) bin/csstools ${bindir}/csstools
	-rm -rf ${sharedir}/csstools
	$(CP) -r config ${sharedir}/csstools

.PHONY: tools
