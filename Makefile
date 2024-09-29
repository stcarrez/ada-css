NAME=cssada
VERSION=1.0.0

DIST_DIR=ada-css-$(VERSION)
DIST_FILE=ada-css-$(VERSION).tar.gz

MAKE_ARGS += -XCSS_BUILD=$(BUILD)

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
build-test::	lib-setup
	cd regtests && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

build:: tools

tools:
	cd tools && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

clean::
	rm -f tools/css-tools-configs.ads

install::
	mkdir -p $(DESTDIR)$(prefix)/bin $(DESTDIR)$(sharedir)/csstools
	$(INSTALL) bin/csstools $(DESTDIR)$(prefix)/bin/csstools
	-rm -rf $(DESTDIR)${sharedir}/csstools
	$(CP) -r config $(DESTDIR)${sharedir}/csstools

# Build and run the unit tests
test:	build-test
	bin/css_harness -xml css-aunit.xml

clean::
	rm -f stamp-test-setup tests.log

$(eval $(call ada_library,$(NAME),.))

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
