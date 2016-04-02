BINDIR=bin
SRCDIR=src
OBJS=\
$(BINDIR)/Shaun/Data/Type.o \
$(BINDIR)/Shaun/Syntax/Comment.o \
$(BINDIR)/Shaun/Syntax/Parser.o \
$(BINDIR)/Shaun/IO.o \

TESTDIR=tests
TESTOBJS=\
$(BINDIR)/tests/comment.o \
$(BINDIR)/tests/parser.o \
$(BINDIR)/tests/io.o \

TESTS=\
$(BINDIR)/tests/comment \
$(BINDIR)/tests/parser \
$(BINDIR)/tests/io \

HC=ghc
HFLAGS=-outputdir $(BINDIR) -i"$(BINDIR)"
LDFLAGS=
LIB=hsshaun

all: $(OBJS) $(TESTS)

$(BINDIR)/%.o: $(SRCDIR)/%.hs
	$(HC) -c $< $(HFLAGS)

# Dependencies
$(BINDIR)/%.hi: $(BINDIR)/%.o
$(BINDIR)/Shaun/Syntax/Parser.o: $(BINDIR)/Shaun/Syntax/Comment.hi $(BINDIR)/Shaun/Data/Type.hi
$(BINDIR)/Shaun/IO.o: $(BINDIR)/Shaun/Syntax/Parser.hi $(BINDIR)/Shaun/Data/Type.hi

# Tests
$(BINDIR)/tests/%.o: $(TESTDIR)/%.hs
	$(HC) -o $@ -c $< -i"$(BINDIR)" -outputdir $(BINDIR)/tests

$(BINDIR)/tests/comment: $(BINDIR)/tests/comment.o $(BINDIR)/Shaun/Syntax/Comment.o
	$(HC) -o $@ -i"$(BINDIR)" $^

$(BINDIR)/tests/parser:\
$(BINDIR)/Shaun/Data/Type.o \
$(BINDIR)/Shaun/Syntax/Comment.o \
$(BINDIR)/Shaun/Syntax/Parser.o \
$(BINDIR)/tests/parser.o
	$(HC) -package parsec -o $@ -i"$(BINDIR)" $^

$(BINDIR)/tests/io:\
$(BINDIR)/Shaun/Data/Type.o \
$(BINDIR)/Shaun/Syntax/Comment.o \
$(BINDIR)/Shaun/Syntax/Parser.o \
$(BINDIR)/Shaun/IO.o \
$(BINDIR)/tests/io.o
	$(HC) -package parsec -o $@ -i"$(BINDIR)" $^
