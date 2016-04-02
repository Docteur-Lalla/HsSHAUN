BINDIR=bin
SRCDIR=src
OBJS=\
$(BINDIR)/Shaun/Data/Type.o \
$(BINDIR)/Shaun/Syntax/Comment.o \
$(BINDIR)/Shaun/Syntax/Parser.o \

TESTDIR=tests
TESTOBJS=\
$(BINDIR)/tests/comment.o \

TESTS=\
$(BINDIR)/tests/comment \

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

# Tests
$(BINDIR)/tests/%.o: $(TESTDIR)/%.hs
	$(HC) -o $@ -c $< -i"$(BINDIR)" -outputdir $(BINDIR)/tests

$(BINDIR)/tests/comment: $(BINDIR)/tests/comment.o bin/Shaun/Syntax/Comment.o
	$(HC) -o $@ -i"$(BINDIR)" $^
