#########################################
#     Custom MakeFile                   #
# By MYSTERY MAN                  #
#########################################

OUTPUT_DIR = bin
COMPILER_DIR = src
COMPILER_BIN = main.native


#============================================
# compiler
#============================================
compiler: 
	$(MAKE) -C $(COMPILER_DIR)

now: compiler

#============================================
# extra's
#============================================

setup:
	-mkdir out
	-mkdir log
	-mkdir coverage

test:
	@./test

report:
	ocveralls --prefix src/_build coverage/run*.out --send 

clean:
	$(MAKE) clean -C $(COMPILER_DIR)
	rm -fr out/*
	

.PHONY: test setup compiler clean now report
