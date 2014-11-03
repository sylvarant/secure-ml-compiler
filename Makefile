#########################################
#     Custom MakeFile                   #
# By Adriaan Larmuseau                  #
#########################################

OUTPUT_DIR = bin
COMPILER_DIR = src
COMPILER_BIN = main.native

#============================================
# compiler
#============================================
compiler: 
	$(MAKE) -C $(COMPILER_DIR)


#============================================
# extra's
#============================================

setup:
	-mkdir out
	-mkdir setup

test:
	@./test


.PHONY: test setup compiler
