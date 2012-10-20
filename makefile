include $(HOME)/Makefiles/common.mk

SUBDIRS := c reposit usr cots

TXT := HISTORY README
DOCOPTS_README := -a toc

include $(TEMPLATES)/dir.mk

