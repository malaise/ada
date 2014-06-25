include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

SUBDIRS := c reposit usr

TXT := HISTORY README
DOCOPTS_README := -a toc

include $(TEMPLATES)/dir.mk

