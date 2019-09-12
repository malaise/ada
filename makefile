include $(if $(MAKFILES_DIR), $(MAKFILES_DIR), $(HOME)/Makefiles)/common.mk

SUBDIRS := cots

include $(TEMPLATES)/dir.mk

