DEST=/users/malaise/bin

# pexec_exe is a dummy target to always execute amake
#  and do it BEFORE evaluating $(DEST)/pexec versus pexec
all : pexec_exe $(DEST)/pexec

pexec_exe :
	amake pexec.adb

$(DEST)/pexec : pexec
	cp pexec $(DEST)

clean :
	rm *.o *.ali

clean_exe:
	rm pexec

