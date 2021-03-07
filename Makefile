BUILD_DIR = build
EXE = raco exe -v -o $(BUILD_DIR)/litr src/main.rkt
DIST = raco distribute $(BUILD_DIR)/litr.d $(BUILD_DIR)/litr
INST = /usr/local/bin

all: litr

install: EXE_PATH = $(BUILD_DIR)/litr
install:
	cp -r $(BUILD_DIR)/litr.d $(INST)
	ln -s $(INST)/litr.d/bin/litr $(INST)/litr

uninstall:
	rm -r $(INST)/litr.d
	rm $(INST)/litr

distribute: litr
	$(DIST)
	
litr:
	@if [ -d "$(BUILD_DIR)" ] ; then\
		$(EXE) ;\
	else\
		mkdir $(BUILD_DIR) ; $(EXE) ;\
	fi

clean:
	@if [ -d "$(BUILD_DIR)" ] ; then\
		rm -r $(BUILD_DIR) ;\
	fi
