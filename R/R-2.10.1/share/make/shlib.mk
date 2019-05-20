## ${R_HOME}/share/make/shlib.mk

include $(R_HOME)/etc${R_ARCH}/Makeconf

all: $(SHLIB)

$(SHLIB): $(OBJECTS)
	@if test  "z$(OBJECTS)" != "z"; then \
	  echo $(SHLIB_LINK) -o $@ $(OBJECTS) $(ALL_LIBS); \
	  $(SHLIB_LINK) -o $@ $(OBJECTS) $(ALL_LIBS); \
	fi

.PHONY: all shlib-clean

shlib-clean:
	@rm -rf .libs _libs
	@rm -f $(OBJECTS)
