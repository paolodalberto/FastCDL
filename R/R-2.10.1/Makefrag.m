.m.o:
	$(OBJC) $(ALL_CPPFLAGS) $(ALL_OBJCFLAGS) -c $< -o $@
.m.d:
	@echo "making $@ from $<"
	@gcc -MM $(ALL_CPPFLAGS) $< > $@
