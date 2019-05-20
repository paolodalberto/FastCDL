.c.o:
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -c $< -o $@
.c.d:
	@echo "making $@ from $<"
	@gcc -std=gnu99 -MM $(ALL_CPPFLAGS) $< > $@
