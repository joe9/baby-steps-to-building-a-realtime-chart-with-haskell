include config.mk

.POSIX:
.SUFFIXES: .c .o

SRC = chapter.1.c chapter.2.c chapter.2.3.c chapter.2.4.c chapter.3.c chapter.3.2.c chapter.4.c use_gl_polygon.c step_01_color_green.c step_02_x_y_vertices.c step_03_performance_test.c using_color_uniform.c mimic_haskell_program.c
SCRIPTS =

OBJ = $(SRC:.c=.o)
BIN = $(SRC:.c=)
DEPS = Utils.o

all: options $(DEPS) bin

options:
	@echo init build options:
	@echo "CFLAGS   = $(CFLAGS)"
	@echo "LDFLAGS  = $(LDFLAGS)"
	@echo "CC       = $(CC)"

bin:	$(DEPS) $(BIN)

scripts: $(SCRIPTS)

$(OBJ): config.mk

.o:
	@echo LD $@
	@$(LD) -o $@ $< $(DEPS) $(LDFLAGS)

.c.o:
	@echo CC $<
	@$(CC) -c -o $@ $< $(CFLAGS)

clean:
	@echo cleaning
	@rm -f $(BIN) $(OBJ) $(DEPS)
