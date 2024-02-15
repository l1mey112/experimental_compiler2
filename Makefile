# such a simple thing that should be built in to the compiler.
# it isn't though, so here it is.

TARGET := a.out

BUILD_DIR := ./build

SRCS := $(shell find . -maxdepth 1 -name '*.c')
OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)

DEPS := $(OBJS:.o=.d)
CFLAGS := -MMD -MP -O0 -ggdb -std=gnu2x -fsanitize=undefined,address

# debian as usual ships an extremely old version of GCC
ifeq ($(shell lsb_release -si 2>/dev/null),Debian)
	CC := clang
endif

_ := $(shell mkdir -p $(BUILD_DIR))

$(TARGET): $(OBJS)
	$(CC) $(OBJS) -o $@ $(CFLAGS)

$(BUILD_DIR)/%.c.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -r $(BUILD_DIR) $(TARGET)

-include $(DEPS)
