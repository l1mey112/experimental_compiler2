# such a simple thing that should be built in to the compiler.
# it isn't though, so here it is.

TARGET := a.out

BUILD_DIR := ./build

SRCS := $(shell find . -maxdepth 1 -name '*.c')
OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)

DEPS := $(OBJS:.o=.d)
CFLAGS := -MMD -MP -O0 -ggdb -fsanitize=undefined,address

$(TARGET): $(OBJS)
	$(CC) $(OBJS) -o $@ $(CFLAGS)

$(BUILD_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -r $(BUILD_DIR) $(TARGET)

-include $(DEPS)
