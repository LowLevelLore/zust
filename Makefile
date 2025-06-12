CXX := g++
CXXFLAGS := -std=c++20 -Wall -Wextra -Iinclude -g -O2 -Werror -Wreorder -D_FORTIFY_SOURCE=2 -D_GLIBCXX_ASSERTIONS -fstack-protector-strong -fstack-clash-protection -Wpedantic -pedantic

# Directories
SRC_DIR := src
OBJ_DIR := build
BIN := zpiler

# Source files
SRCS := $(shell find $(SRC_DIR) -name '*.cpp') main.cpp
OBJS := $(SRCS:%.cpp=$(OBJ_DIR)/%.o)

# Default target
all: $(BIN)

# Binary build rule
$(BIN): $(OBJS)
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -o $@ $^

# Object build rule
$(OBJ_DIR)/%.o: %.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Clean
clean:
	rm -rf $(OBJ_DIR) $(BIN)

# Run
run: $(BIN)
	./$(BIN)

.PHONY: all clean run