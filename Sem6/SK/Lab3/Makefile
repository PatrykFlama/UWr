CC = g++
CFLAGS = -Wall -Wextra -std=c++17
TARGET = transport
SRC = main.cpp
OBJ = $(SRC:.cpp=.o)

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^

%.o: %.cpp
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm $(OBJ)

distclean: clean
	rm $(TARGET)
