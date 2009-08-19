CC=gcc
CCFLAGS=-Wall
LIBS=-liksemel

all:myroster

myroster: myroster.c
	${CC} ${CCFLAGS} ${LIBS} $< -o $@

clean:
	rm -f iksroster myroster *.o
