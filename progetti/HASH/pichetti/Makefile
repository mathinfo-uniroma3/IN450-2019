#CC=/usr/bin/cc
CC=/usr/bin/gcc
LIBS=-lm

default: oracolo uno tre

oracolo: 
	cd oracles; make

uno: algoritmo1.c
	$(CC) -o algoritmo1 $(LIBS) algoritmo1.c

tre: Algoritmo3.c
	$(CC) -o algoritmo3 $(LIBS) algoritmo3.c

run-uno:
	echo "233\n2\n0.5" | ./algoritmo1  
