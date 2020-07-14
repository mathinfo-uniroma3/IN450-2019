#CC=/usr/bin/cc
CC=/usr/bin/gcc

byteoracle: byteoracle.c Sslprog/sha3-384.c 
	cd Sslprog ; make
	$(CC) -o byteoracle byteoracle.c


