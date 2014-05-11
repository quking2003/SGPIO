.SUFFIXES: .asm .hex .inc

CC = gpasm
#CC_OPTION = -p p16F648a -w 2 -r dec
#sgpio_p16f648a.hex: pic16macro.inc
#CC_OPTION = -p p12F683 -w 2 -r dec
#sgpio_p12f683.hex: pic16macro.inc
#CC_OPTION = -p p16F688 -w 2 -r dec
#sgpio_p16f688.hex: pic16macro.inc
#CC_OPTION = -p p16F819 -w 2 -r dec
#sgpio_p16f819.hex: pic16macro.inc
CC_OPTION = -p p16F1827 -w 2 -r dec
sgpio_p16f1827.hex: pic16macro.inc

#sgpio.hex: sgpio.asm
#	$(CC) $<


#.hex.asm:
.asm.hex:
	$(CC) $(CC_OPTION) $<

clean:
	rm -rf sgpio_p12f683.hex sgpio_p16f648a.hex sgpio_p16f688.hex sgpio_p16f819.hex sgpio_p16f1827.hex
	rm -rf sgpio_p12f683.lst sgpio_p16f648a.lst sgpio_p16f688.lst sgpio_p16f819.lst sgpio_p16f1827.lst
	rm -rf sgpio_p12f683.cod sgpio_p16f648a.cod sgpio_p16f688.cod sgpio_p16f819.cod sgpio_p16f1827.cod
	rm -rf *~

