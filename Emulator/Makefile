all:
	mkdir -p obj
	g++ -o obj/emulator emulator.cpp translate.cpp -g -std=c++11

clean:
	rm -rf obj/
	rm test_asm/data.hex
	rm test_asm/inst.hex
	rm test_asm/inst.asm
