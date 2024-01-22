# Z80 Emulator
A header-only Z80 cpu emulator written in C++. I also tried experimenting with templates this project. Every instruction is templated on addressing mode so each combination should have its own templated function generated by the compiler.

## Usage
### Dependencies 
* **A c++ compiler**
* **CMake 3.26+**
* **A CMake build generator**

### Documentation
See [header file](include/z80.h)

### Adding to project
Using CMake's `FetchContent`, add this to your CMakeLists.txt file:
```angular2html

```

## Running tests
Add the required files to [tests-roms](tests/test-roms) and (optionally) [unit-tests](tests/unit-tests).
Logging can be enabled in [driver.cpp](tests/driver.cpp).

### With CMake
```angular2html
git clone https://github.com/harrow22/z80.git
cd z80/
cmake -S . -B build -G your_generator -DCMAKE_BUILD_TYPE=RELEASE
cmake --build build --config Release
build/tests/Intel8080_test.exe
```

By default, it will write output to stdout. Running it on my machine produces this:
```angular2html

```

## Resources
* [Z80 CPU User Manual](https://www.zilog.com/force_download.php?filepath=YUhSMGNEb3ZMM2QzZHk1NmFXeHZaeTVqYjIwdlpHOWpjeTk2T0RBdlZVMHdNRGd3TG5Ca1pnPT0=)
* [Z80 Opcode Table](https://clrhome.org/table/)
* [Z80 Heaven Opcode Reference](http://z80-heaven.wikidot.com/opcode-reference-chart)
* [The Undocumented Z80 Documented](http://www.z80.info/zip/z80-documented.pdf)
* [Z80 Extended Instruction Set (for addressing modes)](https://wiki.specnext.dev/Extended_Z80_instruction_set)
* [Z80 Instruction Set (for undocumented flag calculation)](https://web.archive.org/web/20231220151112/https://wikiti.brandonw.net/index.php?title=Z80_Instruction_Set)