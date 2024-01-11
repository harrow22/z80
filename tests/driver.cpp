#include <fstream>
#include <iostream>
#include <format>
#include <array>
#include <chrono>
#include "z80.h"

static constexpr u16 start {0x100};
static constexpr int clockSpeed {static_cast<int>(2.5e6)}; // 2.5 MHz
static constexpr int cycles {clockSpeed / 60};

struct Memory {
    std::array<u8, 0x10000> ram {};
    bool exit {false};

    [[nodiscard]] u8 read8(const u16 addr) const { return ram[addr]; }
    [[nodiscard]] u16 read16(const u16 addr) const { return ram[addr + 1] << 8 | ram[addr]; }
    void write(const u16 addr, const u8 val) { ram[addr] = val; }
    void write(const u16 addr, const u16 val) { ram[addr] = val >> 8; ram[addr + 1] = val & 0xFF; }

    [[nodiscard]] u8 input(const z80<Memory>* cpu, const u8 port) const
    {
        if (const u8 operation {cpu->regs.c}; operation == 2) {
            // print a character stored in E
            std::cout << static_cast<char>(cpu->regs.e);
        } else if (operation == 9) {
            // print from memory at (DE) until '$' char
            std::uint16_t addr{z80<Memory>::pair(cpu->regs.d, cpu->regs.e)};
            do {
                std::cout << static_cast<char>(ram[addr++]);
            } while (ram[addr] != '$');
        }

        return 0xFFU;
    }
    void output(const z80<Memory>* cpu, const u8 port, const u8 val) { exit = true; }
};

int load(z80<Memory>& z, std::string path)
{
    std::ifstream file {path, std::ios::binary};

    if (file.fail()) {
        std::cerr << std::format("error: failure when opening file '{:s}'.\n", path);
        return -1;
    }

    if (!file.is_open()) {
        std::cerr << std::format("error: unable to open file '{:s}'.\n", path);
        return -2;
    }
    file.read(reinterpret_cast<char*>(&z.memory.ram[start]), z.memory.ram.size());

    return 0;
}

void log(z80<Memory>& z, unsigned long long cycle)
{
    std::cout << std::format(
        "PC: {:0>4X}, AF: {:0>4X}, BC: {:0>4X}, DE: {:0>4X}, HL: {:0>4X}, SP: {:0>4X}, IX: {:0>4X}, IY: {:0>4X}, I: {:0>2X}, R: {:0>2X}",
        z.pc,
        z80<Memory>::pair(z.a, z.flag()),
        z80<Memory>::pair(z.regs.b, z.regs.c),
        z80<Memory>::pair(z.regs.d, z.regs.e),
        z80<Memory>::pair(z.regs.h, z.regs.l),
        z.sp, z.ix, z.iy, z.i, z.r)
        << std::format("\t({:0>2X} {:0>2X} {:0>2X} {:0>2X}), cycle: {:d}\n",
            z.memory.ram[z.pc],
            z.memory.ram[z.pc + 1U],
            z.memory.ram[z.pc + 2U],
            z.memory.ram[z.pc + 3U],
            cycle
            );

}

std::string formatTime(const std::chrono::steady_clock::time_point begin, const std::chrono::steady_clock::time_point end)
{
    if (const auto ms {std::chrono::duration_cast<std::chrono::milliseconds>(end - begin)}; ms.count() >= 60 * 1e3L) {
        const std::string min {std::to_string(std::chrono::duration_cast<std::chrono::minutes>(ms).count())};
        std::string sec {std::to_string(std::chrono::duration_cast<std::chrono::seconds>(ms).count())};
        return  min + '.' + sec.erase(0, 1) + " min";
    } else {
        if (ms.count() >= 1e3L) {
            const std::string sec {std::to_string(std::chrono::duration_cast<std::chrono::seconds>(ms).count())};
            return  sec + '.' + std::to_string(ms.count()).erase(0, 1) + " sec";
        }
        return std::to_string(ms.count()) + " ms";
    }
}

void test(z80<Memory>& z, std::string rom, const unsigned long long expected)
{
    if (load(z, rom) < 0)
        return;

    std::cout << std::format("*** TEST: {:s}\n", rom);

    // inject output instruction at 0x0 to stop the test
    z.memory.ram[0x0] = 0xD3; // $D3 = out (n), a
    z.memory.ram[0x1] = 0x00; // (n) = 0

    // inject input instruction at 0x5 to output some characters
    z.memory.ram[0x5] = 0xDB; // $DB = in a, (n)
    z.memory.ram[0x6] = 0x00; // (n) = 0

    // inject ret instruction at 0x7 to return from syscall
    z.memory.ram[0x7] = 0xC9; // $C9 = ret

    // reset the cpu
    z.reset();
    z.pc = start; z.memory.exit = false;

    unsigned long long count {0}; // holds number of executed instructions
    unsigned long long executed {0}; // holds number of executed cycles
    const std::chrono::steady_clock::time_point begin {std::chrono::steady_clock::now()};

    // run the test
    log(z, 0);
    while(!z.memory.exit) {
        z.requested = cycles - z.requested;

        while(z.requested > 0) {
            const int tmp {z.requested};
            z.step();
            ++count;
            executed += tmp - z.requested;
            log(z, executed);
        }
    }

    // output results
    unsigned long long diff {expected > executed ? expected - executed : executed - expected};
    std::cout << std::format("\n*** {:d} instructions executed on {:d} cycles (expected={:d}, diff={:d}) in {:s}\n\n",
           count, executed, expected, diff, formatTime(begin, std::chrono::steady_clock::now()));
}

int main(int argc, char** argv)
{
    const std::string env {argv[0]};
    z80<Memory> z {};

    const std::chrono::steady_clock::time_point begin {std::chrono::steady_clock::now()};

    // run test roms
    test(z, "tests/test-roms/prelim.com", 8721ULL);
    //test(z, env + "/../test-roms/zexdoc.com", 46734978649ULL);
    //test(z, env + "/../test-roms/zexall.com", 46734978649ULL);

    std::cout << "Done. Total time elapsed " << formatTime(begin, std::chrono::steady_clock::now()) << std::endl;
}