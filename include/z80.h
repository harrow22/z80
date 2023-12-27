#ifndef Z80_LIBRARY_H
#define Z80_LIBRARY_H

#include <cstdint>

using u8 = std::uint8_t;
using u16 = std::uint16_t;
using s8 = std::int8_t;

template<typename Memory>
class z80 {
public:
    explicit z80(Memory mem) : memory_{mem} { }

    int run(int cycles);
    void interrupt();
    void write(u8 val) { memory_.write(val); }
    void write(u16 val) { memory_.write(val); }
    u8 read8(u16 addr) { return memory_.read8(addr); }
    u16 read16(u16 addr) { return memory_.read16(addr); }
private:
    static constexpr u8 sf {0b10000000}; // bit 7: sign flag
    static constexpr u8 zf {0b01000000}; // bit 6: zero flag
    static constexpr u8 yf {0b00100000}; // bit 5: undocumented flag (copy of bit 5 of the result)
    static constexpr u8 hf {0b00010000}; // bit 4: half-carry flag
    static constexpr u8 xf {0b00001000}; // bit 3: undocumented flag (copy of bit 3 of the result)
    static constexpr u8 pf {0b00000100}; // bit 2: parity flag
    static constexpr u8 nf {0b00000010}; // bit 1: add/subtract flag
    static constexpr u8 cf {0b00000001}; // bit 0: carry flag
    static constexpr u8 a {0b111}; // register A
    static constexpr u8 b {0b000}; // register B
    static constexpr u8 c {0b001}; // register C
    static constexpr u8 d {0b010}; // register D
    static constexpr u8 e {0b011}; // register E
    static constexpr u8 h {0b100}; // register H
    static constexpr u8 l {0b101}; // register L
    static constexpr u8 bc {0b00}; // register pair BC
    static constexpr u8 de {0b01}; // register pair BC
    static constexpr u8 hl {0b10}; // register pair BC
    static constexpr u8 sp {0b11}; // register pair BC

    enum class AddressMode {
        Immediate,
        ImmediateEx,
        ModifiedZeroPage,
        Relative,
        Extended,
        Indexed,
        Register,
        Implied,
        Indirect,
        Bit
    };

    enum class Destination {
        A, B, C, D, E, H, L,
        BC, DE, HL, SP
    };

    enum class Operation {
        ADC, ADD, AND, CALL, CCF, CP, CPD, CPDR,
        CPI, CPIR, CPL, DAA, DEC, DI, DJNZ, EI,
        EX, EXX, HALT, IM, IN, INC, IND, INDR,
        INI, INIR, JP, JR, LD, LDD, LDDR, LDI,
        LDIR, NEG, NOP, OR, OTDR, OTIR, OUT, OUTD,
        OUTI, POP, PUSH, RES, RET, RETI, RETN, RL,
        RLA, RLC, RLCA, RLD, RR, RRA, RRCA, RRD,
        RST, SBC, SCF, SET, SLA, SRA, SRL, SUB,
        XOR
    };

    template <Operation op>
    void execute_(u16 operand)
    {
        switch (op) {
            case Operation::NOP: return;
            case Operation::LD: return load(operand);
        }
    }

    template <Operation op, Destination dst>
    void execute_(u16 operand)
    {
        u16& reg = pair_[0];
        if constexpr (dst == Destination::A)
            reg = a_[0];
        else if constexpr (dst == Destination::B)

        switch (op) {
            case Operation::NOP: return;
            case Operation::LD: return load(reg, operand);
        }
    }

    template<AddressMode mode, Operation op>
    consteval void decode_()
    {
        if constexpr (mode == AddressMode::Immediate)
            execute_<op>(read8(pc_++));
        else if constexpr (mode == AddressMode::ImmediateEx)
            execute_<op>(read16(pc_++));
        else if constexpr (mode == AddressMode::ModifiedZeroPage)
            execute_<op>();
        else if constexpr (mode == AddressMode::Relative)
            execute_<op>();
        else if constexpr (mode == AddressMode::Extended)
            execute_<op>();
        else if constexpr (mode == AddressMode::Indexed)
            execute_<op>();
        else if constexpr (mode == AddressMode::Register)
            execute_<op>();
        else if constexpr (mode == AddressMode::Implied)
            execute_<op>();
        else if constexpr (mode == AddressMode::Indirect)
            execute_<op>();
        else if constexpr (mode == AddressMode::Bit)
            execute_<op>();
    }

    template<AddressMode mode, Operation op, Destination dst>
    consteval void decode_()
    {
        if constexpr (mode == AddressMode::Immediate)
            execute_<op, dst>(read8(pc_++));
        else if constexpr (mode == AddressMode::ImmediateEx)
            execute_<op, dst>(read16(pc_++));
        else if constexpr (mode == AddressMode::ModifiedZeroPage)
            execute_<op, dst>();
        else if constexpr (mode == AddressMode::Relative)
            execute_<op, dst>();
        else if constexpr (mode == AddressMode::Extended)
            execute_<op, dst>();
        else if constexpr (mode == AddressMode::Indexed)
            execute_<op, dst>();
        else if constexpr (mode == AddressMode::Register)
            execute_<op, dst>();
        else if constexpr (mode == AddressMode::Implied)
            execute_<op, dst>();
        else if constexpr (mode == AddressMode::Indirect)
            execute_<op, dst>();
        else if constexpr (mode == AddressMode::Bit)
            execute_<op, dst>();
    }

    void tick_()
    {
        u8 opcode {read8(pc_++)};

        if (opcode == 0xCB) {
            opcode = read8(pc_++);

        } else if (opcode == 0xED) {
            opcode = read8(pc_++);

        } else if (opcode == 0xDD) {
            opcode = read8(pc_++);
            if (opcode == 0xCB) {
                opcode = read8(pc_++);

            } else {

            }
        } else if (opcode == 0xFD) {
            opcode = read8(pc_++);
            if (opcode == 0xCB) {
                opcode = read8(pc_++);

            } else {

            }
        } else {
            instruction[opcode]();
        }
    }

    // start instruction pages
    static constexpr auto instruction {
        &execute_<Operation::NOP>, // 0x00: nop
        &decode_<AddressMode::ImmediateEx, Operation::LD, Destination::BC>, // $01: ld bc, nn
        &decode_<AddressMode::ImmediateEx, Operation::LD>, // $10: ld de, nn
        &decode_<AddressMode::Immediate, Operation::ADD>, // $86
        &decode_<AddressMode::Bit, Operation::ADD>, // $87
    };
    // end instruction pages

    void setHi_(const u8 rp, const u8 val) { pair_[0][rp] = pair_[0][rp] & 0x00FFU | val << 8U; }
    void setLo_(const u8 rp, const u8 val) { pair_[0][rp] = pair_[0][rp] & 0xFF00U | val; }
    [[nodiscard]] static u8 hi_(const u16 val) { return (val & 0xFF00) >> 8U; }
    [[nodiscard]] static u8 lo_(const u16 val) { return val & 0xFF; }

    static void load(u16&, u16);

    // registers
    u16 pair_[2][3] {};
    u8 sf_[2] {}, zf_[2] {}, yf_[2] {}, hf_[2] {}, xf_[2] {}, pf_[2] {}, nf_[2] {}, cf_[2] {};
    u8 a_[2] {};
    u16 pc_ {}, sp_ {}, ix_ {}, iy_ {};
    u8 i_ {}, r_ {};

    bool set_ {false}; // which register set is currently being worked with
    bool iff_ {false}; // interrupt latch
    int requested_ {};
    Memory memory_ {};
};

template<typename Memory>
int z80<Memory>::run(const int cycles)
{
    requested_ = cycles;
    while (requested_ > 0) { tick_(); }
    return requested_;
}

template<typename Memory>
void z80<Memory>::load(u16& dst, const u16 src)
{
    dst = src;
}

#endif //Z80_LIBRARY_H
