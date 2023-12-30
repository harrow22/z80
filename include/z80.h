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
    void write(u16 addr, u8 val) { memory_.write(addr, val); }
    void write(u16 addr, u16 val) { memory_.write(addr, val); }
    u8 read8(u16 addr) { return memory_.read8(addr); }
    u16 read16(u16 addr) { return memory_.read16(addr); }

    [[nodiscard]] u8 flag_() const { return flags_.sf | flags_.zf | flags_.yf | flags_.hf | flags_.xf | flags_.pf | flags_.nf | flags_.cf; }
private:
    static constexpr u8 sf {0b10000000}; // bit 7: sign flag
    static constexpr u8 zf {0b01000000}; // bit 6: zero flag
    static constexpr u8 yf {0b00100000}; // bit 5: undocumented flag (copy of bit 5 of the result)
    static constexpr u8 hf {0b00010000}; // bit 4: half-carry flag
    static constexpr u8 xf {0b00001000}; // bit 3: undocumented flag (copy of bit 3 of the result)
    static constexpr u8 pf {0b00000100}; // bit 2: parity flag
    static constexpr u8 nf {0b00000010}; // bit 1: add/subtract flag
    static constexpr u8 cf {0b00000001}; // bit 0: carry flag

    /*
    static constexpr u8 a {0b111}; // register A
    static constexpr u8 b {0b000}; // register B
    static constexpr u8 c {0b001}; // register C
    static constexpr u8 d {0b010}; // register D
    static constexpr u8 e {0b011}; // register E
    static constexpr u8 h {0b100}; // register H
    static constexpr u8 l {0b101}; // register L
    static constexpr u8 bc {0b00}; // register pair BC
    static constexpr u8 de {0b01}; // register pair DE
    static constexpr u8 hl {0b10}; // register pair HL
    static constexpr u8 sp {0b11}; // register pair SP
    */

    enum class AddressMode {
        Immediate,
        ImmediateEx,
        //Relative,
        //ModifiedZeroPage,
        Extended,
        IndexedIX,
        IndexedIY,
        Accumulator,
        RegisterB,
        RegisterC,
        RegisterD,
        RegisterE,
        RegisterH,
        RegisterL,
        RegisterBC,
        RegisterDE,
        RegisterHL,
        RegisterSP,
        RegisterIX,
        RegisterIY,
        //Implied,
        RegisterIndirectBC,
        RegisterIndirectDE,
        RegisterIndirectHL,
        RegisterIndirectSP,
        //Bit
    };

    template<AddressMode mode>
    consteval void setOperand(u16 val)
    {
        //if constexpr (mode == AddressMode::Immediate)
            //write(fetch8_(), val);
        //else if constexpr (mode == AddressMode::ImmediateEx)
            //write(fetch16_(), val);
        //else if constexpr (mode == AddressMode::ModifiedZeroPage)
            //return 0;
        //else if constexpr (mode == AddressMode::Relative)
            //return 0;
        if constexpr (mode == AddressMode::Extended)
            write(fetch16_(), val);
        else if constexpr (mode == AddressMode::IndexedIX)
            write(fetch8_() + ix_, val);
        else if constexpr (mode == AddressMode::IndexedIY)
            write(fetch8_() + iy_, val);
        else if constexpr (mode == AddressMode::Accumulator)
            registers_.a = val;
        else if constexpr (mode == AddressMode::RegisterB)
            setHi_(registers_.bc, val);
        else if constexpr (mode == AddressMode::RegisterC)
            setLo_(registers_.bc, val);
        else if constexpr (mode == AddressMode::RegisterD)
            setHi_(registers_.de, val);
        else if constexpr (mode == AddressMode::RegisterE)
            setLo_(registers_.de, val);
        else if constexpr (mode == AddressMode::RegisterH)
            setHi_(registers_.hl, val);
        else if constexpr (mode == AddressMode::RegisterL)
            setLo_(registers_.hl, val);
        else if constexpr (mode == AddressMode::RegisterBC)
            registers_.bc = val;
        else if constexpr (mode == AddressMode::RegisterDE)
            registers_.de = val;
        else if constexpr (mode == AddressMode::RegisterHL)
            registers_.hl = val;
        else if constexpr (mode == AddressMode::RegisterSP)
            registers_.sp = val;
        else if constexpr (mode == AddressMode::RegisterIX)
            ix_ = val;
        else if constexpr (mode == AddressMode::RegisterIY)
            iy_ = val;
        //else if constexpr (mode == AddressMode::Implied)
            //return 0;
        else if constexpr (mode == AddressMode::RegisterIndirectBC)
            write(registers_.bc, val);
        else if constexpr (mode == AddressMode::RegisterIndirectDe)
            write(registers_.de, val);
        else if constexpr (mode == AddressMode::RegisterIndirectHL)
            write(registers_.hl, val);
        else if constexpr (mode == AddressMode::RegisterIndirectSP)
            write(sp_, val);
        //else if constexpr (mode == AddressMode::Bit)
            //return 0;
    }

    template<AddressMode mode>
    consteval u16 getOperand()
    {
        if constexpr (mode == AddressMode::Immediate)
            return fetch8_();
        else if constexpr (mode == AddressMode::ImmediateEx)
            return fetch16_();
        //else if constexpr (mode == AddressMode::ModifiedZeroPage)
            //return 0;
        //else if constexpr (mode == AddressMode::Relative)
            //return 0;
        else if constexpr (mode == AddressMode::Extended)
            return read8(fetch16_());
        else if constexpr (mode == AddressMode::IndexedIX)
            return read8(fetch8_() + ix_);
        else if constexpr (mode == AddressMode::IndexedIY)
            return read8(fetch8_() + iy_);
        else if constexpr (mode == AddressMode::Accumulator)
            return registers_.a;
        else if constexpr (mode == AddressMode::RegisterB)
            return hi_(registers_.bc);
        else if constexpr (mode == AddressMode::RegisterC)
            return lo_(registers_.bc);
        else if constexpr (mode == AddressMode::RegisterD)
            return hi_(registers_.de);
        else if constexpr (mode == AddressMode::RegisterE)
            return lo_(registers_.de);
        else if constexpr (mode == AddressMode::RegisterH)
            return hi_(registers_.hl);
        else if constexpr (mode == AddressMode::RegisterL)
            return lo_(registers_.hl);
        else if constexpr (mode == AddressMode::RegisterBC)
            return registers_.bc;
        else if constexpr (mode == AddressMode::RegisterDE)
            return registers_.de;
        else if constexpr (mode == AddressMode::RegisterHL)
            return registers_.hl;
        else if constexpr (mode == AddressMode::RegisterSP)
            return registers_.sp;
        else if constexpr (mode == AddressMode::RegisterIX)
            return ix_;
        else if constexpr (mode == AddressMode::RegisterIY)
            return iy_;
        //else if constexpr (mode == AddressMode::Implied)
            //return 0;
        else if constexpr (mode == AddressMode::RegisterIndirectBC)
            return read8(registers_.bc);
        else if constexpr (mode == AddressMode::RegisterIndirectDe)
            return read8(registers_.de);
        else if constexpr (mode == AddressMode::RegisterIndirectHL)
            return read8(registers_.hl);
        else if constexpr (mode == AddressMode::RegisterIndirectSP)
            return read8(sp_);
        //else if constexpr (mode == AddressMode::Bit)
            //return 0;
    }

    static void setHi_(u16& rp, const u8 val) { rp = rp & 0x00FFU | val << 8U; }
    static void setLo_(u16& rp, const u8 val) { rp = rp & 0xFF00U | val; }
    [[nodiscard]] static u8 hi_(const u16 val) { return (val & 0xFF00) >> 8U; }
    [[nodiscard]] static u8 lo_(const u16 val) { return val & 0xFF; }
    [[nodiscard]] u8 fetch8_() { return read8(pc_++); }
    [[nodiscard]] u16 fetch16_() { pc_ += 2; return read16(pc_ - 2); };
    void tick_();

    void nop();
    template<AddressMode mode1, AddressMode mode2>
    void ld() { setOperand<mode1>(getOperand<mode2>()); }
    template<AddressMode mode>
    void inc() { setOperand<mode>(getOperand<mode>() + 1); } // TODO: add flags
    template <AddressMode mode>
    void dec() { setOperand<mode>(getOperand<mode>() - 1); } // TODO: add flags


    // start main instructions
    static constexpr auto instruction {
        &nop, // $00: nop
        &ld<AddressMode::RegisterBC, AddressMode::ImmediateEx>, // $01: ld bc, nn
        &ld<AddressMode::RegisterIndirectBC, AddressMode::Accumulator>, // $02: ld (bc), a
        &inc<AddressMode::RegisterBC>, // $03: inc bc
        &inc<AddressMode::RegisterB>, // $04: inc b
        &dec<AddressMode::RegisterB>, // $05: dec b

    };

    static constexpr u8 cycles[256] {
        4, 10, 7, 6, 4, 4, 7,
    };
    // end main instructions

    // start bit instructions ($CB)
    static constexpr auto bitInstruction {

    };

    static constexpr u8 bitCycles[256] {

    };
    // end bit instructions

    // start misc. instructions ($ED)
    static constexpr auto miscInstruction {

    };

    static constexpr u8 miscCycles[256] {

    };
    // end misc. instructions

    // start ix instructions ($DD)
    static constexpr auto ixInstruction {

    };

    static constexpr u8 ixCycles[256] {

    };
    // end ix instructions

    // start ix bit instructions ($DDCB)
    static constexpr auto ixBitInstruction {

    };

    static constexpr u8 ixBitCycles[256] {

    };
    // end ix bit instructions

    // start iy instructions ($FD)
    static constexpr auto iyInstruction {

    };

    static constexpr u8 iyCycles[256] {

    };
    // end iy instructions

    // start iy bit instructions ($FDCB)
    static constexpr auto iyBitInstruction {

    };

    static constexpr u8 iyBitCycles[256] {

    };
    // end iy bit instructions

    // internals
    struct Registers {
        u16 bc {};
        u16 de {};
        u16 hl {};
        u8 a {};
    };

    struct Flags {
        u8 sf {};
        u8 zf {};
        u8 yf {};
        u8 hf {};
        u8 xf {};
        u8 pf {};
        u8 nf {};
        u8 cf {};
    };

    Flags flags_ {}, flags2_ {};
    Registers registers_ {}, registers2_ {};
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
void z80<Memory>::tick_()
{
    u8 opcode {fetch8_()};

    if (opcode == 0xCB) {
        opcode = fetch8_();
        bitInstruction[opcode]();
        requested_ -= bitCycles[opcode];
    } else if (opcode == 0xED) {
        opcode = fetch8_();
        miscInstruction[opcode]();
        requested_ -= miscCycles[opcode];
    } else if (opcode == 0xDD) {
        opcode = fetch8_();
        if (opcode == 0xCB) {
            opcode = fetch8_();
            ixBitInstruction[opcode]();
            requested_ -= ixBitCycles[opcode];
        } else {
            ixInstruction[opcode]();
            requested_ -= ixCycles[opcode];
        }
    } else if (opcode == 0xFD) {
        opcode = fetch8_();
        if (opcode == 0xCB) {
            opcode = fetch8_();
            iyBitInstruction[opcode]();
            requested_ -= iyBitCycles[opcode];
        } else {
            iyInstruction[opcode]();
            requested_ -= iyCycles[opcode];
        }
    } else {
        instruction[opcode]();
        requested_ -= cycles[opcode];
    }
}

#endif //Z80_LIBRARY_H
