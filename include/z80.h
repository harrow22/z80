#ifndef Z80_LIBRARY_H
#define Z80_LIBRARY_H

#include <cstdint>
#include <algorithm>
#include <array>

using u8 = std::uint8_t;
using u16 = std::uint16_t;
using u32 = std::uint32_t;
using s8 = std::int8_t;

template<typename Memory>
class z80 {
public:
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

    enum class AddressMode {
        Immediate,
        ImmediateEx,
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
        RegisterIndirectBC,
        RegisterIndirectDE,
        RegisterIndirectHL,
        RegisterIndirectSP,
    };

    enum class Condition { Null, C, NC, Z, NZ };

    template<AddressMode mode>
    void setOperand(u16 val)
    {
        switch (mode) {
            case AddressMode::Extended: write(fetch16_(), val); return;
            case AddressMode::IndexedIX: write(fetch8_() + ix_, val); return;
            case AddressMode::IndexedIY: write(fetch8_() + iy_, val); return;
            case AddressMode::Accumulator: regs_.a = val; return;
            case AddressMode::RegisterB: regs_.b = val; return;
            case AddressMode::RegisterC: regs_.c = val; return;
            case AddressMode::RegisterD: regs_.d = val; return;
            case AddressMode::RegisterE: regs_.e = val; return;
            case AddressMode::RegisterH: regs_.h = val; return;
            case AddressMode::RegisterL: regs_.l = val; return;
            case AddressMode::RegisterBC: regs_.b = val >> 8; regs_.c = val & 0xFF; return;
            case AddressMode::RegisterDE: regs_.d = val >> 8; regs_.e = val & 0xFF; return;
            case AddressMode::RegisterHL: regs_.h = val >> 8; regs_.l = val & 0xFF; return;
            case AddressMode::RegisterSP: sp_ = val; return;
            case AddressMode::RegisterIX: ix_ = val; return;
            case AddressMode::RegisterIY: iy_ = val; return;
            case AddressMode::RegisterIndirectBC: write(regs_.bc, val); return;
            case AddressMode::RegisterIndirectDE: write(regs_.de, val); return;
            case AddressMode::RegisterIndirectHL: write(regs_.hl, val); return;
            case AddressMode::RegisterIndirectSP: write(sp_, val); return;
        }
    }

    template<AddressMode mode>
    u16 getOperand()
    {
        switch (mode) {
            case AddressMode::Immediate: return fetch8_();
            case AddressMode::ImmediateEx: return fetch16_();
            case AddressMode::Extended: return read8(fetch16_());
            case AddressMode::IndexedIX: return read8(fetch8_() + ix_);
            case AddressMode::IndexedIY: return read8(fetch8_() + iy_);
            case AddressMode::Accumulator: return regs_.a;
            case AddressMode::RegisterB: return regs_.b;
            case AddressMode::RegisterC: return regs_.c;
            case AddressMode::RegisterD: return regs_.d;
            case AddressMode::RegisterE: return regs_.e;
            case AddressMode::RegisterH: return regs_.h;
            case AddressMode::RegisterL: return regs_.l;
            case AddressMode::RegisterBC: return regs_.b << 8 | regs_.c;
            case AddressMode::RegisterDE: return regs_.d << 8 | regs_.e;
            case AddressMode::RegisterHL: return regs_.h << 8 | regs_.l;
            case AddressMode::RegisterSP: return sp_;
            case AddressMode::RegisterIX: return ix_;
            case AddressMode::RegisterIY: return iy_;
            case AddressMode::RegisterIndirectBC: return read8(regs_.bc);
            case AddressMode::RegisterIndirectDE: return read8(regs_.de);
            case AddressMode::RegisterIndirectHL: return read8(regs_.hl);
            case AddressMode::RegisterIndirectSP: return read8(sp_);
        }
    }

    template<Condition cond>
    bool getCondition()
    {
        switch (cond) {
            case Condition::C: return flags_.cf != 0;
            case Condition::NC: return flags_.cf == 0;
            case Condition::Z: return flags_.zf != 0;
            case Condition::NZ: return flags_.zf == 0;
            case Condition::Null: return true;
        }
    }

    [[nodiscard]] u8 fetch8_() { return read8(pc_++); }
    [[nodiscard]] u16 fetch16_() { pc_ += 2; return read16(pc_ - 2); };
    void tick_();

    void nop() { }

    template<AddressMode mode1, AddressMode mode2>
    void ld() { setOperand<mode1>(getOperand<mode2>()); }

    template<AddressMode mode>
    void incb()
    {
        u8 operand {getOperand<mode>()};
        const u8 res {++operand};
        flags_.hf = (operand & 0xFU) == 0; // only case for half carry is 0b1111 + 0b1 = 0b10000;
        flags_.pf = res < operand ? pf : 0;
        flags_.nf = 0;
        flags_.sf = res & sf;
        flags_.zf = res == 0 ? zf : 0;
        flags_.yf = res & yf;
        flags_.xf = res & xf;
        setOperand<mode>(res);
    }
    template<AddressMode mode>
    void incw() { setOperand<mode>(getOperand<mode>() + 1); }

    template <AddressMode mode>
    void decb()
    {
        u8 operand {getOperand<mode>()};
        const u8 res {--operand};
        flags_.hf = (operand & 0xFU) != 0xF; // only case for half borrow is 0b10000 - 0b1 = 0b1111
        flags_.pf = res > operand ? pf : 0;
        flags_.nf = nf;
        flags_.sf = res & sf;
        flags_.zf = res == 0 ? zf : 0;
        flags_.yf = res & yf;
        flags_.xf = res & xf;
        setOperand<mode>(res);
    }
    template<AddressMode mode>
    void decw() { setOperand<mode>(getOperand<mode>() + 1); }

    void rlca()
    {
        flags_.cf = regs_.a & 0x80U ? cf : 0;
        regs_.a <<= 7U | flags_.cf;
        flags_.hf = 0;
        flags_.nf = 0;
        flags_.yf = regs_.a & yf;
        flags_.xf = regs_.a & xf;
    }

    void rla()
    {
        const u8 temp {flags_.cf};
        flags_.cf = regs_.a & 0x80U ? cf : 0;
        regs_.a <<= 7U | temp;
        flags_.hf = 0;
        flags_.nf = 0;
        flags_.yf = regs_.a & yf;
        flags_.xf = regs_.a & xf;
    }

    void exaf()
    {
        std::swap(regs_.a, regs2_.a);
        std::swap(flags_, flags2_);
    }

    template<AddressMode mode1, AddressMode mode2>
    void addw()
    {
        const u16 addend {getOperand<mode1>()};
        const u16 augend {getOperand<mode2>()};
        const u32 sum {addend + augend};
        flags_.cf = sum > 0xFFFFU ? cf : 0;
        flags_.hf = (addend ^ augend ^ sum) & 0x10U ? hf : 0;
        flags_.nf = 0;
        flags_.yf = sum & yf;
        flags_.xf = sum & xf;
        setOperand<mode1>(static_cast<u16>(sum));
    }

    void rrca()
    {
        flags_.cf = regs_.a & 0x01U ? cf : 0;
        regs_.a >>= 1U | flags_.cf << 7U;
        flags_.hf = 0;
        flags_.nf = 0;
        flags_.yf = regs_.a & yf;
        flags_.xf = regs_.a & xf;
    }

    void rra()
    {
        const u8 temp {flags_.cf};
        flags_.cf = regs_.a & 0x01U ? cf : 0;
        regs_.a >>= 1U | temp << 7U;
        flags_.hf = 0;
        flags_.nf = 0;
        flags_.yf = regs_.a & yf;
        flags_.xf = regs_.a & xf;
    }

    void djnz()
    {
        if (--regs_.b != 0 ) {
            const s8 val {read8(pc_)};
            pc_ += val;
        }
    }

    template<Condition cond>
    void jr()
    {
        if (getCondition<cond>()) {
            const s8 val {read8(pc_)};
            pc_ += val;
        }
    }


    // start main instructions
    static constexpr std::array<void (z80::*)(), 256> instruction {
        &z80::nop, // $00: nop
        &z80::ld<AddressMode::RegisterBC, AddressMode::ImmediateEx>, // $01: ld bc, nn
        &z80::ld<AddressMode::RegisterIndirectBC, AddressMode::Accumulator>, // $02: ld (bc), a
        &z80::incw<AddressMode::RegisterBC>, // $03: inc bc
        &z80::incb<AddressMode::RegisterB>, // $04: inc b
        &z80::decb<AddressMode::RegisterB>, // $05: dec b
        &z80::ld<AddressMode::RegisterB, AddressMode::Immediate>, // $06: ld b, n
        &z80::rlca, // $07: rlca,
        &z80::exaf, // $08: ex af, af'
        &z80::addw<AddressMode::RegisterHL, AddressMode::RegisterBC>, // $09: add hl, bc
        &z80::ld<AddressMode::Accumulator, AddressMode::RegisterIndirectBC>, // $0A: ld a, (bc)
        &z80::decw<AddressMode::RegisterBC>, // $0B: dec bc
        &z80::incb<AddressMode::RegisterC>, // $0C: inc c
        &z80::decb<AddressMode::RegisterC>, // $0D: dec c
        &z80::ld<AddressMode::RegisterC, AddressMode::Immediate>, // $0E: ld c, n
        &z80::rrca, // $0F: rrca
        &z80::djnz, // $10: djnz d
        &z80::ld<AddressMode::RegisterDE, AddressMode::ImmediateEx>, // $11: ld de, nn
        &z80::ld<AddressMode::RegisterIndirectDE, AddressMode::Accumulator>, // $12: ld (de), a
        &z80::incw<AddressMode::DE>, // $13: inc de
        &z80::incb<AddressMode::D>, // $14: inc d
        &z80::decb<AddressMode::D>, // $15: dec d
        &z80::ld<AddressMode::D, AddressMode::Immediate>, // $16: ld d, n
        &z80::rla, // $17: rla
        &z80::jr<Condition::Null>, // $18: jr d
        &z80::addw<AddressMode::RegisterHL, AddressMode::RegisterDE>, // $19: add hl, de
        &z80::ld<AddressMode::Accumulator, AddressMode::RegisterIndirectDE>, // $1A: ld a, (de)
        &z80::decw<AddressMode::RegisterDE>, // $1B: dec de
        &z80::incb<AddressMode::RegisterE>, // $1C: inc e
        &z80::decb<AddressMode::RegisterE>, // $1D: dec e
        &z80::ld<AddressMode::RegisterE, AddressMode::Immediate>, // $1E: ld e, n
        &z80::rra, // $1F: rra
        &z80::jr<Condition::NZ>, // $20: jr nz, d
    };

    static constexpr u8 cycles[256] {
        4, 10, 7, 6, 4, 4, 7, 4, 4, 11, 7, 6, 4, 4, 7, 4,
        8, 10, 7, 6, 4, 4, 7, 4, 12, 11, 7, 6, 4, 4, 7, 4,
        7, 10, 16, 6, 4, 4, 7, 4, 7, 11, 16, 6, 4, 4, 7, 4,
        7, 10, 13, 6, 11, 11, 10, 4, 7, 11, 13, 6, 4, 4, 7, 4,
        4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4,
        4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4,
        4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4,
        7, 7, 7, 7, 7, 7, 4, 7, 4, 4, 4, 4, 4, 4, 7, 4,
        4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4,
        4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4,
        4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4,
        4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4,
        5, 10, 10, 10, 10, 11, 7, 11, 5, 10, 10, 0, 10, 17, 7, 11,
        5, 10, 10, 11, 10, 11, 7, 11, 5, 4, 10, 11, 10, 0, 7, 11,
        5, 10, 10, 19, 10, 11, 7, 11, 5, 4, 10, 4, 10, 0, 7, 11,
        5, 10, 10, 4, 10, 11, 7, 11, 5, 6, 10, 4, 10, 0, 7, 11
    };
    // end main instructions

    // start bit instructions ($CB)
    static constexpr std::array<void (z80::*)(), 256> bitInstruction {

    };

    static constexpr u8 bitCycles[256] {

    };
    // end bit instructions

    // start misc. instructions ($ED)
    static constexpr std::array<void (z80::*)(), 256> miscInstruction {

    };

    static constexpr u8 miscCycles[256] {

    };
    // end misc. instructions

    // start ix instructions ($DD)
    static constexpr std::array<void (z80::*)(), 256> ixInstruction {

    };

    static constexpr u8 ixCycles[256] {

    };
    // end ix instructions

    // start ix bit instructions ($DDCB)
    static constexpr std::array<void (z80::*)(), 256> ixBitInstruction {

    };

    static constexpr u8 ixBitCycles[256] {

    };
    // end ix bit instructions

    // start iy instructions ($FD)
    static constexpr std::array<void (z80::*)(), 256> iyInstruction {

    };

    static constexpr u8 iyCycles[256] {

    };
    // end iy instructions

    // start iy bit instructions ($FDCB)
    static constexpr std::array<void (z80::*)(), 256> iyBitInstruction {

    };

    static constexpr u8 iyBitCycles[256] {

    };
    // end iy bit instructions

    // internals
    struct Registers {
        u8 a {};
        u8 b {};
        u8 c {};
        u8 d {};
        u8 e {};
        u8 h {};
        u8 l {};
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
    Registers regs_ {}, regs2_ {};
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
        (this->*bitInstruction[opcode])();
        requested_ -= bitCycles[opcode];
    } else if (opcode == 0xED) {
        opcode = fetch8_();
        (this->*miscInstruction[opcode])();
        requested_ -= miscCycles[opcode];
    } else if (opcode == 0xDD) {
        opcode = fetch8_();
        if (opcode == 0xCB) {
            opcode = fetch8_();
            (this->*ixBitInstruction[opcode])();
            requested_ -= ixBitCycles[opcode];
        } else {
            (this->*ixBitInstruction[opcode])();
            requested_ -= ixCycles[opcode];
        }
    } else if (opcode == 0xFD) {
        opcode = fetch8_();
        if (opcode == 0xCB) {
            opcode = fetch8_();
            (this->*iyBitInstruction[opcode])();
            requested_ -= iyBitCycles[opcode];
        } else {
            (this->*iyInstruction[opcode])();
            requested_ -= iyCycles[opcode];
        }
    } else {
        (this->*instruction[opcode])();
        requested_ -= cycles[opcode];
    }
}

#endif //Z80_LIBRARY_H
