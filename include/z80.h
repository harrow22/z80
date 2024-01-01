#ifndef Z80_LIBRARY_H
#define Z80_LIBRARY_H

#include <cstdint>
#include <algorithm>
#include <bit>
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

    /**
     * \brief Flags are stored separetly, to get them as a single status byte they must be or'd together.
     * \return The flag register as a single byte.
     */
    [[nodiscard]] u8 flag_() const { return flags_.sf | flags_.zf | flags_.yf | flags_.hf | flags_.xf | flags_.pf | flags_.nf | flags_.cf; }

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

    Registers regs_ {};
    Flags flags_ {};
    u16 pc_ {}, sp_ {}, ix_ {}, iy_ {};
    u8 i_ {}, r_ {};
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
            case AddressMode::RegisterIndirectBC: write(regs_.b << 8 | regs_.c, val); return;
            case AddressMode::RegisterIndirectDE: write(regs_.d << 8 | regs_.e, val); return;
            case AddressMode::RegisterIndirectHL: write(regs_.h << 8 | regs_.l, val); return;
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
            case AddressMode::RegisterIndirectBC: return read8(regs_.b << 8 | regs_.c);
            case AddressMode::RegisterIndirectDE: return read8(regs_.d << 8 | regs_.e);
            case AddressMode::RegisterIndirectHL: return read8(regs_.h << 8 | regs_.l);
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
        const u8 operand {static_cast<u8>(getOperand<mode>())};
        const u8 res {static_cast<u8>(operand + 1U)};
        flags_.hf = (operand & 0xFU) == 0; // only case for half carry is 0b1111 + 0b1 = 0b10000;
        flags_.pf = ((operand ^ res) & (1U ^ res) & 0x80U) != 0 ? pf : 0;
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
        u8 operand {static_cast<u8>(getOperand<mode>())};
        const u8 res {--operand};
        flags_.hf = (operand & 0xFU) != 0xF; // only case for half borrow is 0b10000 - 0b1 = 0b1111
        flags_.pf = ((res ^ operand) & (1 ^ operand) & 0x80) != 0 ? pf : 0;
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
        const u32 sum {static_cast<u32>(addend + augend)};
        flags_.cf = sum > 0xFFFFU ? cf : 0;
        flags_.hf = (addend ^ augend ^ sum) & 0x100U ? hf : 0;
        flags_.nf = 0;
        flags_.yf = sum & yf;
        flags_.xf = sum & xf;
        setOperand<mode1>(static_cast<u16>(sum));
    }

    template<AddressMode mode>
    void addb()
    {
        const u8 augend {static_cast<u8>(getOperand<mode>())};
        const u16 sum {static_cast<u16>(regs_.a + augend)};
        flags_.cf = sum > 0xFFU ? cf : 0;
        flags_.hf = (regs_.a ^ augend ^ sum) & 0x10U ? hf : 0;
        flags_.pf = ((regs_.a ^ sum) & (augend ^ sum) & 0x80U) != 0 ? pf : 0;
        flags_.nf = 0;
        flags_.yf = sum & yf;
        flags_.xf = sum & xf;
        regs_.a = static_cast<u8>(sum);
    }

    template<AddressMode mode>
    void adc()
    {
        const u8 augend {static_cast<u8>(getOperand<mode>())};
        const u16 sum {static_cast<u16>(regs_.a + augend + flags_.cf)};
        flags_.cf = sum > 0xFFU ? cf : 0;
        flags_.hf = (regs_.a ^ augend ^ sum) & 0x10U ? hf : 0;
        flags_.pf = ((regs_.a ^ sum) & (augend ^ sum) & 0x80U) != 0 ? pf : 0;
        flags_.nf = 0;
        flags_.yf = sum & yf;
        flags_.xf = sum & xf;
        regs_.a = static_cast<u8>(sum);
    }

    template<AddressMode mode>
    void sub()
    {
        const u8 subtrahend {static_cast<u8>(getOperand<mode>())};
        const u16 difference {static_cast<u16>(regs_.a - subtrahend)};
        flags_.cf = difference < subtrahend ? cf : 0;
        flags_.hf = ~(regs_.a ^ subtrahend ^ difference) & 0x10U ? hf : 0;
        flags_.pf = ((difference ^ regs_.a) & (subtrahend ^ regs_.a) & 0x80U) != 0 ? pf : 0;
        flags_.nf = 0;
        flags_.yf = difference & yf;
        flags_.xf = difference & xf;
        regs_.a = static_cast<u8>(difference);
    }

    template<AddressMode mode>
    void sbc()
    {
        const u8 subtrahend {static_cast<u8>(getOperand<mode>())};
        const u16 difference {static_cast<u16>(regs_.a - subtrahend - flags_.cf)};
        flags_.cf = difference < subtrahend + flags_.cf ? cf : 0;
        flags_.hf = ~(regs_.a ^ subtrahend ^ difference) & 0x10U ? hf : 0;
        flags_.pf = ((difference ^ regs_.a) & (subtrahend ^ regs_.a) & 0x80U) != 0 ? pf : 0;
        flags_.nf = 0;
        flags_.yf = difference & yf;
        flags_.xf = difference & xf;
        regs_.a = static_cast<u8>(difference);
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
            const s8 val {static_cast<s8>(read8(pc_))};
            pc_ += val;
        }
    }

    template<Condition cond>
    void jr()
    {
        if (getCondition<cond>()) {
            const s8 val {static_cast<s8>(read8(pc_))};
            pc_ += val;
        }
    }

    void daa()
    {
        u8 correction = 0;
        flags_.cf = 0;

        if (flags_.hf or (!flags_.nf and (regs_.a & 0xF) > 9)) {
            correction |= 0x6;
        }

        if (flags_.cf or (!flags_.nf and (regs_.a & 0xF0) > 0x99)) {
            correction |= 0x60;
            flags_.cf = cf;
        }

        regs_.a += flags_.nf ? -correction : correction;
        flags_.pf = std::popcount(regs_.a) % 2 == 0 ? pf : 0;
        flags_.sf = regs_.a & sf;
        flags_.zf = regs_.a == 0 ? zf : 0;
        flags_.yf = regs_.a & yf;
        flags_.xf = regs_.a & xf;
    }

    void cpl()
    {
        regs_.a = ~regs_.a;
        flags_.nf = nf;
        flags_.hf = hf;
    }

    void scf()
    {
        flags_.cf = cf;
        flags_.nf = 0;
        flags_.hf = 0;
    }

    void ccf()
    {
        flags_.cf = flags_.cf ? 0 : cf;
        flags_.hf = flags_.cf;
        flags_.nf = 0;
    }

    void halt()
    {
        // TODO: impelement this
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
        &z80::incw<AddressMode::RegisterDE>, // $13: inc de
        &z80::incb<AddressMode::RegisterD>, // $14: inc d
        &z80::decb<AddressMode::RegisterD>, // $15: dec d
        &z80::ld<AddressMode::RegisterD, AddressMode::Immediate>, // $16: ld d, n
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
        &z80::ld<AddressMode::RegisterHL, AddressMode::ImmediateEx>, // $21: ld hl, nn
        &z80::ld<AddressMode::Extended, AddressMode::RegisterHL>, // $22: ld (nn), hl
        &z80::incw<AddressMode::RegisterHL>, // $23: inc hl
        &z80::incb<AddressMode::RegisterH>, // $24: inc h
        &z80::decb<AddressMode::RegisterH>, // $25: dec h
        &z80::ld<AddressMode::RegisterH, AddressMode::Immediate>, // $26: ld h, n
        &z80::daa, // $27: daa
        &z80::jr<Condition::Z>, // $28: jr z, d
        &z80::addw<AddressMode::RegisterHL, AddressMode::RegisterHL>, // $29: add hl, hl
        &z80::ld<AddressMode::RegisterHL, AddressMode::Extended>, // $2A: ld hl, (nn)
        &z80::decw<AddressMode::RegisterHL>, // $2B: dec hl
        &z80::incb<AddressMode::RegisterL>, // $2C: inc l
        &z80::decb<AddressMode::RegisterL>, // $2D: dec l
        &z80::ld<AddressMode::RegisterL, AddressMode::Immediate>, // $2E: ld l, n
        &z80::cpl, // $2F: cpl
        &z80::jr<Condition::NC>, // $30: jr nc, d
        &z80::ld<AddressMode::RegisterSP, AddressMode::ImmediateEx>, // $31 ld sp, nn
        &z80::ld<AddressMode::Extended, AddressMode::Accumulator>, // $32: ld (nn), a
        &z80::incw<AddressMode::RegisterSP>, // $33: inc sp
        &z80::incb<AddressMode::RegisterIndirectHL>, // $34: inc (hl)
        &z80::decb<AddressMode::RegisterIndirectHL>, // $35: dec (hl)
        &z80::ld<AddressMode::RegisterIndirectHL, AddressMode::Immediate>, // $36: ld (hl), n
        &z80::scf, // $37: scf
        &z80::jr<Condition::C>, // $38: jr c, d
        &z80::addw<AddressMode::RegisterHL, AddressMode::RegisterSp>, // $39: add hl, sp
        &z80::ld<AddressMode::Accumulator, AddressMode::Extended>, // $3A: ld a, (nn)
        &z80::decw<AddressMode::RegisterSP>, // $3B: dec sp
        &z80::incb<AddressMode::Accumulator>, // $3C: inc a
        &z80::decb<AddressMode::Accumulator>, // $3D: dec a
        &z80::ld<AddressMode::Accumulator, AddressMode::Immediate>, // $3E: ld a, n
        &z80::ccf, // $3F: ccf
        &z80::ld<AddressMode::RegisterB, AddressMode::RegisterB>, // 40: ld b, b
        &z80::ld<AddressMode::RegisterB, AddressMode::RegisterC>, // 41: ld b, c
        &z80::ld<AddressMode::RegisterB, AddressMode::RegisterD>, // 42: ld b, d
        &z80::ld<AddressMode::RegisterB, AddressMode::RegisterE>, // 43: ld b, e
        &z80::ld<AddressMode::RegisterB, AddressMode::RegisterH>, // 44: ld b, h
        &z80::ld<AddressMode::RegisterB, AddressMode::RegisterL>, // 45: ld b, l
        &z80::ld<AddressMode::RegisterB, AddressMode::RegisterIndirectHL>, // 46: ld b, (hl)
        &z80::ld<AddressMode::RegisterB, AddressMode::Accumulator>, // 47: ld c, a
        &z80::ld<AddressMode::RegisterC, AddressMode::RegisterB>, // 48: ld c, b
        &z80::ld<AddressMode::RegisterC, AddressMode::RegisterC>, // 49: ld c, c
        &z80::ld<AddressMode::RegisterC, AddressMode::RegisterD>, // 4A: ld c, d
        &z80::ld<AddressMode::RegisterC, AddressMode::RegisterE>, // 4B: ld c, e
        &z80::ld<AddressMode::RegisterC, AddressMode::RegisterH>, // 4C: ld c, h
        &z80::ld<AddressMode::RegisterC, AddressMode::RegisterL>, // 4D: ld c, l
        &z80::ld<AddressMode::RegisterC, AddressMode::RegisterIndirectHL>, // 4E: ld c, (hl)
        &z80::ld<AddressMode::RegisterC, AddressMode::Accumulator>, // 4F: ld c, a
        &z80::ld<AddressMode::RegisterD, AddressMode::RegisterB>, // 50: ld d, b
        &z80::ld<AddressMode::RegisterD, AddressMode::RegisterC>, // 51: ld d, c
        &z80::ld<AddressMode::RegisterD, AddressMode::RegisterD>, // 52: ld d, d
        &z80::ld<AddressMode::RegisterD, AddressMode::RegisterE>, // 53: ld d, e
        &z80::ld<AddressMode::RegisterD, AddressMode::RegisterH>, // 54: ld d, h
        &z80::ld<AddressMode::RegisterD, AddressMode::RegisterL>, // 55: ld d, l
        &z80::ld<AddressMode::RegisterD, AddressMode::RegisterIndirectHL>, // 56: ld d, (hl)
        &z80::ld<AddressMode::RegisterD, AddressMode::Accumulator>, // 57: ld d, a
        &z80::ld<AddressMode::RegisterE, AddressMode::RegisterB>, // 58: ld e, b
        &z80::ld<AddressMode::RegisterE, AddressMode::RegisterC>, // 59: ld e, c
        &z80::ld<AddressMode::RegisterE, AddressMode::RegisterD>, // 5A: ld e, d
        &z80::ld<AddressMode::RegisterE, AddressMode::RegisterE>, // 5B: ld e, e
        &z80::ld<AddressMode::RegisterE, AddressMode::RegisterH>, // 5C: ld e, h
        &z80::ld<AddressMode::RegisterE, AddressMode::RegisterL>, // 5D: ld e, l
        &z80::ld<AddressMode::RegisterE, AddressMode::RegisterIndirectHL>, // 5E: ld e, (hl)
        &z80::ld<AddressMode::RegisterE, AddressMode::Accumulator>, // 5F: ld e, a
        &z80::ld<AddressMode::RegisterH, AddressMode::RegisterB>, // 60: ld h, b
        &z80::ld<AddressMode::RegisterH, AddressMode::RegisterC>, // 61: ld h, c
        &z80::ld<AddressMode::RegisterH, AddressMode::RegisterD>, // 62: ld h, d
        &z80::ld<AddressMode::RegisterH, AddressMode::RegisterE>, // 63: ld h, e
        &z80::ld<AddressMode::RegisterH, AddressMode::RegisterH>, // 64: ld h, h
        &z80::ld<AddressMode::RegisterH, AddressMode::RegisterL>, // 65: ld h, l
        &z80::ld<AddressMode::RegisterH, AddressMode::RegisterIndirectHL>, // 66: ld h, (hl)
        &z80::ld<AddressMode::RegisterH, AddressMode::Accumulator>, // 67: ld h, a
        &z80::ld<AddressMode::RegisterL, AddressMode::RegisterB>, // 68: ld l, b
        &z80::ld<AddressMode::RegisterL, AddressMode::RegisterC>, // 69: ld l, c
        &z80::ld<AddressMode::RegisterL, AddressMode::RegisterD>, // 6A: ld l, d
        &z80::ld<AddressMode::RegisterL, AddressMode::RegisterE>, // 6B: ld l, e
        &z80::ld<AddressMode::RegisterL, AddressMode::RegisterH>, // 6C: ld l, h
        &z80::ld<AddressMode::RegisterL, AddressMode::RegisterL>, // 6D: ld l, l
        &z80::ld<AddressMode::RegisterL, AddressMode::RegisterIndirectHL>, // 6E: ld l, (hl)
        &z80::ld<AddressMode::RegisterL, AddressMode::Accumulator>, // 6F: ld l, a
        &z80::ld<AddressMode::RegisterIndirectHL, AddressMode::RegisterB>, // $70: ld (hl), b
        &z80::ld<AddressMode::RegisterIndirectHL, AddressMode::RegisterC>, // $71: ld (hl), c
        &z80::ld<AddressMode::RegisterIndirectHL, AddressMode::RegisterD>, // $72: ld (hl), d
        &z80::ld<AddressMode::RegisterIndirectHL, AddressMode::RegisterE>, // $73: ld (hl), e
        &z80::ld<AddressMode::RegisterIndirectHL, AddressMode::RegisterH>, // $74: ld (hl), h
        &z80::ld<AddressMode::RegisterIndirectHL, AddressMode::RegisterL>, // $75: ld (hl), l
        &z80::halt, // $76: ld (hl), b
        &z80::ld<AddressMode::RegisterIndirectHL, AddressMode::Accumulator>, // $77: ld (hl), a
        &z80::ld<AddressMode::Accumulator, AddressMode::RegisterB>, // $78: ld a, b
        &z80::ld<AddressMode::Accumulator, AddressMode::RegisterC>, // $79: ld a, c
        &z80::ld<AddressMode::Accumulator, AddressMode::RegisterD>, // $7A: ld a, d
        &z80::ld<AddressMode::Accumulator, AddressMode::RegisterE>, // $7B: ld a, e
        &z80::ld<AddressMode::Accumulator, AddressMode::RegisterH>, // $7C: ld a, h
        &z80::ld<AddressMode::Accumulator, AddressMode::RegisterL>, // $7D: ld a, l
        &z80::ld<AddressMode::Accumulator, AddressMode::RegisterIndirectHL>, // $7E: ld a, (hl)
        &z80::ld<AddressMode::Accumulator, AddressMode::Accumulator>, // $7F: ld a, a
        &z80::addb<AddressMode::RegisterB>, // $80: add a, b
        &z80::addb<AddressMode::RegisterC>, // $81: add a, c
        &z80::addb<AddressMode::RegisterD>, // $82: add a, d
        &z80::addb<AddressMode::RegisterE>, // $83: add a, e
        &z80::addb<AddressMode::RegisterH>, // $84: add a, h
        &z80::addb<AddressMode::RegisterL>, // $85: add a, l
        &z80::addb<AddressMode::RegisterIndirectHL>, // $86: add a, (hl)
        &z80::addb<AddressMode::Accumulator>, // $87: add a, a
        &z80::adc<AddressMode::RegisterB>, // $88: adc a, b
        &z80::adc<AddressMode::RegisterC>, // $89: adc a, c
        &z80::adc<AddressMode::RegisterD>, // $8A: adc a, d
        &z80::adc<AddressMode::RegisterE>, // $8B: adc a, e
        &z80::adc<AddressMode::RegisterH>, // $8C: adc a, h
        &z80::adc<AddressMode::RegisterL>, // $8D: adc a, l
        &z80::adc<AddressMode::RegisterIndirectHL>, // $8E: adc a, (hl)
        &z80::adc<AddressMode::Accumulator>, // $8F: adc a, a

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

    Registers regs2_ {};
    Flags flags2_ {};
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
