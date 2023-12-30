#include <gtest/gtest.h>
#include "z80.h"

// Demonstrate some basic assertions.
TEST(HelloTest, BasicAssertions) {
    // Expect two strings not to be equal.
    EXPECT_STRNE("hello", "world");
    // Expect equality.
    EXPECT_EQ(7 * 6, 42);

    struct ram {
        int array[65336] {};

        u8 read8(u16 addr) { return 0; }
        u16 read16(u16 addr) { return 0; }
        void write(u16 addr, u8 val) { }
        void write(u16 addr, u16 val) { }
    };

    z80<ram> z80 {};
    z80.run(10);
}
