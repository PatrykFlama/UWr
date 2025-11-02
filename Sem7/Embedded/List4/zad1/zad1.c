#include <avr/io.h>
#include <inttypes.h>
#include <util/delay.h>

#include "../../customlib/uart.c"

#define ITERATIONS 2000u
#define PRESCALER 64u

void timer1_init() {
    TCCR1B = _BV(CS11) | _BV(CS10);  // 011 -> prescaler 64
    TCNT1 = 0;
}

#define MEASURE_BINOP(type, tname, opname, op)                                              \
    {                                                                                       \
        TCNT1 = 0;                                                                          \
        volatile type x = (type)3;                                                          \
        volatile type y = (type)2;                                                          \
        volatile type r = (type)0;                                                          \
        /* loop overhead */                                                                 \
        uint16_t over_start = TCNT1;                                                        \
        for (uint32_t i = 0; i < ITERATIONS; ++i) {                                         \
            r = r;                                                                          \
        }                                                                                   \
        uint16_t over_end = TCNT1;                                                          \
        uint32_t overhead_ticks = (uint32_t)((uint16_t)(over_end - over_start));            \
        /* operations */                                                                    \
        uint16_t op_start = TCNT1;                                                          \
        for (uint32_t i = 0; i < ITERATIONS; ++i) {                                         \
            r = (x)op(y);                                                                   \
        }                                                                                   \
        uint16_t op_end = TCNT1;                                                            \
        uint32_t op_ticks = (uint32_t)((uint16_t)(op_end - op_start));                      \
        uint32_t eff_ticks = (op_ticks > overhead_ticks) ? (op_ticks - overhead_ticks) : 0; \
        uint32_t total_cycles = eff_ticks * PRESCALER;                                      \
        uint32_t cycles_per_op = total_cycles / ITERATIONS;                                 \
        uint32_t frac = (total_cycles % ITERATIONS) * 100 / ITERATIONS;                     \
        printf("%-8s %-6s : ticks=%" PRIu32 " cycles/op=%" PRIu32 ".%02" PRIu32 "\r\n",     \
               (tname), (opname), eff_ticks, cycles_per_op, frac);                          \
    }

int main() {
    uart_init();

    timer1_init();
    _delay_ms(100);

    printf("Benchmark start: ITER=%" PRIu32 " prescaler=%d\r\n", (uint32_t)ITERATIONS, PRESCALER);

    // int8_t
    MEASURE_BINOP(int8_t, "int8_t", "add", +);
    MEASURE_BINOP(int8_t, "int8_t", "mul", *);
    MEASURE_BINOP(int8_t, "int8_t", "div", /);

    // int16_t
    MEASURE_BINOP(int16_t, "int16_t", "add", +);
    MEASURE_BINOP(int16_t, "int16_t", "mul", *);
    MEASURE_BINOP(int16_t, "int16_t", "div", /);

    // int32_t
    MEASURE_BINOP(int32_t, "int32_t", "add", +);
    MEASURE_BINOP(int32_t, "int32_t", "mul", *);
    MEASURE_BINOP(int32_t, "int32_t", "div", /);

    // int64_t
    MEASURE_BINOP(int64_t, "int64_t", "add", +);
    MEASURE_BINOP(int64_t, "int64_t", "mul", *);
    MEASURE_BINOP(int64_t, "int64_t", "div", /);

    // float
    MEASURE_BINOP(float, "float", "add", +);
    MEASURE_BINOP(float, "float", "mul", *);
    MEASURE_BINOP(float, "float", "div", /);
}
