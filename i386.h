#define I386_PREFIX_LOCK               0x001
#define I386_PREFIX_REPNE              0x002
#define I386_PREFIX_REP                0x004
#define I386_PREFIX_SSE                0x008
#define I386_PREFIX_CS_OVERRIDE        0x010
#define I386_PREFIX_SS_OVERRIDE        0x020
#define I386_PREFIX_DS_OVERRIDE        0x040
#define I386_PREFIX_ES_OVERRIDE        0x080
#define I386_PREFIX_FS_OVERRIDE        0x100
#define I386_PREFIX_GS_OVERRIDE        0x200
#define I386_PREFIX_OP_SIZE_OVERRIDE   0x400
#define I386_PREFIX_ADDR_SIZE_OVERRIDE 0x800

#define I386_NUM_PREFIXES 9

void i386_decode_opcode (interpreter_t *intp, int *prefix_flags, word_8 *opcode1, word_8 *opcode2);
void i386_decode_modrm (interpreter_t *intp, word_8 *_modrm, word_8 *_mod, word_8 *_reg, word_8 *_rm);
void i386_decode_sib (interpreter_t *intp, word_8 modrm, word_8 *_scale, word_8 *_index, word_8 *_base, word_8 *_disp8, word_32 *_disp32);
word_8 i386_decode_imm8 (interpreter_t *intp);
word_16 i386_decode_imm16 (interpreter_t *intp);
word_32 i386_decode_imm32 (interpreter_t *intp);
