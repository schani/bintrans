typedef word_32 reg_t;
typedef int label_t;

reg_t ref_integer_reg (int foreign_reg, int reading, int writing);
void unref_integer_reg (reg_t reg);

reg_t ref_float_reg (int foreign_reg, int reading, int writing);
void unref_float_reg (reg_t reg);

void emit (word_32 insn);

#define ref_integer_reg_for_reading(f)                    ref_integer_reg(f,1,0)
#define ref_integer_reg_for_writing(f)                    ref_integer_reg(f,0,1)
#define ref_integer_reg_for_reading_and_writing(f)        ref_integer_reg(f,1,1)

#define ref_gpr_reg_for_reading                           ref_integer_reg_for_reading
#define ref_gpr_reg_for_writing                           ref_integer_reg_for_writing

#define unref_gpr_reg(f)                                  unref_integer_reg(f)

#define ref_float_reg_for_reading(f)                      ref_float_reg(f,1,0)
#define ref_float_reg_for_writing(f)                      ref_float_reg(f,0,1)
#define ref_float_reg_for_reading_and_writing(f)          ref_float_reg(f,1,1)

#define emit_store_mem_32(val,addr)    emit(COMPOSE_STL((val),0,(addr)))
#define emit_load_mem_32(val,addr)     emit(COMPOSE_LDL((val),0,(addr)))

#define NEED_NATIVE        0x1000
