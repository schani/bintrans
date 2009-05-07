#define unary_BitNeg(x)              (~(x))

#define unary_ConditionNeg(x)        (!(x))
#define binary_ConditionAnd(x,y)     ((x)&&(y))
#define binary_ConditionOr(x,y)      ((x)||(y))

#define unary_IntZero_8(x)           ((x)==0LL)
#define binary_IntEqual_8(x,y)       ((x)==(y))
#define binary_IntLessU_8(x,y)       ((x)<(y))

#define binary_IntAdd(x,y)           ((x)+(y))
#define binary_IntSub(x,y)           ((x)-(y))
#define binary_IntMul(x,y)           ((x)*(y))

#define binary_ShiftL(x,y)           ((x)<<(y))
#define binary_LShiftR_4(x,y)        ((word_64)((word_32)(x)>>(word_32)(y)))
#define binary_LShiftR_8(x,y)        ((word_64)(x)>>(y))
#define binary_AShiftR_8(x,y)        (ashiftr_64((x),(y)))

#define binary_BitAnd(x,y)           ((x)&(y))
#define binary_BitOr(x,y)            ((x)|(y))
#define binary_BitMask(s,l)          ((l)==0 ? 0LL : (mask_64((s),(s)+(l)-1)))

#define userop_IsMaskMask(x,w)       ((word_64)can_inv_maskmask((w),(x)))
#define reverse_maskmask(x,w)        (inv_maskmask((w),(x)))
