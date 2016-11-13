var assert = require('assert');

function skip_args (a, n)
{
    var args = [];
    var i;
    for (i = n; i < a.length; i++)
	args.push(a[i]);
    return args;
}

function int_op (intf, bigf)
{
    var args = skip_args(arguments, 2);

    var x = args[0];
    if (typeof(x) === 'number')
    {
	for (var y of args)
	    assert(Number.isInteger(y));
	return intf.apply(null, args) & 0xffffffff;
    }
    return bigf.apply(null, args);
}

function uint_op (intf, bigf)
{
    var args = skip_args(arguments, 2);

    var x = args[0];
    if (typeof(x) === 'number')
    {
	var uargs = [];
	for (var y of args)
	{
	    assert(Number.isInteger(y));
	    uargs.push(y < 0 ? (~y)+1 : y);
	}
	return intf.apply(null, uargs) & 0xffffffff;
    }
    // FIXME: implement
    return bigf.apply(null, args);
}

function bt_addcarry_8 (op1, op2)
{
    return int_op(function (op1, op2) {
	if ((op1 + op2) >> 8 != 0)
	    return 1;
	return 0;
    }, undefined, op1, op2);
}
exports.addcarry_8 = bt_addcarry_8;

function bt_addcarry_16 (op1, op2)
{
    return int_op(function (op1, op2) {
	if ((op1 + op2) >> 16 != 0)
	    return 1;
	return 0;
    }, undefined, op1, op2);
}
exports.addcarry_16 = bt_addcarry_16;

function bt_addcarry_32 (op1, op2)
{
    return int_op(function (op1, op2) {
	if (((op1 + op2) / 2) >> 31 != 0)
	    return 1;
	return 0;
    }, undefined, op1, op2);
}
exports.addcarry_32 = bt_addcarry_32;

function addoverflow_int(op1, op2, mask)
{
    if ((op1 & mask) === (op2 & mask))
    {
	var result = op1 + op2;

	if ((result & mask) != (op1 & mask))
	    return 1;
    }

    return 0;
}

function bt_addoverflow_8 (op1, op2)
{
    return int_op(function (op1, op2) { return addoverflow_int(op1, op2, 0x80); }, undefined, op1, op2);
}
exports.addoverflow_8 = bt_addoverflow_8;

function bt_addoverflow_16 (op1, op2)
{
    return int_op(function (op1, op2) { return addoverflow_int(op1, op2, 0x8000); }, undefined, op1, op2);
}
exports.addoverflow_16 = bt_addoverflow_16;

function bt_addoverflow_32 (op1, op2)
{
    return int_op(function (op1, op2) { return addoverflow_int(op1, op2, 0x80000000); }, undefined, op1, op2);
}
exports.addoverflow_32 = bt_addoverflow_32;

function bt_ashiftr_8 (v, a)
{
    assert(Number.isInteger(v));
    return ((v << 24) >> (24 + a)) & 0xff;
}
exports.ashiftr_8 = bt_ashiftr_8;

function bt_ashiftr_16 (v, a)
{
    assert(Number.isInteger(v));
    return ((v << 16) >> (16 + a)) & 0xffff;
}
exports.ashiftr_16 = bt_ashiftr_16;

function bt_ashiftr_32 (v, a)
{
    assert(Number.isInteger(v));
    return v >> a;
}
exports.ashiftr_32 = bt_ashiftr_32;

function bt_bit_set_p (v, i)
{
    assert(Number.isInteger(i));
    return int_op(function (v) { return (v >> i) & 1; }, undefined, v);
}
exports.bit_set_p = bt_bit_set_p;

function bt_bitneg (x)
{
    return int_op(function (x) { return ~x; }, undefined, x);
}
exports.bitneg = bt_bitneg;

function bt_bits_to_float_32 (x)
{
    throw "not implemented: bt_bits_to_float_32";
}
exports.bits_to_float_32 = bt_bits_to_float_32;

function bt_bits_to_float_64 (x)
{
    throw "not implemented: bt_bits_to_float_64";
}
exports.bits_to_float_64 = bt_bits_to_float_64;

function bt_booland (x, y)
{
    return int_op(function (x, y) { return (x && y) ? 1 : 0; }, undefined, x, y);
}
exports.booland = bt_booland;

function bt_boolor (x, y)
{
    return int_op(function (x, y) { return (x || y) ? 1 : 0; }, undefined, x, y);
}
exports.boolor = bt_boolor;

function bt_convert_float_32 (x)
{
    throw "not implemented: bt_convert_float_32";
}
exports.convert_float_32 = bt_convert_float_32;

function bt_convert_float_64 (x)
{
    throw "not implemented: bt_convert_float_64";
}
exports.convert_float_64 = bt_convert_float_64;

function bt_equalp (x, y)
{
    return int_op(function (x, y) { return x === y; }, undefined, x, y);
}
exports.equalp = bt_equalp;

function bt_extract_field (v, begin, width)
{
    return int_op(function (v) { return (v >> begin) & ((1 << width) - 1); }, undefined, v);
}
exports.extract_field = bt_extract_field;

function bt_float_to_bits_32 (x)
{
    throw "not implemented: bt_float_to_bits_32";
}
exports.float_to_bits_32 = bt_float_to_bits_32;

function bt_float_to_bits_64 (x)
{
    throw "not implemented: bt_float_to_bits_64";
}
exports.float_to_bits_64 = bt_float_to_bits_64;

function bt_float_to_integer_32 (x)
{
    throw "not implemented: bt_float_to_integer_32";
}
exports.float_to_integer_32 = bt_float_to_integer_32;

function bt_float_to_integer_64 (x)
{
    throw "not implemented: bt_float_to_integer_64";
}
exports.float_to_integer_64 = bt_float_to_integer_64;

function bt_iadd (x, y)
{
    return int_op(function (x, y) { return x + y; }, undefined, x, y);
}
exports.iadd = bt_iadd;

function bt_idiv (x, y)
{
    return uint_op(function (x, y) { return x / y; }, undefined, x, y);
}
exports.idiv = bt_idiv;

function bt_imod (x, y)
{
    return uint_op(function (x, y) { return x % y; }, undefined, x, y);
}
exports.imod = bt_imod;

function bt_imul (x, y)
{
    return int_op(function (x, y) { return x * y; }, undefined, x, y);
}
exports.imul = bt_imul;

function bt_insert_field (x, begin, width, v)
{
    return int_op(function (x, v) { return (x & ~(((1 << width) - 1) << begin)) | (v << begin); }, undefined, x, v);
}
exports.insert_field = bt_insert_field;

function bt_integer_to_float_64 (x)
{
    throw "not implemented: bt_integer_to_float_64";
}
exports.integer_to_float_64 = bt_integer_to_float_64;

function bt_isub (x, y)
{
    var result = int_op(function (x, y) {return x - y;}, undefined, x, y);
    return result;
}
exports.isub = bt_isub;

function bt_leading_zeros (w)
{
    var m = 0x80000000;
    var i;

    for (i = 0; i < 32; ++i)
    {
	if (w & m)
	    break;
	m >>= 1;
    }

    return i;
}
exports.leading_zeros = bt_leading_zeros;

function bt_logand (x, y)
{
    return int_op(function (x, y) { return x & y; }, undefined, x, y);
}
exports.logand = bt_logand;

function bt_logor (x, y)
{
    return int_op(function (x, y) { return x | y; }, undefined, x, y);
}
exports.logor = bt_logor;

function bt_logxor (x, y)
{
    return int_op(function (x, y) { return x ^ y; }, undefined, x, y);
}
exports.logxor = bt_logxor;

function bt_lshiftr (x, a)
{
    return int_op(function (x) {
	return (x >> a) & ~(~0 << (32 - a));
    }, undefined, x);
}
exports.lshiftr = bt_lshiftr;

function bt_mask_32 (begin, end)
{
    var x = 0;
    var b;
    var i;

    if (end < begin)
    {
	b = 1;
	for (i = 0; i <= end; ++i)
	{
	    x = x | b;
	    b <<= 1;
	}

	b = 1 << begin;
	for (i = begin; i < 32; ++i)
	{
	    x = x | b;
	    b <<= 1;
	}
    }
    else
    {
	b = 1 << begin;
	for (i = 0; i <= end - begin; ++i)
	{
	    x = x | b;
	    b <<= 1;
	}
    }

    return x;
}
exports.mask_32 = bt_mask_32;

function bt_mask_64 (begin, end)
{
    throw "not implemented: bt_mask_64";
}
exports.mask_64 = bt_mask_64;

function bt_neg (x)
{
    return int_op(function (x) { return -x; }, undefined, x);
}
exports.neg = bt_neg;

function bt_not (x)
{
    return int_op(function (x) { return x === 0 ? 1 : 0; }, undefined, x);
}
exports.not = bt_not;

function bt_parity_even (op)
{
    var val = 0;

    while (op != 0)
    {
	val ^= op & 1;
	op >>= 1;
    }

    return val ^ 1;
}
exports.parity_even = bt_parity_even;

function bt_promote_word_16 (x)
{
    assert(Number.isInteger(x));
    return x & 0xffff;
}
exports.promote_word_16 = bt_promote_word_16;

function bt_promote_word_32 (x)
{
    throw "not implemented: bt_promote_word_32";
}
exports.promote_word_32 = bt_promote_word_32;

function bt_promote_word_8 (x)
{
    assert(Number.isInteger(x));
    return x & 0xff;
}
exports.promote_word_8 = bt_promote_word_8;

function bt_rotl_16 (x, i)
{
    assert(Number.isInteger(x));
    assert(i <= 16);
    return ((x << i) | bt_lshiftr(x, 16 - i)) & 0xffff;
}
exports.rotl_16 = bt_rotl_16;

function bt_rotl_32 (x, i)
{
    assert(Number.isInteger(x));
    assert(i <= 32);
    return (x << i) | bt_lshiftr(x, 32 - i);
}
exports.rotl_32 = bt_rotl_32;

function bt_sex_16_32 (x)
{
    assert(Number.isInteger(x));
    return (x << 16) >> 16;
}
exports.sex_16_32 = bt_sex_16_32;

function bt_sex_32_64 (x)
{
    throw "not implemented: bt_sex_32_64";
}
exports.sex_32_64 = bt_sex_32_64;

function bt_sex_8_16 (x)
{
    assert(Number.isInteger(x));
    return ((x << 24) >> 24) & 0xffff;
}
exports.sex_8_16 = bt_sex_8_16;

function bt_sex_8_32 (x)
{
    assert(Number.isInteger(x));
    return (x << 24) >> 24;
}
exports.sex_8_32 = bt_sex_8_32;

function bt_shiftl (x, a)
{
    return int_op(function (x) { return x << a; }, undefined, x);
}
exports.shiftl = bt_shiftl;

function bt_sidiv (x, y)
{
    return int_op(function (x, y) { return x / y; }, undefined, x, y);
}
exports.sidiv = bt_sidiv;

function bt_simod (x, y)
{
    return int_op(function (x, y) { return x % y; }, undefined, x, y);
}
exports.simod = bt_simod;

function bt_subcarry_16 (op1, op2)
{
    if ((op1 - op2) >> 16 != 0)
	return 1;
    return 0;
}
exports.subcarry_16 = bt_subcarry_16;

function bt_subcarry_32 (op1, op2)
{
    console.log("subcarry " + op1.toString() + " " + op2.toString());

    var result = op1 - op2;
    if (result != (result & 0xffffffff))
	return 1;
    return 0;
}
exports.subcarry_32 = bt_subcarry_32;

function bt_subcarry_8 (op1, op2)
{
    if ((op1 - op2) >> 8 != 0)
	return 1;
    return 0;
}
exports.subcarry_8 = bt_subcarry_8;

function bt_zex_32_64 (x)
{
    throw "not implemented: bt_zex_32_64";
}
exports.zex_32_64 = bt_zex_32_64;
