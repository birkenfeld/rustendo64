use simd::{i16x8, bool16ix8, i32x4};
use simd::x86::sse2::{Sse2I16x8, Sse2U16x8, Sse2I32x4};

use rsp::Rsp;
use cp2::{format_vec, VCO, VCC, VCE, HI, LO, ACC_HI, ACC_MD, ACC_LO};
use tables::RECIPROCAL_ROM;
use r4k::instruction::*;

// Helpers

#[inline(always)]
fn zero() -> i16x8 {
    i16x8::splat(0)
}

#[inline(always)]
fn frombool(x: bool16ix8) -> i16x8 {
    x.select(i16x8::splat(-1), i16x8::splat(0))
}

trait I16Ext {
    // Add/sub/mul the i16 items as u16
    fn addsu(self, other: Self) -> Self;
    fn subsu(self, other: Self) -> Self;
    fn mulhiu(self, other: Self) -> Self;
    // Shift right logical (default is arithmetic)
    fn srli(self, shift: u32) -> Self;
    // Compare ops that result not in bools
    fn i_eq(self, other: Self) -> Self;
    fn i_ne(self, other: Self) -> Self;
    fn i_gt(self, other: Self) -> Self;
    fn i_lt(self, other: Self) -> Self;
}

impl I16Ext for i16x8 {
    fn addsu(self, other: Self)  -> Self { self.to_u16().adds(other.to_u16()).to_i16() }
    fn subsu(self, other: Self)  -> Self { self.to_u16().subs(other.to_u16()).to_i16() }
    fn mulhiu(self, other: Self) -> Self { self.to_u16().mulhi(other.to_u16()).to_i16() }
    fn srli(self, shift: u32)  -> Self { (self.to_u16() >> shift).to_i16() }
    fn i_eq(self, other: Self) -> Self { frombool(self.eq(other)) }
    fn i_ne(self, other: Self) -> Self { frombool(self.ne(other)) }
    fn i_gt(self, other: Self) -> Self { frombool(self.gt(other)) }
    fn i_lt(self, other: Self) -> Self { frombool(self.lt(other)) }
}

#[allow(dead_code)]
pub fn p128(s: &'static str, v: i16x8) {
    println!("{}: {}", s, format_vec(v));
}

fn sclamp_acc_tomd(acc_md: i16x8, acc_hi: i16x8) -> i16x8 {
    let acc_md = acc_md.to_u16();
    let acc_hi = acc_hi.to_u16();
    // unpack intrinsics are missing...
    let l0 = acc_md.extract(0) as i32 | ((acc_hi.extract(0) as i32) << 16);
    let l1 = acc_md.extract(1) as i32 | ((acc_hi.extract(1) as i32) << 16);
    let l2 = acc_md.extract(2) as i32 | ((acc_hi.extract(2) as i32) << 16);
    let l3 = acc_md.extract(3) as i32 | ((acc_hi.extract(3) as i32) << 16);
    let h0 = acc_md.extract(4) as i32 | ((acc_hi.extract(4) as i32) << 16);
    let h1 = acc_md.extract(5) as i32 | ((acc_hi.extract(5) as i32) << 16);
    let h2 = acc_md.extract(6) as i32 | ((acc_hi.extract(6) as i32) << 16);
    let h3 = acc_md.extract(7) as i32 | ((acc_hi.extract(7) as i32) << 16);
    i32x4::new(l0, l1, l2, l3).packs(i32x4::new(h0, h1, h2, h3))
}

fn uclamp_acc(val: i16x8, acc_md: i16x8, acc_hi: i16x8) -> i16x8 {
    let hi_negative: i16x8 = acc_hi >> 15;
    let md_negative = acc_md >> 15;
    let hi_sign_check = hi_negative.eq(acc_hi);
    let md_sign_check = hi_negative.eq(md_negative);
    let clamp_mask = hi_sign_check & md_sign_check;

    let clamped_val = hi_negative.i_eq(zero());
    // Note, this is a 8x16 select while the C code uses a 16x8 blendv.  Still
    // works as intended since simd booleans have all bits set to 1 when true.
    let res = clamp_mask.select(val, clamped_val);
    res
}

// These operations are basically just translated from CEN64's C code.  Didn't
// bother copying the comments yet.

pub fn vadd(vs: i16x8, vt: i16x8, _: u32, rsp: &mut Rsp) -> i16x8 {
    // Signed sum of carry + source1 + source2.
    let carry = rsp.read_flags(VCO, LO);
    let vd = vs + vt;
    rsp.write_acc(ACC_LO, vd - carry);
    let min = vs.min(vt);
    let max = vs.max(vt);
    let min = min.subs(carry);
    rsp.write_flags(VCO, HI, zero());
    rsp.write_flags(VCO, LO, zero());
    min.adds(max)
}

pub fn vaddc(vs: i16x8, vt: i16x8, _: u32, rsp: &mut Rsp) -> i16x8 {
    let sat_sum = vs.addsu(vt);
    let unsat_sum = vs + vt;
    let sn = sat_sum.i_ne(unsat_sum);
    rsp.write_flags(VCO, HI, zero());
    rsp.write_flags(VCO, LO, sn);
    rsp.write_acc(ACC_LO, unsat_sum);
    unsat_sum
}

pub fn vsub(vs: i16x8, vt: i16x8, _: u32, rsp: &mut Rsp) -> i16x8 {
    let carry = rsp.read_flags(VCO, LO);
    let unsat_diff = vt - carry;
    let sat_diff = vt.subs(carry);
    rsp.write_acc(ACC_LO, vs - unsat_diff);
    let vd = vs.subs(sat_diff);
    let overflow = sat_diff.i_gt(unsat_diff);
    rsp.write_flags(VCO, HI, zero());
    rsp.write_flags(VCO, LO, zero());
    vd.adds(overflow)
}

pub fn vsubc(vs: i16x8, vt: i16x8, _: u32, rsp: &mut Rsp) -> i16x8 {
    let sat_udiff = vs.subsu(vt);
    let equal = vs.i_eq(vt);
    let sat_udiff_zero = sat_udiff.i_eq(zero());
    let eq = equal.i_eq(zero());
    let sn = !equal & sat_udiff_zero;
    let result = vs - vt;
    rsp.write_flags(VCO, HI, eq);
    rsp.write_flags(VCO, LO, sn);
    rsp.write_acc(ACC_LO, result);
    result
}

pub fn vmulx(vs: i16x8, vt: i16x8, op: u32, rsp: &mut Rsp) -> i16x8 {
    let lo = vs * vt;
    let sign1 = lo.srli(15);
    let lo = lo + lo;
    let round = i16x8::splat(1 << 15);
    let hi = vs.mulhi(vt);
    let sign2 = lo.srli(15);
    rsp.write_acc(ACC_LO, round + lo);
    let sign1 = sign1 + sign2;

    let hi = hi << 1;
    let eq = vs.i_eq(vt);
    let neq = eq;
    let accmd = sign1 + hi;
    rsp.write_acc(ACC_MD, accmd);
    let neg = accmd >> 15;

    if op == VMULU {
        let acchi = !eq & neg;
        rsp.write_acc(ACC_HI, acchi);
        let hi = accmd | neg;
        !acchi & hi
    } else {
        let eq = eq & neg;
        let acchi = !neq & neg;
        rsp.write_acc(ACC_HI, acchi);
        accmd + eq
    }
}

pub fn vmxdn(vs: i16x8, vt: i16x8, op: u32, rsp: &mut Rsp) -> i16x8 {
    let lo = vs * vt;
    let hi = vs.mulhiu(vt);
    let sign = vt >> 15;
    let vs = vs & sign;
    let hi = hi - vs;

    if op == VMADN {
        let acc_lo = rsp.read_acc(ACC_LO);
        let acc_md = rsp.read_acc(ACC_MD);
        let acc_hi = rsp.read_acc(ACC_HI);

        let overflow_mask = acc_lo.addsu(lo);
        let acc_lo = acc_lo + lo;

        let overflow_mask = acc_lo.i_ne(overflow_mask);

        let hi = hi - overflow_mask;

        let overflow_mask = acc_md.addsu(hi);
        let acc_md = acc_md + hi;

        let overflow_mask = acc_md.i_ne(overflow_mask);

        let acc_hi = acc_hi + (hi >> 15);
        let acc_hi = acc_hi - overflow_mask;

        rsp.write_acc(ACC_LO, acc_lo);
        rsp.write_acc(ACC_MD, acc_md);
        rsp.write_acc(ACC_HI, acc_hi);
        uclamp_acc(acc_lo, acc_md, acc_hi)
    } else {
        rsp.write_acc(ACC_LO, lo);
        rsp.write_acc(ACC_MD, hi);
        rsp.write_acc(ACC_HI, hi >> 15);
        lo
    }
}

pub fn vmxdh(vs: i16x8, vt: i16x8, op: u32, rsp: &mut Rsp) -> i16x8 {
    let lo = vs * vt;
    let hi = vs.mulhi(vt);

    let (acc_md, acc_hi) = if op == VMADH {
        let acc_md = rsp.read_acc(ACC_MD);
        let acc_hi = rsp.read_acc(ACC_HI);

        let overflow_mask = acc_md.addsu(lo);
        let acc_md = acc_md + lo;

        let overflow_mask = acc_md.i_ne(overflow_mask);

        let hi = hi - overflow_mask;
        let acc_hi = acc_hi + hi;

        rsp.write_acc(ACC_MD, acc_md);
        rsp.write_acc(ACC_HI, acc_hi);
        (acc_md, acc_hi)
    } else {
        rsp.write_acc(ACC_LO, zero());
        rsp.write_acc(ACC_MD, lo);
        rsp.write_acc(ACC_HI, hi);
        (lo, hi)
    };
    sclamp_acc_tomd(acc_md, acc_hi)
}

pub fn vmxdl(vs: i16x8, vt: i16x8, op: u32, rsp: &mut Rsp) -> i16x8 {
    let hi = vs.mulhiu(vt);

    if op == VMADL {
        let acc_lo = rsp.read_acc(ACC_LO);
        let acc_md = rsp.read_acc(ACC_MD);
        let acc_hi = rsp.read_acc(ACC_HI);

        let overflow_mask = acc_lo.addsu(hi);
        let acc_lo = acc_lo + hi;

        let overflow_mask = acc_lo.i_ne(overflow_mask);
        let hi = zero() - overflow_mask;

        let overflow_mask = acc_md.addsu(hi);
        let acc_md = acc_md + hi;

        let overflow_mask = acc_md.i_ne(overflow_mask);
        let acc_hi = acc_hi - overflow_mask;

        rsp.write_acc(ACC_LO, acc_lo);
        rsp.write_acc(ACC_MD, acc_md);
        rsp.write_acc(ACC_HI, acc_hi);
        uclamp_acc(acc_lo, acc_md, acc_hi)
    } else {
        rsp.write_acc(ACC_LO, hi);
        rsp.write_acc(ACC_MD, zero());
        rsp.write_acc(ACC_HI, zero());
        hi
    }
}

pub fn vmxdm(vs: i16x8, vt: i16x8, op: u32, rsp: &mut Rsp) -> i16x8 {
    let lo = vs * vt;
    let hi = vs.mulhiu(vt);
    let sign = vs >> 15;
    let vt = vt & sign;
    let hi = hi - vt;

    if op == VMADM {
        let acc_lo = rsp.read_acc(ACC_LO);
        let acc_md = rsp.read_acc(ACC_MD);
        let acc_hi = rsp.read_acc(ACC_HI);

        let overflow_mask = acc_lo.addsu(lo);
        let acc_lo = acc_lo + lo;

        let overflow_mask = acc_lo.i_ne(overflow_mask);

        let hi = hi - overflow_mask;

        let overflow_mask = acc_md.addsu(hi);
        let acc_md = acc_md + hi;

        let overflow_mask = acc_md.i_ne(overflow_mask);

        let acc_hi = acc_hi + (hi >> 15);
        let acc_hi = acc_hi - overflow_mask;

        rsp.write_acc(ACC_LO, acc_lo);
        rsp.write_acc(ACC_MD, acc_md);
        rsp.write_acc(ACC_HI, acc_hi);
        sclamp_acc_tomd(acc_md, acc_hi)
    } else {
        rsp.write_acc(ACC_LO, lo);
        rsp.write_acc(ACC_MD, hi);
        rsp.write_acc(ACC_HI, hi >> 15);
        hi
    }
}

pub fn vmacx(vs: i16x8, vt: i16x8, op: u32, rsp: &mut Rsp) -> i16x8 {
    let acc_lo = rsp.read_acc(ACC_LO);
    let acc_md = rsp.read_acc(ACC_MD);
    let acc_hi = rsp.read_acc(ACC_HI);

    let lo = vs * vt;
    let hi = vs.mulhi(vt);

    let md = hi << 1;
    let carry = lo.srli(15);
    let hi = hi >> 15;
    let md = md | carry;
    let lo: i16x8 = lo << 1;

    let overflow_mask = acc_lo.addsu(lo);
    let acc_lo = acc_lo + lo;

    let overflow_mask = acc_lo.i_ne(overflow_mask);

    let md: i16x8 = md - overflow_mask;
    let carry = md.i_eq(zero());
    let carry = carry & overflow_mask;
    let hi = hi - carry;

    let overflow_mask = acc_md.addsu(md);
    let acc_md = acc_md + md;

    let overflow_mask = acc_md.i_ne(overflow_mask);

    let acc_hi = acc_hi + hi;
    let acc_hi = acc_hi - overflow_mask;

    let result = if op == VMACU {
        let overflow_hi_mask: i16x8 = acc_hi >> 15;
        let overflow_md_mask = acc_md >> 15;
        let md = overflow_md_mask | acc_md;
        let overflow_mask = acc_hi.i_gt(zero());
        let md = !overflow_hi_mask & md;
        overflow_mask | md
    } else {
        sclamp_acc_tomd(acc_md, acc_hi)
    };
    rsp.write_acc(ACC_LO, acc_lo);
    rsp.write_acc(ACC_MD, acc_md);
    rsp.write_acc(ACC_HI, acc_hi);
    result
}

pub fn vabs(vs: i16x8, vt: i16x8, _: u32, rsp: &mut Rsp) -> i16x8 {
    let vs_zero = vs.i_eq(zero());
    let sign_lt = vs >> 15;
    let vd = !vs_zero & vt;
    let vd = vd ^ sign_lt;
    rsp.write_acc(ACC_LO, vd - sign_lt);
    vd.subs(sign_lt)
}

pub fn vand(vs: i16x8, vt: i16x8, _: u32, rsp: &mut Rsp) -> i16x8 {
    let result = vs & vt;
    rsp.write_acc(ACC_LO, result);
    result
}

pub fn vnand(vs: i16x8, vt: i16x8, _: u32, rsp: &mut Rsp) -> i16x8 {
    let result = !(vs & vt);
    rsp.write_acc(ACC_LO, result);
    result
}

pub fn vor(vs: i16x8, vt: i16x8, _: u32, rsp: &mut Rsp) -> i16x8 {
    let result = vs | vt;
    rsp.write_acc(ACC_LO, result);
    result
}

pub fn vnor(vs: i16x8, vt: i16x8, _: u32, rsp: &mut Rsp) -> i16x8 {
    let result = !(vs | vt);
    rsp.write_acc(ACC_LO, result);
    result
}

pub fn vxor(vs: i16x8, vt: i16x8, _: u32, rsp: &mut Rsp) -> i16x8 {
    let result = vs ^ vt;
    rsp.write_acc(ACC_LO, result);
    result
}

pub fn vnxor(vs: i16x8, vt: i16x8, _: u32, rsp: &mut Rsp) -> i16x8 {
    let result = !(vs ^ vt);
    rsp.write_acc(ACC_LO, result);
    result
}

pub fn vnop(vs: i16x8, _: i16x8, _: u32, _: &mut Rsp) -> i16x8 {
    vs
}

pub fn vch(vs: i16x8, vt: i16x8, _: u32, rsp: &mut Rsp) -> i16x8 {
    let sign = vs ^ vt;
    let sign_bool = sign.lt(zero());
    let sign = frombool(sign_bool);

    let sign_negvt = vt ^ sign;
    let sign_negvt = sign_negvt - sign;

    let diff = vs - sign_negvt;
    let diff_zero = diff.eq(zero());

    let vt_neg = vt.i_lt(zero());
    let diff_lez = diff.gt(zero());
    let diff_gez = frombool(diff_lez | diff_zero);
    let diff_lez = frombool(!diff_lez);

    let ge = sign_bool.select(vt_neg, diff_gez);
    let le = sign_bool.select(diff_lez, vt_neg);

    let vce_bool = diff.eq(sign);
    let vce = frombool(vce_bool) & sign;

    let eq = diff_zero | vce_bool;
    let eq = frombool(!eq);

    let diff_sel_mask = sign_bool.select(le, ge);
    let diff_lez = diff_sel_mask & sign_negvt;
    let diff_gez = !diff_sel_mask & vs;
    let result = diff_lez | diff_gez;

    rsp.write_flags(VCC, HI, ge);
    rsp.write_flags(VCC, LO, le);
    rsp.write_flags(VCO, HI, eq);
    rsp.write_flags(VCO, LO, sign);
    rsp.write_flags(VCE, LO, vce);
    rsp.write_acc(ACC_LO, result);
    result
}

pub fn vcl(vs: i16x8, vt: i16x8, _: u32, rsp: &mut Rsp) -> i16x8 {
    let ge = rsp.read_flags(VCC, HI);
    let le = rsp.read_flags(VCC, LO);
    let eq = rsp.read_flags(VCO, HI);
    let sign = rsp.read_flags(VCO, LO);
    let vce = rsp.read_flags(VCE, LO);

    let sign_negvt = vt ^ sign;
    let sign_negvt = sign_negvt - sign;

    let diff = vs - sign_negvt;
    let ncarry = vs.addsu(vt);
    let ncarry = diff.i_eq(ncarry);
    let nvce = vce.i_eq(zero());
    let diff_zero = diff.i_eq(zero());

    let le_case1 = (diff_zero & ncarry) & nvce;
    let le_case2 = (diff_zero | ncarry) & vce;
    let le_eq = le_case1 | le_case2;

    let ge_eq = vt.subsu(vs);
    let ge_eq = ge_eq.i_eq(zero());

    let do_le = !eq & sign;
    let le_eq = do_le & le_eq;
    let le = !do_le & le;
    let le = le_eq | le;

    let do_ge = eq | sign;
    let ge = do_ge & ge;
    let ge_eq = !do_ge & ge_eq;
    let ge = ge_eq | ge;

    let do_le = sign & le;
    let do_ge = !sign & ge;
    let mux_mask = do_le | do_ge;

    let sign_negvt = mux_mask & sign_negvt;
    let vs = !mux_mask & vs;
    let result = sign_negvt | vs;

    rsp.write_flags(VCC, HI, ge);
    rsp.write_flags(VCC, LO, le);
    rsp.write_flags(VCO, HI, zero());
    rsp.write_flags(VCO, LO, zero());
    rsp.write_flags(VCE, LO, zero());
    rsp.write_acc(ACC_LO, result);
    result
}

pub fn vcr(vs: i16x8, vt: i16x8, _: u32, rsp: &mut Rsp) -> i16x8 {
    let sign = vs ^ vt;
    let sign = sign >> 15;

    let diff_lez = vs & sign;
    let diff_lez = diff_lez + vt;
    let le = diff_lez >> 15;

    let diff_gez = vs | sign;
    let diff_gez = diff_gez.min(vt);
    let ge = diff_gez.i_eq(vt);

    let sign_notvt = vt ^ sign;

    let diff_sel_mask = le - ge;
    let diff_sel_mask = diff_sel_mask & sign;
    let diff_sel_mask = diff_sel_mask + ge;

    let zzero = sign_notvt - vs;
    let zzero = zzero & diff_sel_mask;
    let result = zzero + vs;

    rsp.write_flags(VCC, HI, ge);
    rsp.write_flags(VCC, LO, le);
    rsp.write_flags(VCO, HI, zero());
    rsp.write_flags(VCO, LO, zero());
    rsp.write_flags(VCE, LO, zero());
    rsp.write_acc(ACC_LO, result);
    result
}

pub fn vmrg(vs: i16x8, vt: i16x8, _: u32, rsp: &mut Rsp) -> i16x8 {
    let le = rsp.read_flags(VCC, LO);

    let vs = le & vs;
    let vt = !le & vt;
    let result = vs | vt;

    rsp.write_flags(VCO, HI, zero());
    rsp.write_flags(VCO, LO, zero());
    rsp.write_acc(ACC_LO, result);
    result
}

pub fn vcmp(vs: i16x8, vt: i16x8, op: u32, rsp: &mut Rsp) -> i16x8 {
    let eq = rsp.read_flags(VCO, HI);
    let sign = rsp.read_flags(VCO, LO);

    let equal = vs.i_eq(vt);

    let mut le;
    match op {
        VGE => {
            let gt = vs.i_gt(vt);
            let equalsign = eq & sign;
            let equal = !equalsign & equal;
            le = gt | equal;
        }
        VNE => {
            let nequal = !equal;
            le = eq & equal;
            le = le | nequal;
        }
        VEQ => {
            le = !eq & equal;
        }
        VLT => {
            let lt = vs.i_lt(vt);
            let equal = eq & equal;
            let equal = sign & equal;
            le = lt | equal;
        }
        _   => unreachable!()
    }

    let vs = le & vs;
    let vt = !le & vt;
    let result = vs | vt;

    rsp.write_flags(VCC, HI, zero());
    rsp.write_flags(VCC, LO, le);
    rsp.write_flags(VCO, HI, zero());
    rsp.write_flags(VCO, LO, zero());
    rsp.write_acc(ACC_LO, result);
    result
}

pub fn vmov(vt: i16x8, vt_el: i16, _: u32, rsp: &mut Rsp) -> i16 {
    rsp.write_acc(ACC_LO, vt);
    vt_el
}

pub fn vrcph_vrsqh(vt: i16x8, vt_el: i16, _: u32, rsp: &mut Rsp) -> i16 {
    rsp.write_acc(ACC_LO, vt);
    rsp.get_cp2().dp_flag = 1;
    rsp.get_cp2().div_in = vt_el;
    rsp.get_cp2().div_out
}

pub fn vrcp_vrsqh(vt: i16x8, vt_el: i16, op: u32, rsp: &mut Rsp) -> i16 {
    rsp.write_acc(ACC_LO, vt);
    let dp = op as u8 & rsp.get_cp2().dp_flag;
    rsp.get_cp2().dp_flag = 0;

    let dp_input = (rsp.get_cp2().div_in as u32) << 16 | (vt_el as u16 as u32);
    let sp_input = vt_el as u32;

    let input = (if dp != 0 { dp_input } else { sp_input }) as i32;
    let input_mask = input >> 31;
    let mut data = input ^ input_mask;

    if input > -32768 {
        data -= input_mask;
    }
    let result = if data == 0 {
        0x7fff_ffff
    } else if input == -32768 {
        0xffff_0000_u32 as i32
    } else {
        let shift = data.leading_zeros();
        let idx = (((data as u64) << shift) & 0x7fc0_0000) >> 22;
        let result = if op == VRSQ || op == VRSQL {
            let idx = (idx | 0x200) & 0x3fe | (shift % 2) as u64;
            let tableres = RECIPROCAL_ROM[idx as usize] as i32;
            ((0x10000 | tableres) << 14) >> ((31 - shift) >> 1)
        } else {
            let tableres = RECIPROCAL_ROM[idx as usize] as i32;
            ((0x10000 | tableres) << 14) >> (31 - shift)
        };
        result ^ input_mask
    };
    rsp.get_cp2().div_out = (result >> 16) as i16;
    result as i16
}
