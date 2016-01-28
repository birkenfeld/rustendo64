pub fn mult_64_64(a: u64, b: u64) -> (u64, u64) {
    // Extract the high and low 32-bit parts, keeping everything as u64s.
    let ah = a >> 32;
    let al = a & 0xFFFF_FFFF;
    let bh = b >> 32;
    let bl = b & 0xFFFF_FFFF;
    // Multiply them together.
    let ll = al * bl;
    let hh = ah * bh;
    let m1 = al * bh;
    let m2 = ah * bl;
    // Extract low/high parts from the middle bits.
    let ml1 = m1 << 32;
    let ml2 = m2 << 32;
    let ml = ml1.wrapping_add(ml2);
    let mh = (m1 >> 32) + (m2 >> 32) + (ml < ml1) as u64;
    // Now create the high and low parts, including carry.
    let rl = ll.wrapping_add(ml);
    let rh = hh + mh + (rl < ll) as u64;
    (rl, rh)
}
