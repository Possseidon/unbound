use bevy::math::{U16Vec3, UVec3};

use super::extent::OctreeExtent;

// TODO: Only use this for iteration, don't actually store it like this.
//       Iteration will only ever iterate up to 64 items, so this can be really small too.

#[derive(Clone, Copy, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct OctreeOffset {
    /// Stored with bits for x, y and z interleaved so that [`Ord`] sorts by
    /// [Z-order](https://en.wikipedia.org/wiki/Z-order_curve).
    offset: [u32; 3],
}

impl OctreeOffset {
    pub const ZERO: Self = Self { offset: [0; 3] };

    pub const fn new(pos: UVec3) -> Self {
        Self {
            offset: interleave(pos),
        }
    }

    pub const fn get(self) -> UVec3 {
        deinterleave(self.offset)
    }

    /// Returns the next [`OctreeOffset`] according to Z-order that lies within the given extent.
    ///
    /// # Panics
    ///
    /// Panics if the [`OctreeOffset`] is not within the given `extent_mask`.
    pub fn next_within_extent(self, extent_mask: ExtentMask) -> Option<Self> {
        let overflow_mask = !extent_mask.last_valid_offset;
        let mut offset = u128_from_u32_array(self.offset);
        assert!(
            offset & overflow_mask == 0,
            "offset should not be past the extent"
        );
        if offset == extent_mask.last_valid_offset {
            return None;
        }
        offset += 1;
        let result = loop {
            if offset == extent_mask.last_valid_offset {
                break offset;
            }

            let overflow = offset & overflow_mask;
            if overflow == 0 {
                break offset;
            }

            offset += overflow;
        };
        Some(Self {
            offset: u32_array_from_u128(result)?,
        })
    }
}

impl std::fmt::Debug for OctreeOffset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OctreeOffset")
            .field("offset", &self.get())
            .finish()
    }
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct ExtentMask {
    /// The last valid offset within an [`OctreeExtent`].
    ///
    /// Since this is guaranteed to just contain a contiguous range of trailing ones, this can
    /// conveniently be used as a bitmask.
    last_valid_offset: u128,
}

impl ExtentMask {
    pub fn new(extent: OctreeExtent) -> Self {
        let mask = |count: u16, mask: u128| ((1 << (3 * count)) - 1) & mask;

        let U16Vec3 { x, y, z } = extent.size_log2();
        let last_valid_offset = mask(x, X_MASK) | mask(y, Y_MASK) | mask(z, Z_MASK);
        Self { last_valid_offset }
    }
}

const MASK_1: u128 = 0x2492_4924_9249_2492_4924_9249;
const MASK_2: u128 = 0x0C30_C30C_30C3_0C30_C30C_30C3;
const MASK_4: u128 = 0x00F0_0F00_F00F_00F0_0F00_F00F;
const MASK_8: u128 = 0x0000_FF00_00FF_0000_FF00_00FF;
const MASK_16: u128 = 0x0000_0000_FFFF_0000_0000_FFFF;

const X_MASK: u128 = MASK_1;
const Y_MASK: u128 = MASK_1 << 1;
const Z_MASK: u128 = MASK_1 << 2;

/// Interleaves x, y and z components.
///
/// I.e. converts from `[XXXX, YYYY, ZZZZ]` to `[XYZ, XYZ, XYZ, XYZ]`.
const fn interleave(v: UVec3) -> [u32; 3] {
    let interleaved = spread(v.x) | spread(v.y) << 1 | spread(v.z) << 2;
    if let Some(val) = u32_array_from_u128(interleaved) {
        val
    } else {
        panic!("only 96 bits should be in use")
    }
}

/// The inverse operation to [`interleave`], restoring the original [`UVec3`].
///
/// I.e. converts from `[XYZ, XYZ, XYZ, XYZ]` to `[XXXX, YYYY, ZZZZ]`.
const fn deinterleave(offset: [u32; 3]) -> UVec3 {
    let interleaved = u128_from_u32_array(offset);
    UVec3 {
        x: despread(interleaved),
        y: despread(interleaved >> 1),
        z: despread(interleaved >> 2),
    }
}

const fn u32_array_from_u128(interleaved: u128) -> Option<[u32; 3]> {
    if interleaved >> 96 == 0 {
        Some([
            (interleaved >> 64) as u32,
            (interleaved >> 32) as u32,
            interleaved as u32,
        ])
    } else {
        None
    }
}

const fn u128_from_u32_array(offset: [u32; 3]) -> u128 {
    let mut interleaved = 0;
    interleaved |= (offset[0] as u128) << 64;
    interleaved |= (offset[1] as u128) << 32;
    interleaved |= offset[2] as u128;
    interleaved
}

const _TEST_INTERLEAVE: () = {
    const VALUE: UVec3 = UVec3::new(0x1234_5678, 0xFEDC_BA98, 0x192A_3B4C);
    const INTERLEAVED: [u32; 3] = [0x497C_9C4A, 0xBCE0_477C, 0x7C54_BF00];
    assert!(matches!(interleave(VALUE), INTERLEAVED));
    assert!(matches!(deinterleave(INTERLEAVED), VALUE));
};

/// Interleaves each bit in the input value with two `0` bits.
///
/// I.e. turns a bit pattern `ABCD` into `00A00B00C00D`.
const fn spread(v: u32) -> u128 {
    let mut v = v as u128;
    v |= v << 32;
    v &= MASK_16;
    v |= v << 16;
    v &= MASK_8;
    v |= v << 8;
    v &= MASK_4;
    v |= v << 4;
    v &= MASK_2;
    v |= v << 2;
    v &= MASK_1;
    v
}

/// The inverse operation to [`spread`], removing the interleaved bits again.
///
/// I.e. turns a bit pattern `xxAxxBxxCxxD` into `ABCD`.
const fn despread(mut v: u128) -> u32 {
    v &= MASK_1;
    v |= v >> 2;
    v &= MASK_2;
    v |= v >> 4;
    v &= MASK_4;
    v |= v >> 8;
    v &= MASK_8;
    v |= v >> 16;
    v &= MASK_16;
    v |= v >> 32;
    v as u32
}

const _TEST_SPREAD: () = {
    const VALUE: u32 = 0x1234_5678;
    const SPREADED: u128 = 0x0010_0800_9040_0410_4804_9200;
    assert!(spread(VALUE) == SPREADED);
    assert!(despread(SPREADED) == VALUE);
};

#[cfg(test)]
mod tests {
    use bevy::math::U16Vec3;
    use itertools::Itertools;

    use super::*;

    #[test]
    fn next_within_extent() {
        fn check_size(size_log2: U16Vec3) {
            let extent_mask = ExtentMask::new(OctreeExtent::from_size_log2(size_log2).unwrap());
            let iter = || {
                let mut offset = OctreeOffset::new(UVec3::ZERO);
                std::iter::from_fn(move || {
                    offset = offset.next_within_extent(extent_mask)?;
                    Some(offset)
                })
            };

            // just checking total count and order is enough to ensure correct values
            let expected_count = 1 << (size_log2.x + size_log2.y + size_log2.z);
            assert_eq!(iter().count() + 1, expected_count);
            assert!(iter().tuple_windows().all(|(lhs, rhs)| lhs < rhs));
        }

        for ((x, y), z) in (0..4).cartesian_product(0..4).cartesian_product(0..4) {
            check_size(U16Vec3::new(x, y, z));
        }
    }

    #[test]
    #[should_panic = "offset should not be past the extent"]
    fn next_within_extent_panics() {
        OctreeOffset::new(UVec3::X).next_within_extent(ExtentMask::new(OctreeExtent::ONE));
    }
}
