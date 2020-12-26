use crate::instructions::Load;

/// Convert a load instruction into its opcode
impl From<Load> for u16 {
    fn from(load: Load) -> u16 {
        use Load::*;
        match load {
            LdReg8Imm8(dst, _) => {
                match dst {
                    Reg8::B => 0x06,
                    Reg8::C => 0x0E,
                    Reg8::D => 0x16,
                    Reg8::E => 0x1E,
                    Reg8::H => 0x26,
                    Reg8::L => 0x2E,
                    _ => panic!("Unexpected dst Reg8 {:?} for LdReg8Imm8", dst),
                }
            }
            LdReg8Reg8(dst, src) => {
                match (dst, src) {
                    (Reg8::A, Reg8::A) => 0x7F,
                    (Reg8::A, Reg8::B) => 0x78,
                    (Reg8::A, Reg8::C) => 0x79,
                    (Reg8::A, Reg8::D) => 0x7A,
                    (Reg8::A, Reg8::E) => 0x7B,
                    (Reg8::A, Reg8::H) => 0x7C,
                    (Reg8::A, Reg8::L) => 0x7D,
                    (Reg8::B, Reg8::B) => 0x40,
                    (Reg8::B, Reg8::C) => 0x41,
                    (Reg8::B, Reg8::D) => 0x42,
                    (Reg8::B, Reg8::E) => 0x43,
                    (Reg8::B, Reg8::H) => 0x44,
                    (Reg8::B, Reg8::L) => 0x45,
                    (Reg8::C, Reg8::B) => 0x48,
                    (Reg8::C, Reg8::C) => 0x49,
                    (Reg8::C, Reg8::D) => 0x4A,
                    (Reg8::C, Reg8::E) => 0x4B,
                    (Reg8::C, Reg8::H) => 0x4C,
                    (Reg8::C, Reg8::L) => 0x4D,
                    (Reg8::D, Reg8::B) => 0x50,
                    (Reg8::D, Reg8::C) => 0x51,
                    (Reg8::D, Reg8::D) => 0x52,
                    (Reg8::D, Reg8::E) => 0x53,
                    (Reg8::D, Reg8::H) => 0x54,
                    (Reg8::D, Reg8::L) => 0x55,
                    (Reg8::E, Reg8::B) => 0x58,
                    (Reg8::E, Reg8::C) => 0x59,
                    (Reg8::E, Reg8::D) => 0x5A,
                    (Reg8::E, Reg8::E) => 0x5B,
                    (Reg8::E, Reg8::H) => 0x5C,
                    (Reg8::E, Reg8::L) => 0x5D,
                    (Reg8::H, Reg8::B) => 0x60,
                    (Reg8::H, Reg8::C) => 0x61,
                    (Reg8::H, Reg8::D) => 0x62,
                    (Reg8::H, Reg8::E) => 0x63,
                    (Reg8::H, Reg8::H) => 0x64,
                    (Reg8::H, Reg8::L) => 0x65,
                    (Reg8::L, Reg8::B) => 0x68,
                    (Reg8::L, Reg8::C) => 0x69,
                    (Reg8::L, Reg8::D) => 0x6A,
                    (Reg8::L, Reg8::E) => 0x6B,
                    (Reg8::L, Reg8::H) => 0x6C,
                    (Reg8::L, Reg8::L) => 0x6D,
                    _ => panic!("Unexpected Reg8(s) for LdReg8Reg8: src {:?}, dst {:?}", src, dst),
                }
            }
            LdReg8Mem(dst) => {
                match dst {
                    Reg8::A => 0x7E,
                    Reg8::B => 0x46,
                    Reg8::C => 0x4E,
                    Reg8::D => 0x56,
                    Reg8::E => 0x5E,
                    Reg8::H => 0x66,
                    Reg8::L => 0x6E,
                    _ => panic!("Unexpected dst Reg8 for LdReg8Mem: {:?}", dst),
                }
            }
            LdMemReg8(src) => {
                match src {
                    Reg8::B => 0x70,
                    Reg8::C => 0x71,
                    Reg8::D => 0x72,
                    Reg8::E => 0x73,
                    Reg8::H => 0x74,
                    Reg8::L => 0x75,
                    _ => panic!("Unexpected src Reg8 for LdMemReg8: {:?}", src),
                }
            }
            LdMemImm8(_) => 0x36,
            LdAMem(src)  => {
                match src {
                    Reg16::BC => 0x0A,
                    Reg16::DE => 0x0A,
                    Reg16::HL => 0x0A,
                    _ => panic!("Unexpected src Reg16 for LdAMem: {:?}", src),
                }
            }
            LdAMemImm16(_) => 0xFA,
            LdAImm8(_) => 0x3E,
            LdReg8A(dst) => {
                match dst {
                    Reg8::A => 0x7F,
                    Reg8::B => 0x47,
                    Reg8::C => 0x4F,
                    Reg8::D => 0x57,
                    Reg8::E => 0x5F,
                    Reg8::H => 0x67,
                    Reg8::L => 0x6F,
                    _ => panic!("Unexpected dst Reg8 for LdReg8A: {:?}", dst),
                }
            }
            LdMemA(dst) => {
                match dst {
                    Reg16::BC => 0x02,
                    Reg16::DE => 0x12,
                    Reg16::HL => 0x77,
                    _ => panic!("Unexpected dst Reg16 for LdMemA: {:?}", dst),
                }
            }
            LdMemImm16A(_) => 0xEA,
            LdAMemC => 0xF2,
            LdMemCA => 0xE2,
            LddAMemHl => 0x3A,
            LddMemHlA => 0x32,
            LdiAMemHl => 0x2A,
            LdiMemHlA => 0x22,
            LdhMemImm8A(_) => 0xE0,
            LdhAMemImm8(_) => 0xF0,
            LdReg16Imm16(dst, _) => {
                match dst {
                    Reg16::BC => 0x01,
                    Reg16::DE => 0x11,
                    Reg16::HL => 0x21,
                    Reg16::SP => 0x31,
                    _ => panic!("Unexpected dst Reg16 for LdReg16Imm16: {:?}", dst),
                }
            }
            LdSpHl => 0xF9,
            LdHlSpImm8(i8) => 0xF8,
            LdMemImm16Sp(_) => 0x08,
            PushReg16(src) => {
                match src {
                    Reg16::AF => 0xF5,
                    Reg16::BC => 0xC5,
                    Reg16::DE => 0xD5,
                    Reg16::HL => 0xE5,
                    _ => panic!("Unexpected src Reg16 for PushReg16: {:?}", src),
                }
            }
            PopReg16(dst) => {
                match dst {
                    Reg16::AF => 0xF1,
                    Reg16::BC => 0xC1,
                    Reg16::DE => 0xD1,
                    Reg16::HL => 0xE1,
                    _ => panic!("Unexpected dst Reg16 for PopReg16: {:?}", dst),
                }
            }
        }
    }
}
