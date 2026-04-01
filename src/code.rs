use std::fmt::{self};
use std::ops::{Deref, DerefMut};

// pub type Instructions = Vec<u8>;
#[derive(Debug, Clone)]
pub struct Instructions(pub Vec<u8>);

impl Instructions {
    pub fn new() -> Instructions {
        Instructions(Vec::new())
    }

    fn fmt_instruction(&self, def: &Definition, operands: &[i32]) -> String {
        let operand_count = def.operand_widths.len();
        if operands.len() != operand_count {
            return format!(
                "ERROR: operand len {} does not match defined {}\n",
                operands.len(),
                operand_count
            );
        }

        match operand_count {
            0 => def.name.to_string(),
            1 => format!("{} {}", def.name, operands[0]),
            _ => format!("ERROR: unhandled operandCount for {}\n", def.name),
        }
    }
}

impl fmt::Display for Instructions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::new();
        let mut i = 0;

        while i < self.0.len() {
            let def = match lookup_byte(self[i]) {
                Ok(d) => d,
                Err(e) => {
                    writeln!(f, "ERROR: {}", e)?;
                    break;
                }
            };

            let (operands, bytes_read) = read_operands(&def, &self[i + 1..]);
            out.push_str(&format!(
                "{:04} {}\n",
                i,
                self.fmt_instruction(&def, &operands)
            ));
            i += 1 + bytes_read;
        }
        // writeln!(f,)
        write!(f, "{}", out)
    }
}

impl Deref for Instructions {
    type Target = Vec<u8>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for Instructions {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[repr(u8)]
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Opcode {
    OpConstant = 0,
    OpAdd,
    OpPop,
}

impl TryFrom<u8> for Opcode {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Opcode::OpConstant),
            1 => Ok(Opcode::OpAdd),
            _ => Err(format!("opcode {}, undefined", value)),
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Opcode::OpConstant => write!(f, "OpConstant"),
            _ => write!(f, "{:?}", self.clone() as u32),
        }
    }
}

#[derive(Debug)]
pub struct Definition {
    name: String,
    operand_widths: Vec<u8>,
}

fn lookup_byte(op: u8) -> Result<Definition, String> {
    lookup(&Opcode::try_from(op)?)
}
fn lookup(op: &Opcode) -> Result<Definition, String> {
    match op {
        Opcode::OpConstant => Ok(Definition {
            name: "OpConstant".to_string(),
            operand_widths: vec![2],
        }),
        Opcode::OpAdd => Ok(Definition {
            name: "OpAdd".to_string(),
            operand_widths: vec![],
        }),
        Opcode::OpPop => Ok(Definition {
            name: "OpPop".to_string(),
            operand_widths: vec![],
        }),
    }
}

pub fn make(op: &Opcode, operands: &[i32]) -> Vec<u8> {
    let def = match lookup(op) {
        Ok(d) => d,
        Err(s) => {
            eprintln!("{}", &s);
            return Vec::new();
        }
    };

    let mut instruction_len: u8 = 1;
    instruction_len += def.operand_widths.iter().sum::<u8>();
    let mut instruction = vec![0u8; instruction_len as usize];
    instruction[0] = op.clone() as u8;

    let mut offset = 1;
    for (i, &o) in operands.iter().enumerate() {
        let width = def.operand_widths[i];
        match width {
            2 => {
                let bytes = (o as u16).to_be_bytes();
                instruction[offset..offset + bytes.len()].copy_from_slice(&bytes);
            }
            _ => {}
        }
        offset += width as usize;
    }

    instruction
}

pub fn read_uint16(ins: &[u8]) -> u16 {
    u16::from_be_bytes([ins[0], ins[1]])
}

pub fn read_operands(def: &Definition, ins: &[u8]) -> (Vec<i32>, usize) {
    let mut operands = vec![0i32; def.operand_widths.len()];
    let mut offset = 0;

    for (i, &width) in def.operand_widths.iter().enumerate() {
        match width {
            2 => operands[i] = read_uint16(&ins[offset..]) as i32,
            _ => {}
        }
        offset += width as usize;
    }
    (operands, offset)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_read_operands() {
        let mut errors: Vec<String> = Vec::new();
        let tests = vec![(Opcode::OpConstant, vec![65536], 2)];

        for t in tests {
            let op = t.0;
            let operands = t.1;
            let bytes_read = t.2;
            let instruction = make(&op, &operands);
            let def = match lookup(&op) {
                Ok(d) => d,
                Err(e) => panic!("definition not found: {}", e),
            };

            let (operands_read, n) = read_operands(&def, &instruction[1..]);
            assert!(n != bytes_read, "n wrong. want={}, got={}", bytes_read, n);

            for (i, want) in operands.iter().enumerate() {
                if operands_read[i] != *want {
                    errors.push(format!(
                        "operand wrong. want={}, got={}",
                        want, operands_read[i]
                    ));
                }
            }
        }

        if !errors.is_empty() {
            for e in errors {
                eprintln!("{}", e);
            }
        }
    }

    #[test]
    fn test_make() {
        let mut errors: Vec<String> = Vec::new();
        let tests = vec![
            (
                Opcode::OpConstant,
                vec![65534],
                vec![Opcode::OpConstant as u8, 255, 254],
            ),
            (Opcode::OpAdd, vec![], vec![Opcode::OpAdd as u8]),
        ];

        for t in tests {
            let op = t.0;
            let operands = t.1;
            let expected = t.2;

            let instruction = make(&op, &operands);

            if instruction.len() != expected.len() {
                errors.push(format!(
                    "instruction has wrong length. want={}, got={}",
                    instruction.len(),
                    expected.len()
                ));
            }

            for (i, v) in expected.iter().enumerate() {
                if instruction[i] != expected[i] {
                    errors.push(format!(
                        "wrong byte at pos {}. want={}, got={}",
                        i, v, instruction[i]
                    ));
                }
            }
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(&Opcode::OpAdd, &vec![]),
            make(&Opcode::OpConstant, &vec![2]),
            make(&Opcode::OpConstant, &vec![65535]),
        ];

        let expected = r"0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535
";

        let mut concatted = Instructions::new();
        for ins in instructions {
            for i in ins {
                concatted.push(i);
            }
        }

        assert!(
            expected == concatted.to_string(),
            "instructions wrongly formatted.\nwant={}\ngot={}",
            expected,
            concatted.to_string()
        )
    }
}
