pub type Instructions = Vec<u8>;

#[repr(u8)]
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Opcode {
    OpConstant = 0,
}

impl From<Opcode> for u8 {
    fn from(op: Opcode) -> u8 {
        op as u8
    }
}

impl TryFrom<u8> for Opcode {
    type Error = String;
    fn try_from(byte: u8) -> Result<Self, Self::Error> {
        match byte {
            0 => Ok(Opcode::OpConstant),
            _ => Err(format!("unknown opcode: {}", byte)),
        }
    }
}

pub struct Definition {
    pub name: Opcode,
    pub operand_widths: Vec<u8>,
}

pub fn lookup(op: u8) -> Option<Definition> {
    let code = Opcode::try_from(op);
    match code {
        Ok(opcode) => match opcode {
            Opcode::OpConstant => Some(Definition {
                name: opcode,
                operand_widths: vec![2],
            }),
        },
        _ => {
            eprintln!("opcode {:?} undefined", op);
            None
        }
    }
}

pub fn make(op: Opcode, operands: &Vec<i32>) -> Vec<u8> {
    let def = match lookup(op.clone() as u8) {
        Some(d) => d,
        None => return vec![],
    };

    let instruction_len = 1 + def
        .operand_widths
        .iter()
        .map(|&w| w as usize)
        .sum::<usize>();
    let mut instruction = vec![0u8; instruction_len];
    instruction[0] = op as u8;

    let mut offset = 1;
    for (i, &o) in operands.iter().enumerate() {
        let width = def.operand_widths[i];
        match width {
            2 => {
                let bytes = (o as u16).to_be_bytes();
                instruction[offset..offset + 2].copy_from_slice(&bytes);
            }
            _ => eprintln!("unsupported operand width: {}", width),
        }
        offset += width as usize;
    }
    instruction
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make() {
        struct TestCase {
            op: Opcode,
            operands: Vec<i32>,
            expected: Vec<u8>,
        }

        let tests = vec![TestCase {
            op: Opcode::OpConstant,
            operands: vec![65534],
            expected: vec![Opcode::OpConstant as u8, 255, 254],
        }];

        let mut failures: Vec<String> = Vec::new();

        for tt in tests {
            let instruction = make(tt.op, &tt.operands);

            if instruction.len() != tt.expected.len() {
                failures.push(format!(
                    "instruction has wrong length. want={}, got={}",
                    tt.expected.len(),
                    instruction.len()
                ));
                // skip byte checks if length is wrong, same as Go's continue behavior
                continue;
            }

            for (i, expected_byte) in tt.expected.iter().enumerate() {
                if instruction[i] != *expected_byte {
                    failures.push(format!(
                        "wrong byte at pos {}. want={}, got={}",
                        i, expected_byte, instruction[i]
                    ));
                }
            }
        }

        assert!(failures.is_empty(), "\n{}", failures.join("\n"));
    }
}
