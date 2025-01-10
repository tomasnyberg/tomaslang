use crate::{compiler::OpCode, vm::Value};

#[derive(Debug, PartialEq, Clone)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub lines: Vec<usize>,
    pub constants: Vec<Value>,
    pub debug_code: Vec<String>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            lines: Vec::new(),
            constants: Vec::new(),
            debug_code: Vec::new(),
        }
    }

    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let constant = self.code[offset + 1];
        print!("{:16} {:4} '", name, constant);
        print!("{}", self.constants[constant as usize]);
        println!("'");
        offset + 2
    }

    fn simple_instruction(&self, name: &str, offset: usize) -> usize {
        println!("{}", name);
        offset + 1
    }

    fn byte_instruction(&self, name: &str, offset: usize) -> usize {
        let slot = self.code[offset + 1];
        println!("{:16} {:4}", name, slot);
        offset + 2
    }

    fn jump_instruction(&self, name: &str, sign: i16, offset: usize) -> usize {
        let jump = ((self.code[offset + 1] as i16) << 8) | self.code[offset + 2] as i16;
        println!(
            "{:16} {:4} -> {}",
            name,
            offset,
            (offset as i16) + 3 + (sign * jump)
        );
        offset + 3
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);
        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines[offset]);
        }

        let instruction: OpCode = OpCode::from_u8(self.code[offset]);
        match instruction {
            OpCode::Constant => self.constant_instruction("CONSTANT_OP", offset),
            OpCode::Access => self.simple_instruction("ACCESS_OP", offset),
            OpCode::AccessSet => self.simple_instruction("ACCESS_SET_OP", offset),
            OpCode::Array => self.byte_instruction("ARRAY_OP", offset),
            OpCode::Add => self.simple_instruction("ADD_OP", offset),
            OpCode::Sub => self.simple_instruction("SUB_OP", offset),
            OpCode::Mul => self.simple_instruction("MUL_OP", offset),
            OpCode::Div => self.simple_instruction("DIV_OP", offset),
            OpCode::DivInt => self.simple_instruction("DIV_INT_OP", offset),
            OpCode::Extend => self.simple_instruction("EXTEND_OP", offset),
            OpCode::Equal => self.simple_instruction("EQUAL_OP", offset),
            OpCode::Mod => self.simple_instruction("MOD_OP", offset),
            OpCode::NotEqual => self.simple_instruction("NOT_EQUAL_OP", offset),
            OpCode::Greater => self.simple_instruction("GREATER_OP", offset),
            OpCode::GreaterEqual => self.simple_instruction("GREATER_EQUAL_OP", offset),
            OpCode::DefineGlobal => self.constant_instruction("DEFINE_GLOBAL_OP", offset),
            OpCode::GetGlobal => self.constant_instruction("GET_GLOBAL_OP", offset),
            OpCode::SetGlobal => self.constant_instruction("SET_GLOBAL_OP", offset),
            OpCode::GetLocal => self.byte_instruction("GET_LOCAL_OP", offset),
            OpCode::SetLocal => self.byte_instruction("SET_LOCAL_OP", offset),
            OpCode::JumpIfFalse => self.jump_instruction("JUMP_IF_FALSE_OP", 1, offset),
            OpCode::JumpIfTrue => self.jump_instruction("JUMP_IF_TRUE_OP", 1, offset),
            OpCode::JumpIfNull => self.jump_instruction("JUMP_IF_NULL_OP", 1, offset),
            OpCode::Jump => self.jump_instruction("JUMP_OP", 1, offset),
            OpCode::Loop => self.jump_instruction("LOOP_OP", -1, offset),
            OpCode::Call => self.simple_instruction("CALL_OP", offset),
            OpCode::Negate => self.simple_instruction("NEGATE_OP", offset),
            OpCode::Not => self.simple_instruction("NOT_OP", offset),
            OpCode::Next => self.simple_instruction("NEXT_OP", offset),
            OpCode::Pop => self.simple_instruction("POP_OP", offset),
            OpCode::Print => self.simple_instruction("PRINT_OP", offset),
            OpCode::Range => self.simple_instruction("RANGE_OP", offset),
            OpCode::Null => self.simple_instruction("NULL_OP", offset),
            OpCode::True => self.simple_instruction("TRUE_OP", offset),
            OpCode::False => self.simple_instruction("FALSE_OP", offset),
            OpCode::Return => self.simple_instruction("RETURN_OP", offset),
            OpCode::Eof => self.simple_instruction("EOF_OP", offset),
        }
    }

    pub fn disassemble(&self, name: &str) {
        if !cfg!(feature = "debug_disassemble") {
            return;
        }
        if cfg!(feature = "debug_raw") {
            println!("{}", self.debug_code.join("\n"));
        }
        println!("===== {} =====", name);
        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }
}
