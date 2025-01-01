use signal_hook::consts::{SIGHUP, SIGINT, SIGPIPE, SIGQUIT, SIGTERM, SIGTSTP};
use signal_hook::iterator::Signals;
use std::io::{self, Read, Write};
use vm::interpret;

mod chunk;
mod compiler;
mod scanner;
mod vm;

fn run_program(source: &str) {
    interpret(source);
}

const STDIN_FILENO: i32 = 0;

use termios::{tcsetattr, Termios, ECHO, ICANON, TCSANOW};

fn enable_raw_mode() -> Termios {
    let termios = Termios::from_fd(STDIN_FILENO).unwrap();
    let mut raw = termios;

    // Disable canonical mode and echo
    raw.c_lflag &= !(ICANON | ECHO);
    tcsetattr(STDIN_FILENO, TCSANOW, &raw).unwrap();

    termios
}

fn disable_raw_mode(original: Termios) {
    tcsetattr(STDIN_FILENO, TCSANOW, &original).unwrap();
}

fn setup_signals(termios: Termios) {
    let mut signals = Signals::new([SIGHUP, SIGINT, SIGTERM, SIGQUIT, SIGTSTP, SIGPIPE]).unwrap();

    std::thread::spawn(move || {
        signals.forever().for_each(|_| {
            disable_raw_mode(termios);
            std::process::exit(0);
        });
    });
}

fn print_prompt(input: &str, cursor_pos: usize) {
    print!("\r\x1b[K> {}", input);
    let offset = input.len().saturating_sub(cursor_pos);
    if offset > 0 {
        print!("\x1b[{}D", offset);
    }
    io::stdout().flush().unwrap();
}

fn handle_escape_sequence(
    buffer: &mut [u8; 3],
    input: &mut String,
    cursor_pos: &mut usize,
    history: &mut [String],
    current_index: &mut usize,
) {
    if io::stdin().read_exact(&mut buffer[1..3]).is_ok() {
        match &buffer[..3] {
            // Arrow Up
            [b'\x1b', b'[', b'A'] => {
                if *current_index > 0 {
                    *current_index -= 1;
                    input.clone_from(&history[*current_index]);
                    *cursor_pos = input.len();
                }
            }
            // Arrow Down
            [b'\x1b', b'[', b'B'] => {
                if *current_index < history.len() {
                    *current_index += 1;
                    if let Some(new_line) = history.get(*current_index) {
                        input.clone_from(new_line);
                    } else {
                        input.clear();
                    }
                    *cursor_pos = input.len();
                }
            }
            // Arrow Right
            [b'\x1b', b'[', b'C'] => {
                if *cursor_pos < input.len() {
                    *cursor_pos += 1;
                }
            }
            // Arrow Left
            [b'\x1b', b'[', b'D'] => {
                *cursor_pos = cursor_pos.saturating_sub(1);
            }
            _ => {}
        }
    }
}

fn repl() {
    let original_termios = enable_raw_mode();
    let mut history: Vec<String> = Vec::new();
    let mut line_idx: usize = 0;
    let mut input = String::new();
    let mut cursor_pos = 0;

    setup_signals(original_termios);
    loop {
        print_prompt(&input, cursor_pos);
        let mut buffer = [0; 3];
        while let Ok(_n) = io::stdin().read_exact(&mut buffer[..1]) {
            match buffer[0] {
                b'\n' => {
                    println!();
                    if !input.is_empty() && (history.last().map_or(true, |last| last != &input)) {
                        history.push(input.clone());
                    }
                    line_idx = history.len();
                    run_program(&input);
                    input.clear();
                    cursor_pos = 0;
                    break;
                }
                // escape chars
                b'\x1b' => {
                    #[rustfmt::skip]
                    handle_escape_sequence(&mut buffer, &mut input, &mut cursor_pos, &mut history, &mut line_idx);
                    print_prompt(&input, cursor_pos);
                }
                // backspace and delete
                b'\x08' | b'\x7f' => {
                    if cursor_pos > 0 {
                        cursor_pos -= 1;
                        input.remove(cursor_pos);
                    }
                    print_prompt(&input, cursor_pos);
                }
                // normal character, just insert it
                c => {
                    input.insert(cursor_pos, c as char);
                    cursor_pos += 1;
                    print_prompt(&input, cursor_pos);
                }
            }
        }
    }
}

fn run_file(path: &str) {
    let source = std::fs::read_to_string(path).unwrap();
    run_program(&source);
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        run_file(&args[1]);
    }
}
