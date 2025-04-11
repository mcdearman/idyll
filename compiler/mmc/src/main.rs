use mmc_pipeline::Pipeline;
use rustyline::{
    error::ReadlineError, validate::Validator, Completer, Editor, Helper, Highlighter, Hinter,
};

#[derive(Completer, Helper, Highlighter, Hinter)]
struct TermValidator;

impl Validator for TermValidator {
    fn validate(
        &self,
        ctx: &mut rustyline::validate::ValidationContext,
    ) -> rustyline::Result<rustyline::validate::ValidationResult> {
        if ctx.input().ends_with("\n") {
            Ok(rustyline::validate::ValidationResult::Valid(None))
        } else {
            Ok(rustyline::validate::ValidationResult::Incomplete)
        }
    }
}

fn main() {
    env_logger::init();

    let h = TermValidator;
    let mut rl = Editor::new().expect("Failed to create editor");
    rl.set_helper(Some(h));
    if rl.load_history(".repl_history").is_err() {
        eprintln!("No previous history.");
    }

    println!("Welcome to MiniML!");

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                match line.trim() {
                    ":q" | ":quit" => break,
                    "clear" => {
                        rl.clear_history().expect("history failed to clear");
                        continue;
                    }
                    _ => (),
                }
                rl.add_history_entry(line.as_str())
                    .expect("Failed to add history entry");

                let pipeline = Pipeline::new(&line);
                pipeline.run();
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history(".repl_history")
        .expect("Failed to save history");
}
