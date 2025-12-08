use idyllic_syn::lexer::Lexer;

pub struct Pipeline<'src> {
    src: &'src str,
    lexer: Lexer<'src>,
}

impl<'src> Pipeline<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            lexer: Lexer::new(src),
        }
    }

    pub fn run(&mut self) {
        let tokens = self.lexer.collect_tokens();
        for token in tokens {
            println!("{:?} at {:?}", token.kind(), token.span());
        }
    }
}
