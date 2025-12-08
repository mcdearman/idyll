use idyllic_syn::lexer::Lexer;

pub struct Pipeline<'src> {
    src: &'src str,
}

impl<'src> Pipeline<'src> {
    pub fn new(src: &'src str) -> Self {
        Self { src }
    }

    pub fn run(&self) {
        let mut lexer = Lexer::new(self.src);
        let tokens = lexer.collect_tokens();
        for token in tokens {
            println!("{:?} at {:?}", token.kind(), token.span());
        }
    }
}
