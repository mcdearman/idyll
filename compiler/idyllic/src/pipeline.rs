use crate::token_stream::TokenStream;
use itertools::Itertools;

#[derive(Debug, Clone, PartialEq)]
pub struct Pipeline<'src> {
    src: &'src str,
}

impl<'src> Pipeline<'src> {
    pub fn new(src: &'src str) -> Self {
        Self { src }
    }

    pub fn run(&self) {
        let mut token_stream = TokenStream::new(self.src);
        println!("{:#?}", token_stream.collect_vec());
        // let (ast, errors) = parse(stream, true);
    }
}
