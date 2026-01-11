use crate::{token_stream::TokenStream, utils::InternedString};
use itertools::Itertools;

#[derive(Debug, Clone, PartialEq)]
pub struct Pipeline<'src> {
    filename: InternedString,
    src: &'src str,
}

impl<'src> Pipeline<'src> {
    pub fn new(filename: InternedString, src: &'src str) -> Self {
        Self { filename, src }
    }

    pub fn run(&self) {
        let mut token_stream = TokenStream::new(self.src, self.filename);
        println!("{:#?}", token_stream.collect_vec());
        // let (ast, errors) = parse(stream, true);
    }
}
