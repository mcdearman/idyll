use crate::{token_stream::TokenStream, utils::InternedString};
use itertools::Itertools;

#[derive(Debug, Clone, PartialEq)]
pub struct Pipeline<'src> {
    src: &'src str,
    filename: InternedString,
}

impl<'src> Pipeline<'src> {
    pub fn new(src: &'src str, filename: InternedString) -> Self {
        Self { src, filename }
    }

    pub fn run(&self) {
        let mut token_stream = TokenStream::new(self.src, self.filename);
        println!("{:#?}", token_stream.collect_vec());
        // let (ast, errors) = parse(stream, true);
    }
}