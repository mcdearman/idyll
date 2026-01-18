use crate::{token_stream::TokenStream, utils::InternedString};

#[derive(Debug)]
pub struct Parser<'src> {
    src: &'src str,
    tokens: TokenStream<'src>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str, filename: InternedString) -> Self {
        Self {
            src,
            tokens: TokenStream::new(src, filename),
        }
    }
}
