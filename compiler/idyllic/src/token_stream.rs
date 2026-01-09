use logos::{Lexer, Logos};

use crate::{
    token::{Token, TokenKind},
    utils::Span,
};

#[derive(Debug, Clone)]
pub struct TokenStream<'src> {
    logos: Lexer<'src, TokenKind>,
    peek: Option<Token>,
}

impl<'src> TokenStream<'src> {
    pub(crate) fn new(src: &'src str) -> Self {
        Self {
            logos: TokenKind::lexer(src),
            peek: None,
        }
    }

    pub fn fetch_token(&mut self) -> Token {
        match self.logos.next().map(|res| match res {
            Ok(t) => (t, Span::from(self.logos.span())),
            Err(_) => (TokenKind::Error, Span::from(self.logos.span())),
        }) {
            Some((token, s)) => Token::new(token, s),
            None => Token::new(TokenKind::Eof, Span::from(self.logos.span())),
        }
    }

    pub fn peek(&mut self) -> Token {
        if let Some(token) = self.peek.clone() {
            token
        } else {
            let token = self.fetch_token();
            self.peek = Some(token.clone());
            token
        }
    }

    pub fn next(&mut self) -> Token {
        if let Some(token) = self.peek.take() {
            self.peek = None;
            token
        } else {
            self.fetch_token()
        }
    }

    pub fn collect_tokens(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next();
            if token.item == TokenKind::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        tokens
    }
}

impl<'src> Iterator for TokenStream<'src> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next();
        if token.item == TokenKind::Eof {
            None
        } else {
            Some(token)
        }
    }
}