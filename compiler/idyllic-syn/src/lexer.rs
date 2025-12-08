use idyllic_utils::span::Span;
use logos::{Lexer as LogosLexer, Logos};

use crate::token::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    logos: LogosLexer<'src, TokenKind>,
    peek: Option<Token>,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
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
            if token.kind() == &TokenKind::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        tokens
    }
}
