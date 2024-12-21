use crate::renderer::dom::node::Node;
use crate::renderer::dom::node::Window;
use crate::renderer::html::token::HtmlToken;
use crate::renderer::html::token::HtmlTokenizer;
use alloc::rc::Rc;
use alloc::vec::Vec;
use core::cell::RefCell;

/// https://html.spec.whatwg.org/multipage/parsing.html#the-insertion-mode
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum InsertionMode {
    Initial,
    BeforeHtml,
    BeforeHead,
    InHead,
    AfterHead,
    InBody,
    Text,
    AfterBody,
    AfterAfterBody,
}

#[derive(Debug, Clone)]
pub struct HtmlParser {
    window: Rc<RefCell<Window>>,
    mode: InsertionMode,
    /// https://html.spec.whatwg.org/multipage/parsing.html#original-insertion-mode
    original_insertion_mode: InsertionMode,
    /// https://html.spec.whatwg.org/multipage/parsing.html#the-stack-of-open-elements
    stack_of_open_elements: Vec<Rc<RefCell<Node>>>,
    t: HtmlTokenizer,
}

impl HtmlParser {
    pub fn new(t: HtmlTokenizer) -> Self {
        Self {
            window: Rc::new(RefCell::new(Window::new())),
            mode: InsertionMode::Initial,
            original_insertion_mode: InsertionMode::Initial,
            stack_of_open_elements: Vec::new(),
            t, // HtmlTokenizer
        }
    }

    pub fn construct_tree(&mut self) -> Rc<RefCell<Window>> {
        let mut token = self.t.next();

        while token.is_some() {
            match self.mode {
                InsertionMode::Initial => {
                    // DOCTYPE is not supported in this version.
                    // So, a token like `<!DOCTYPE html>` displays as a string token.
                    // The string token is ignored.
                    if let Some(HtmlToken::Char(_)) = token {
                        token = self.t.next();
                        continue;
                    }

                    self.mode = InsertionMode::BeforeHtml;
                    continue;
                }
            }
        }

        self.window.clone()
    }
}
