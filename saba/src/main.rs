#![no_std]
#![no_main]

extern crate alloc;

use alloc::rc::Rc;
use core::cell::RefCell;
use noli::*;
use saba_core::browser::Browser;
use ui_wasabi::app::WasabiUI;

static TEST_HTTP_RESPONSE: &str = r#"HTTP/1.1 200 OK
Data: xx xx xx


<html>
<head></head>
<body>
    <h1 id="title">H1 title</h1>
    <h2 class="class">H2 title</h2>
    <p>Test text.</p>
    <p>
        <a href="example.com">Link1</a>
        <a href="example.com">Link2</a>
    </p>
</body>
</html>
"#;

fn main() -> u64 {
    let browser = Browser::new();

    // Initialize WasabiUI struct.
    let ui = Rc::new(RefCell::new(WasabiUI::new(browser)));

    // Start the application.
    match ui.borrow_mut().start() {
        Ok(_) => {}
        Err(e) => {
            println!("browser fails to start: {:?}", e);
            return 1;
        }
    };

    0
}

entry_point!(main);
