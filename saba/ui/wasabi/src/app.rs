use crate::alloc::string::ToString;
// use crate::cursor::Cursor;
use alloc::format;
use alloc::rc::Rc;
use alloc::string::String;
use core::cell::RefCell;
use noli::error::Result as OsResult;
use noli::prelude::SystemApi;
use noli::println;
use noli::rect::Rect;
use noli::sys::api::MouseEvent;
use noli::sys::wasabi::Api;
use noli::window::StringSize;
use noli::window::Window;
use saba_core::browser::Browser;
use saba_core::constants::WHITE;
use saba_core::constants::WINDOW_HEIGHT;
use saba_core::constants::WINDOW_INIT_X_POS;
use saba_core::constants::WINDOW_INIT_Y_POS;
use saba_core::constants::WINDOW_WIDTH;
use saba_core::constants::*;
use saba_core::display_item::DisplayItem;
use saba_core::error::Error;
use saba_core::http::HttpResponse;
use saba_core::renderer::layout::computed_style::FontSize;
use saba_core::renderer::layout::computed_style::TextDecoration;

#[derive(Debug)]
pub struct WasabiUI {
    browser: Rc<RefCell<Browser>>,
    window: Window,
}

impl WasabiUI {
    pub fn new(browser: Rc<RefCell<Browser>>) -> Self {
        Self {
            browser,
            window: Window::new(
                "saba".to_string(),
                WHITE,
                WINDOW_INIT_X_POS,
                WINDOW_INIT_Y_POS,
                WINDOW_WIDTH,
                WINDOW_HEIGHT,
            )
            .unwrap(),
        }
    }

    pub fn start(&mut self) -> Result<(), Error> {
        self.setup()?;

        self.run_app()?;

        Ok(())
    }

    fn run_app(&mut self) -> Result<(), Error> {
        loop {
            self.handle_mouse_input()?;
            self.handle_key_input()?;
        }
    }

    fn handle_mouse_input(&mut self) -> Result<(), Error> {
        if let Some(MouseEvent { button, position }) = Api::get_mouse_cursor_info() {
            println!("mouse position: {:?}", position);
            if button.l() || button.c() || button.r() {
                println!("mouse clicked {:?}", button);
            }
        }

        Ok(())
    }

    fn handle_key_input(&mut self) -> Result<(), Error> {
        if let Some(c) = Api::read_key() {
            println!("input text: {:?}", c);
        }

        Ok(())
    }

    fn setup(&mut self) -> Result<(), Error> {
        if let Err(error) = self.setup_toolbar() {
            // OsResult and Result have different Error types. So, we need to convert the error type.
            return Err(Error::InvalidUI(format!(
                "failed to initialize a toolbar with error: {:#?}",
                error
            )));
        }
        // Update the window.
        self.window.flush();
        Ok(())
    }

    fn setup_toolbar(&mut self) -> OsResult<()> {
        // Draw the toolbar background.
        self.window
            .fill_rect(LIGHTGREY, 0, 0, WINDOW_WIDTH, TOOLBAR_HEIGHT)?;

        // Draw the border line between toolbar and contents area.
        self.window
            .draw_line(GREY, 0, TOOLBAR_HEIGHT, WINDOW_WIDTH - 1, TOOLBAR_HEIGHT)?;
        self.window.draw_line(
            DRAKGREY,
            0,
            TOOLBAR_HEIGHT + 1,
            WINDOW_WIDTH - 1,
            TOOLBAR_HEIGHT + 1,
        )?;

        // Draw a character like 'Address:' to the side of the address bar.
        self.window.draw_string(
            BLACK,
            5,
            5,
            "Address:",
            StringSize::Medium,
            /*underline=*/ false,
        )?;

        // Draw a square of the address bar.
        self.window
            .fill_rect(WHITE, 70, 2, WINDOW_WIDTH - 74, 2 + ADDRESSBAR_HEIGHT)?;

        // Draw a line of shadow of address bar.
        self.window.draw_line(GREY, 70, 2, WINDOW_WIDTH - 4, 2)?;
        self.window
            .draw_line(GREY, 70, 2, 70, 2 + ADDRESSBAR_HEIGHT)?;
        self.window.draw_line(BLACK, 71, 3, WINDOW_WIDTH - 5, 3)?;

        self.window
            .draw_line(GREY, 71, 3, 71, 1 + ADDRESSBAR_HEIGHT)?;

        Ok(())
    }
}
