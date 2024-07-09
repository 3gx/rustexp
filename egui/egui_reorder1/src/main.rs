use eframe::egui;
use std::collections::HashSet;

fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([320.0, 240.0]),
        ..Default::default()
    };
    eframe::run_native(
        "Selectable and Reorderable List",
        options,
        Box::new(|_cc| Box::new(MyApp::default())),
    )
}

struct MyApp {
    items: Vec<String>,
    selected: HashSet<usize>,
    dragged_item: Option<usize>,
    drag_started: Option<usize>,
    drag_stopped: Option<usize>,
}

impl Default for MyApp {
    fn default() -> Self {
        Self {
            items: vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
                "d".to_string(),
            ],
            selected: HashSet::new(),
            dragged_item: None,
            drag_started: None,
            drag_stopped: None,
        }
    }
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("Selectable and Reorderable List");
            ui.label("Hold Ctrl to reorder items");

            let ctrl_pressed = ui.input(|i| i.modifiers.ctrl);

            for index in 0..self.items.len() {
                let item = self.items[index].clone();
                let selected = self.selected.contains(&index);

                ui.horizontal(|ui| {
                    let response =
                        ui.selectable_label(selected, item)
                            .on_hover_cursor(if ctrl_pressed {
                                egui::CursorIcon::Grab
                            } else {
                                egui::CursorIcon::Default
                            });

                    if response.clicked() && !ctrl_pressed {
                        self.selected.clear();
                        self.selected.insert(index);
                    } else if response.clicked() && ctrl_pressed {
                        if selected {
                            self.selected.remove(&index);
                        } else {
                            self.selected.insert(index);
                        }
                    }

                    if ctrl_pressed {
                        if response.drag_started() {
                            self.drag_started = Some(index);
                        }

                        if response.dragged() {
                            self.dragged_item = Some(index);
                            ui.ctx().request_repaint();
                        }

                        if response.drag_released() {
                            self.drag_stopped = Some(index);
                        }
                    }
                });
            }

            // Handle drag and drop outside the loop
            if let (Some(start), Some(stop)) = (self.drag_started, self.drag_stopped) {
                if start != stop {
                    let item = self.items.remove(start);
                    self.items.insert(stop, item);
                    self.selected.clear();
                    self.selected.insert(stop);
                }
            }
            self.dragged_item = None;
        });
    }
}
