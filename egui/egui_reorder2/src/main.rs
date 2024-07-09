// use egui::*;

// #[derive(Clone, PartialEq, Eq)]
// #[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
// pub struct DragAndDropDemo {
//     /// columns with items
//     columns: Vec<Vec<String>>,
// }

// impl Default for DragAndDropDemo {
//     fn default() -> Self {
//         Self {
//             columns: vec![
//                 vec!["Item A", "Item B", "Item C", "Item D"],
//                 vec!["Item E", "Item F", "Item G"],
//                 vec!["Item H", "Item I", "Item J", "Item K"],
//             ]
//             .into_iter()
//             .map(|v| v.into_iter().map(ToString::to_string).collect())
//             .collect(),
//         }
//     }
// }

// impl crate::Demo for DragAndDropDemo {
//     fn name(&self) -> &'static str {
//         "✋ Drag and Drop"
//     }

//     fn show(&mut self, ctx: &Context, open: &mut bool) {
//         use crate::View as _;
//         Window::new(self.name())
//             .open(open)
//             .default_size(vec2(256.0, 256.0))
//             .vscroll(false)
//             .resizable(false)
//             .show(ctx, |ui| self.ui(ui));
//     }
// }

// /// What is being dragged.
// #[derive(Clone, Copy, Debug, PartialEq, Eq)]
// struct Location {
//     col: usize,
//     row: usize,
// }

// impl crate::View for DragAndDropDemo {
//     fn ui(&mut self, ui: &mut Ui) {
//         ui.label("This is a simple example of drag-and-drop in egui.");
//         ui.label("Drag items between columns.");

//         // If there is a drop, store the location of the item being dragged, and the destination for the drop.
//         let mut from = None;
//         let mut to = None;

//         ui.columns(self.columns.len(), |uis| {
//             for (col_idx, column) in self.columns.clone().into_iter().enumerate() {
//                 let ui = &mut uis[col_idx];

//                 let frame = Frame::default().inner_margin(4.0);

//                 let (_, dropped_payload) = ui.dnd_drop_zone::<Location, ()>(frame, |ui| {
//                     ui.set_min_size(vec2(64.0, 100.0));
//                     for (row_idx, item) in column.iter().enumerate() {
//                         let item_id = Id::new(("my_drag_and_drop_demo", col_idx, row_idx));
//                         let item_location = Location {
//                             col: col_idx,
//                             row: row_idx,
//                         };
//                         let response = ui
//                             .dnd_drag_source(item_id, item_location, |ui| {
//                                 ui.label(item);
//                             })
//                             .response;

//                         // Detect drops onto this item:
//                         if let (Some(pointer), Some(hovered_payload)) = (
//                             ui.input(|i| i.pointer.interact_pos()),
//                             response.dnd_hover_payload::<Location>(),
//                         ) {
//                             let rect = response.rect;

//                             // Preview insertion:
//                             let stroke = egui::Stroke::new(1.0, Color32::WHITE);
//                             let insert_row_idx = if *hovered_payload == item_location {
//                                 // We are dragged onto ourselves
//                                 ui.painter().hline(rect.x_range(), rect.center().y, stroke);
//                                 row_idx
//                             } else if pointer.y < rect.center().y {
//                                 // Above us
//                                 ui.painter().hline(rect.x_range(), rect.top(), stroke);
//                                 row_idx
//                             } else {
//                                 // Below us
//                                 ui.painter().hline(rect.x_range(), rect.bottom(), stroke);
//                                 row_idx + 1
//                             };

//                             if let Some(dragged_payload) = response.dnd_release_payload() {
//                                 // The user dropped onto this item.
//                                 from = Some(dragged_payload);
//                                 to = Some(Location {
//                                     col: col_idx,
//                                     row: insert_row_idx,
//                                 });
//                             }
//                         }
//                     }
//                 });

//                 if let Some(dragged_payload) = dropped_payload {
//                     // The user dropped onto the column, but not on any one item.
//                     from = Some(dragged_payload);
//                     to = Some(Location {
//                         col: col_idx,
//                         row: usize::MAX, // Inset last
//                     });
//                 }
//             }
//         });

//         if let (Some(from), Some(mut to)) = (from, to) {
//             if from.col == to.col {
//                 // Dragging within the same column.
//                 // Adjust row index if we are re-ordering:
//                 to.row -= (from.row < to.row) as usize;
//             }

//             let item = self.columns[from.col].remove(from.row);

//             let column = &mut self.columns[to.col];
//             to.row = to.row.min(column.len());
//             column.insert(to.row, item);
//         }

//         ui.vertical_centered(|ui| {
//             ui.add(crate::egui_github_link_file!());
//         });
//     }
// }

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
    columns: Vec<Vec<String>>,
    selected: Option<Location>,
}

impl Default for MyApp {
    fn default() -> Self {
        Self {
            columns: vec![
                vec!["Item A", "Item B", "Item C", "Item D"],
                vec!["Item E", "Item F", "Item G"],
                vec!["Item H", "Item I", "Item J", "Item K"],
            ]
            .into_iter()
            .map(|v| v.into_iter().map(ToString::to_string).collect())
            .collect(),
            selected: None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Location {
    col: usize,
    row: usize,
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        use egui::*;
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.label("This is a simple example of drag-and-drop in egui.");
            ui.label("Drag items between columns.");

            // If there is a drop, store the location of the item being dragged, and the destination for the drop.
            let mut from = None;
            let mut to = None;

            let ctrl_pressed = ui.input(|i| i.modifiers.ctrl);
            ui.columns(self.columns.len(), |uis| {
                for (col_idx, column) in self.columns.clone().into_iter().enumerate() {
                    let ui = &mut uis[col_idx];

                    let frame = Frame::default().inner_margin(4.0);

                    let (_, dropped_payload) = ui.dnd_drop_zone::<Location, ()>(frame, |ui| {
                        ui.set_min_size(vec2(64.0, 100.0));
                        for (row_idx, item) in column.iter().enumerate() {
                            let item_id = Id::new(("my_drag_and_drop_demo", col_idx, row_idx));
                            let item_location = Location {
                                col: col_idx,
                                row: row_idx,
                            };
                            let is_selected = Some(item_location) == self.selected;
                            let response = if ctrl_pressed {
                                ui.dnd_drag_source(item_id, item_location, |ui| {
                                    ui.selectable_label(is_selected, format!("- {item}"));
                                })
                                .response
                            } else {
                                let response =
                                    ui.selectable_label(is_selected, format!("- {item}"));
                                if response.clicked() {
                                    self.selected = Some(item_location);
                                }
                                response
                            };

                            // Detect drops onto this item:
                            if let (Some(pointer), Some(hovered_payload)) = (
                                ui.input(|i| i.pointer.interact_pos()),
                                response.dnd_hover_payload::<Location>(),
                            ) {
                                let rect = response.rect;

                                // Preview insertion:
                                let stroke = egui::Stroke::new(1.0, Color32::WHITE);
                                let insert_row_idx = if *hovered_payload == item_location {
                                    // We are dragged onto ourselves
                                    ui.painter().hline(rect.x_range(), rect.center().y, stroke);
                                    row_idx
                                } else if pointer.y < rect.center().y {
                                    // Above us
                                    ui.painter().hline(rect.x_range(), rect.top(), stroke);
                                    row_idx
                                } else {
                                    // Below us
                                    ui.painter().hline(rect.x_range(), rect.bottom(), stroke);
                                    row_idx + 1
                                };

                                if let Some(dragged_payload) = response.dnd_release_payload() {
                                    // The user dropped onto this item.
                                    from = Some(dragged_payload);
                                    to = Some(Location {
                                        col: col_idx,
                                        row: insert_row_idx,
                                    });
                                }
                            }
                        }
                    });

                    if let Some(dragged_payload) = dropped_payload {
                        // The user dropped onto the column, but not on any one item.
                        from = Some(dragged_payload);
                        to = Some(Location {
                            col: col_idx,
                            row: usize::MAX, // Inset last
                        });
                    }
                }
            });

            if let (Some(from), Some(mut to)) = (from, to) {
                if from.col == to.col {
                    // Dragging within the same column.
                    // Adjust row index if we are re-ordering:
                    let target_to =
                        (to.row - (from.row < to.row) as usize).min(self.columns[to.col].len());
                    if let Some(mut loc) = self.selected {
                        let to_row = to.row;
                        let loc_row = loc.row;
                        let from_row = from.row;
                        println!("to= {to_row}/{target_to},  loc= {loc_row}  rfom= {from_row}");
                        if from.row == loc.row {
                            loc.row = target_to;
                        } else if to.row <= loc.row && loc.row < from.row {
                            loc.row += 1;
                        } else if from.row < loc.row && loc.row <= to.row {
                            loc.row -= 1;
                        }
                        self.selected = Some(loc);
                    }
                    to.row = target_to;
                }

                // let selected_item = if let Some(loc) = &self.selected {
                //     Some(self.columns[loc.col][loc.row].clone())
                // } else {
                //     None
                // };
                let item = self.columns[from.col].remove(from.row);

                let column = &mut self.columns[to.col];
                to.row = to.row.min(column.len());
                column.insert(to.row, item);
                // if let Some(mut loc) = self.selected {
                //     if loc == *from {
                //         self.selected = Some(to);
                //     } else {
                //         let item = selected_item.unwrap();
                //         println!("selected_item= {item}");
                //         let index = self.columns[loc.col]
                //             .iter()
                //             .position(|x| x == &item)
                //             .unwrap();
                //         loc.row = index;
                //         self.selected = Some(loc);
                //         let item = &self.columns[loc.col][loc.row];
                //         println!("item= {item}");
                //     }
                // }
            }

            ui.vertical_centered(|ui| {
                ui.label("Ta-da!");
                //                ui.add(crate::egui_github_link_file!());
            });
        });
    }
}
