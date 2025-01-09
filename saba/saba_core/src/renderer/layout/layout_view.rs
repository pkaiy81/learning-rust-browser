use crate::constants::CONTENT_AREA_WIDTH;
use crate::display_item::DisplayItem;
use crate::renderer::css::cssom::StyleSheet;
use crate::renderer::dom::api::get_target_element_node;
use crate::renderer::dom::node::ElementKind;
use crate::renderer::dom::node::Node;
use crate::renderer::layout::layout_object::create_layout_object;
use crate::renderer::layout::layout_object::LayoutObject;
use crate::renderer::layout::layout_object::LayoutObjectKind;
use crate::renderer::layout::layout_object::LayoutPoint;
use crate::renderer::layout::layout_object::LayoutSize;
use alloc::rc::Rc;
use alloc::vec::Vec;
use core::cell::RefCell;

fn build_layout_tree(
    node: &Option<Rc<RefCell<Node>>>,
    parent_obj: &Option<Rc<RefCell<LayoutObject>>>,
    cssom: &StyleSheet,
) -> Option<Rc<RefCell<LayoutObject>>> {
    // Try to create a LayoutObject as a node with the function `create_layout_object`.
    // If “display:none” is specified by CSS, the node is not created.
    let mut target_node = node.clone();
    let mut layout_object = create_layout_object(node, parent_obj, cssom); // 1

    // If the node is not created, attempt to create a LayoutObject using a sibling node of the DOM node.
    // Continue traversing sibling nodes until LayoutObject is created.
    while layout_object.is_none() {
        // 2
        if let Some(n) = target_node {
            target_node = n.borrow().next_sibling().clone();
            layout_object = create_layout_object(&target_node, parent_obj, cssom);
        // 3
        } else {
            // If there are no sibling nodes, the DOM tree to be processed is finished and the layout tree created so far is returned.
            return layout_object;
        }
    }

    if let Some(n) = target_node {
        let original_first_child = n.borrow().first_child();
        let original_next_sibling = n.borrow().next_sibling();
        let mut first_child = build_layout_tree(&original_first_child, &layout_object, cssom); // 4
        let mut next_sibling = build_layout_tree(&original_next_sibling, &None, cssom);
        // 5

        // If “display:node” is specified for the child node, no LayoutObject is created, so it tries to create a LayoutObject using the child node's sibling nodes.
        // The process is repeated until either the LayoutObject is created or there are no more sibling nodes to follow.
        if first_child.is_none() && original_first_child.is_some() {
            let mut original_dom_node = original_first_child
                .expect("first child shoud exist")
                .borrow()
                .next_sibling();

            loop {
                first_child = build_layout_tree(&original_dom_node, &layout_object, cssom);

                if first_child.is_none() && original_dom_node.is_some() {
                    original_dom_node = original_dom_node
                        .expect("next sibling should exist")
                        .borrow()
                        .next_sibling();
                    continue;
                }

                break;
            }
        }

        // If “display:node” is specified for the sibling node, no LayoutObject will be created, so attempt to create a LayoutObject using the sibling node of the sibling node.
        // The process is repeated until either a LayoutObject is created or there are no more sibling nodes to follow.
        if next_sibling.is_none() && n.borrow().next_sibling().is_some() {
            let mut original_dom_node = original_next_sibling
                .expect("first child should exist")
                .borrow()
                .next_sibling();

            loop {
                next_sibling = build_layout_tree(&original_dom_node, &None, cssom);

                if next_sibling.is_none() && original_dom_node.is_some() {
                    original_dom_node = original_dom_node
                        .expect("next sibling should exist")
                        .borrow()
                        .next_sibling();
                    continue;
                }

                break;
            }
        }

        let obj = match layout_object {
            Some(ref obj) => obj,
            None => panic!("render object should exist here"),
        };
        obj.borrow_mut().set_first_child(first_child); // 6
        obj.borrow_mut().set_next_sibling(next_sibling); // 7
    }

    layout_object
}

#[derive(Debug, Clone)]
pub struct LayoutView {
    root: Option<Rc<RefCell<LayoutObject>>>,
}

impl LayoutView {
    pub fn new(root: Rc<RefCell<Node>>, cssom: &StyleSheet) -> Self {
        // Try to create a LayoutObject as a node by `create_layout_object` function.
        // If “display:none” is specified by CSS, the node is not created.
        let body_root = get_target_element_node(Some(root), ElementKind::Body);

        let mut tree = Self {
            root: build_layout_tree(&body_root, &None, cssom),
        };

        tree.update_layout();

        tree
    }

    pub fn root(&self) -> Option<Rc<RefCell<LayoutObject>>> {
        self.root.clone()
    }

    fn update_layout(&mut self) {
        Self::calculate_node_size(&self.root, LayoutSize::new(CONTENT_AREA_WIDTH, 0));

        Self::calculate_node_position(
            &self.root,
            LayoutPoint::new(0, 0),
            LayoutObjectKind::Block,
            None,
            None,
        );
    }

    fn calculate_node_size(node: &Option<Rc<RefCell<LayoutObject>>>, parent_size: LayoutSize) {
        if let Some(n) = node {
            // If the node is a block element, determine width before calculating the layout of child node.
            if n.borrow().kind() == LayoutObjectKind::Block {
                n.borrow_mut().compute_size(parent_size); // 1
            }

            let first_child = n.borrow().first_child();
            Self::calculate_node_size(&first_child, n.borrow().size());

            let next_sibling = n.borrow().next_sibling();
            Self::calculate_node_size(&next_sibling, parent_size);

            // Calculate size after determining the size of the child node.
            // If the node is a block element, the height depends on the height of the child node.
            // If the node is an inline element, both the height and width depend on the child node.
            n.borrow_mut().compute_size(parent_size); // 2
        }
    }
}
