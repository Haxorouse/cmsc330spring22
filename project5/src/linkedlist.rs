use std::{
    borrow::Borrow,
    borrow::BorrowMut,
    ops::{Deref, DerefMut},
    sync::{Arc, RwLock},
    rc::Rc,
};
use crate::linkedlist::Component::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Component {
    Helmet(bool),              //is damaged?
    LeftThrusters(bool, i32),  //is damaged? How much power left?
    RightThrusters(bool, i32), //is damaged? How much power left?
    LeftRepulsor(bool, i32),   //is damaged? How much power left?
    RightRepulsor(bool, i32),  //is damaged? How much power left?
    ChestPiece(bool, i32),     //is damaged? How much power left?
    Missiles(i32),             //how many missiles left?
    ArcReactor(i32),           // How much power left?
    Wifi(bool),                // connected to wifi?
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Armor {
    pub component: Component,
    pub version: i32,
}

// Part 2

// Students should fill in the Link type themselves. The Node and List types are given as is.
type Link = (Option<Rc<Node>>);

struct Node {
    data: Armor,
    rest: Link,
}

#[derive(Clone)]
pub struct List {
    head_link: Link,
    size: usize,
}

impl List {
    pub fn new() -> Self {
        return List{head_link: None, size: 0};
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn peek(&self) -> Option<Armor> {
        match self.head_link{
            Some(head) => {
                return Some((*Rc::as_ptr(&head)).data);
            }
            None => {
                return None;
            }
        }
    }

    pub fn push(&mut self, component: Armor) {
        match self.head_link{
            Some(head_ref) => {
                self.head_link = Some(Rc::new(Node{data: component, rest: Some(head_ref)}));
                self.size+=1;
            }
            None => {
                self.head_link = Some(Rc::new(Node{data: component, rest: None}));
                self.size+=1;
            }
        }
    }

    pub fn pop(&mut self) -> Option<Armor> {
        let mut head = self.head_link;
        match head{
            Some(n) =>{
                let popped = Rc::into_raw(n);
                self.head_link = (*popped).rest;
                self.size-=1;
                return Some((*popped).data);
            }
            None =>{
                return None;
            }
        }
    }
}

// Part 3

#[derive(Clone)]
pub struct Suit {
    pub armor: List,
    pub version: i32,
}

impl Suit {
    pub fn is_compatible(&self) -> bool {
        match self.armor.head_link{
            Some(head_ref) => {
                return self.comp_rec(*Rc::as_ptr(&head_ref));
            }
            None => {
                return false;
            }
        }
    }

    fn comp_rec(&self, check: Node) -> bool{
        if(check.data.version == self.version){
            match check.rest{
                Some(link_ref) => {
                    return self.comp_rec(*Rc::as_ptr(&link_ref));
                }
                None => {
                    return true;
                }
            }
        }
        return false;
    }

    pub fn repair(&mut self) {
        match self.armor.head_link{
            Some(head_ref) => {
                self.repair_rec(Rc::get_mut(&mut head_ref));
                return;
            }
            None => {
                return;
            }
        }
    }

    fn repair_rec(&mut self, mut opt: Option<Node>){
        match opt {
            Some(peice) => {
                match peice.data.component{
                    Helmet(x) => {}
                    LeftThrusters(dam, pow) => {
                        peice.data.component = LeftThrusters(false, 100);
                    }
                    RightThrusters(dam, pow) => {
                        peice.data.component = RightThrusters(false, 100);
                    }
                    LeftRepulsor(dam, pow) => {
                        peice.data.component = LeftRepulsor(false, 100);
                    }
                    RightRepulsor(dam, pow) => {
                        peice.data.component = RightRepulsor(false, 100);
                    }
                    ChestPiece(dam, pow) => {
                        peice.data.component = ChestPiece(false, 100);
                    }
                    Missiles(x) => {}
                    ArcReactor(x) => {}
                    Wifi(x) => {}
                }
                match peice.rest{
                    Some(node_ref) => {
                        self.repair_rec(Rc::get_mut(&mut node_ref))
                    }
                    None => {
                        return;
                    }
                }
            }
            None => {}
        }
    }
}
