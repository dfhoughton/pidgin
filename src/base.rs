extern crate regex;
use regex::{Captures, Regex};
use std::collections::HashMap;
use util::{Flags,Expression};
use grammar::Grammar;

#[derive(Debug)]
pub struct Matcher {
    rx: Regex,
    root: String,
    translation: HashMap<String, String>,
    parentage: HashMap<String, Vec<String>>,
}

impl Matcher {
    pub fn parse<'t>(&self, s: &'t str) -> Option<Match<'t>> {
        match self.rx.captures(s) {
            Some(captures) => {
                let c = captures.get(0).unwrap();
                let mut m = Match {
                    name: self.root.clone(),
                    text: s,
                    start: c.start(),
                    end: c.end(),
                    children: None,
                };
                self.build_tree(&mut m, &self.root, s, &captures);
                Some(m)
            }
            None => None,
        }
    }
    pub fn is_match(&self, text: &str) -> bool {
        self.rx.is_match(text)
    }
    // recursively build a parse tree showing the groups matched
    fn build_tree<'t>(&self, m: &mut Match<'t>, parent: &str, text: &'t str, captures: &Captures) {
        if let Some(children) = self.parentage.get(parent) {
            for c in children {
                if let Some(n) = captures.name(c) {
                    let name = self.translation.get(c).unwrap();
                    let mut child = Match {
                        text,
                        name: name.clone(),
                        start: n.start(),
                        end: n.end(),
                        children: None,
                    };
                    self.build_tree(&mut child, &c, text, captures);
                    m.children.get_or_insert_with(|| Vec::new()).push(child);
                }
            }
        }
    }
    pub(crate) fn new(g: &Grammar) -> Result<Matcher, regex::Error> {
        let mut idx = 0;
        let mut translation = HashMap::new();
        let mut parentage = HashMap::new();
        let root = g.name.clone().unwrap_or(String::from("0"));
        // parentage.entry(root.clone()).or_insert(Vec::new());
        let mut g = g.clone();
        g.name = Some(root.clone());
        let mut g = Expression::Grammar(g.clone(), false);
        Matcher::walk(
            &mut idx,
            &root,
            &mut g,
            &mut translation,
            &mut parentage,
            true,
        );
        if let Expression::Grammar(g, _) = g {
            match Regex::new(&g.to_s(&Flags::defaults())) {
                Ok(rx) => Ok(Matcher {
                    translation,
                    parentage,
                    rx,
                    root,
                }),
                Err(e) => Err(e),
            }
        } else {
            panic!("there has to be a better way to fool the borrow checker")
        }
    }
    fn walk(
        idx: &mut usize,
        parent: &str,
        e: &mut Expression,
        translation: &mut HashMap<String, String>,
        parentage: &mut HashMap<String, Vec<String>>,
        inserting: bool,
    ) {
        match e {
            Expression::Sequence(v, _) => {
                for e in v {
                    Matcher::walk(idx, parent, e, translation, parentage, inserting);
                }
            }
            Expression::Repetition(e, _, _) => {
                Matcher::walk(idx, parent, e, translation, parentage, inserting)
            }
            Expression::Alternation(v, _) => {
                for e in v {
                    Matcher::walk(idx, parent, e, translation, parentage, inserting);
                }
            }
            Expression::Grammar(g, _) => {
                let new_name = format!("m{}", idx);
                *idx += 1;
                let mut inserting = inserting;
                if inserting {
                    if let Some(n) = g.name.clone() {
                        translation.insert(new_name.clone(), n);
                        parentage
                            .entry(parent.to_string())
                            .or_insert_with(|| Vec::new())
                            .push(new_name.clone());
                    } else {
                        inserting = false;
                    }
                    g.name = if inserting {
                        Some(new_name.clone())
                    } else {
                        None
                    }
                }
                for ref mut e in &mut g.sequence {
                    Matcher::walk(idx, &new_name, e, translation, parentage, inserting);
                }
            }
            _ => (),
        }
    }
}

#[derive(Debug)]
pub struct Match<'t> {
    name: String,
    text: &'t str,
    start: usize,
    end: usize,
    children: Option<Vec<Match<'t>>>,
}

impl<'t> Match<'t> {
    pub fn rule(&self) -> &str {
        &self.name
    }
    pub fn children(&self) -> Option<&[Match<'t>]> {
        match self.children.as_ref() {
            Some(v) => Some(v),
            None => None,
        }
    }
    pub fn start(&self) -> usize {
        self.start
    }
    pub fn end(&self) -> usize {
        self.end
    }
    pub fn value(&self) -> &'t str {
        &self.text[self.start..self.end]
    }
    // recursive search for the value of any named pattern by this name that matched
    pub fn name(&self, name: &str) -> Option<&Match> {
        if self.name == name {
            Some(self)
        } else {
            if self.children.is_some() {
                for m in self.children.as_ref().unwrap() {
                    if let Some(n) = m.name(name) {
                        return Some(n);
                    }
                }
            }
            None
        }
    }
    // collect all the times this rule matched
    pub fn all_names(&self, name: &str) -> Vec<&Match> {
        let mut v = Vec::new();
        self.collect(name, &mut v);
        v
    }
    fn collect(&'t self, name: &str, names: &mut Vec<&'t Match<'t>>) {
        if self.name == name {
            names.push(self);
        }
        if self.children.is_some() {
            for m in self.children.as_ref().unwrap() {
                m.collect(name, names);
            }
        }
    }
}
