extern crate regex;
use crate::grammar::Grammar;
use crate::util::{Expression, Flags};
use regex::{Captures, Error, Regex};
use std::collections::HashMap;

/// This is functionally equivalent to a `Regex`: you can use it repeatedly to
/// search a string. It cannot itself be used directly to split strings, but
/// its regular expression is public and may be so used. It improves on regular
/// expressions in that the `Match` object it returns is the root node in a
/// parse tree, so its matches preserve parse structure.
#[derive(Debug)]
pub struct Matcher {
    /// The `Regex` used for parsing.
    pub rx: Regex,
    root: String,
    translation: HashMap<String, String>,
    parentage: HashMap<String, Vec<String>>,
}

impl Matcher {
    /// Returns `Some(Match)` if the grammar can parse the string. Note that
    /// unless the grammar is string-bounded, this only means it can parse
    /// the string at some point.
    pub fn parse<'t>(&self, s: &'t str) -> Option<Match<'t>> {
        match self.rx.captures(s) {
            Some(captures) => {
                let c = captures.get(0).unwrap();
                let mut m = Match {
                    rule: self.root.clone(),
                    text: s,
                    start: c.start(),
                    end: c.end(),
                    children: None,
                };
                self.build_tree(&mut m, &self.root, s, &captures);
                let m = self.simplify_tree(m);
                Some(m)
            }
            None => None,
        }
    }
    fn simplify_tree<'t>(&self, m: Match<'t>) -> Match<'t> {
        if m.children.is_some() {
            let mut clone = m.clone();
            let children: Vec<Match> = m
                .children
                .unwrap()
                .into_iter()
                .map(|c| self.simplify_tree(c))
                .collect();
            if children.len() == 1 && m.rule == children[0].rule {
                children[0].clone()
            } else {
                clone.children = Some(children);
                clone
            }
        } else {
            m
        }
    }
    /// Returns whether the grammar can parse the string. This is a cheaper
    /// operation than parsing.
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
                        rule: name.clone(),
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
    pub(crate) fn new(g: &Grammar) -> Result<Matcher, Error> {
        let mut idx = 0;
        let mut translation = HashMap::new();
        let mut parentage = HashMap::new();
        let root = g.name.clone().unwrap_or(String::from("0"));
        let mut g = g.clone();
        g.name = Some(root.clone());
        let mut g = Expression::Grammar(g.clone(), false);
        Matcher::walk(&mut idx, &root, &mut g, &mut translation, &mut parentage);
        if let Expression::Grammar(g, _) = g {
            match Regex::new(&g.to_s(&Flags::defaults(), false, false)) {
                Ok(rx) => Ok(Matcher {
                    translation,
                    parentage,
                    rx,
                    root,
                }),
                Err(e) => Err(e),
            }
        } else {
            unreachable!()
        }
    }
    fn walk(
        idx: &mut usize,
        parent: &str,
        e: &mut Expression,
        translation: &mut HashMap<String, String>,
        parentage: &mut HashMap<String, Vec<String>>,
    ) {
        match e {
            Expression::Sequence(v, _) => {
                for e in v {
                    Matcher::walk(idx, parent, e, translation, parentage);
                }
            }
            Expression::Repetition(e, _, _) => {
                Matcher::walk(idx, parent, e, translation, parentage)
            }
            Expression::Alternation(v, _) => {
                for e in v {
                    Matcher::walk(idx, parent, e, translation, parentage);
                }
            }
            Expression::Grammar(g, _) => {
                if let Some(n) = g.name.clone() {
                    let new_name = format!("m{}", idx);
                    *idx += 1;
                    translation.insert(new_name.clone(), n);
                    parentage
                        .entry(parent.to_string())
                        .or_insert_with(|| Vec::new())
                        .push(new_name.clone());
                    for e in &mut g.sequence {
                        Matcher::walk(idx, &new_name, &mut *e, translation, parentage);
                    }
                    g.name = Some(new_name.clone());
                } else {
                    for ref mut e in &mut g.sequence {
                        Matcher::walk(idx, parent, e, translation, parentage);
                    }
                }
            }
            _ => (),
        }
    }
}

/// This is a node in a parse tree. It is functionally similar to `regex::Match`,
/// in fact providing much the same API, but unlike a `regex::Match` a `pidgin::Match`
/// always corresponds to some rule, it knows what rule it corresponds to,
/// and it records any sub-matches involved in its parsing.
///
/// The lifetime parameter `'t` represents the lifetime of the `&str` matched
/// against.
#[derive(Debug, Clone)]
pub struct Match<'t> {
    rule: String,
    text: &'t str,
    start: usize,
    end: usize,
    children: Option<Vec<Match<'t>>>,
}

impl<'t> Match<'t> {
    /// Returns the matched text.
    pub fn as_str(&self) -> &'t str {
        &self.text[self.start..self.end]
    }
    /// Returns the starting offset of the match.
    pub fn start(&self) -> usize {
        self.start
    }
    /// Returns the ending offset of the match.
    pub fn end(&self) -> usize {
        self.end
    }
    /// Returns the grammar rule matched.
    pub fn rule(&self) -> &str {
        &self.rule
    }
    /// Returns the sub-matches of this match, if any.
    pub fn children(&self) -> Option<&[Match<'t>]> {
        match self.children.as_ref() {
            Some(v) => Some(v),
            None => None,
        }
    }
    /// Returns the first `Match` defined by the given rule under this parse
    /// node searching recursively, depth-first, left-to-right.
    pub fn name(&self, name: &str) -> Option<&Match> {
        if self.rule == name {
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
    /// Returns all `Match`es matching the given rule in the parse tree under
    /// this node. Matches are ordered as found by a depth-first left-to-right
    /// search of the parse tree.
    pub fn all_names(&self, name: &str) -> Vec<&Match> {
        let mut v = Vec::new();
        self.collect(name, &mut v);
        v
    }
    /// Returns whether the given rule matched for any node in the parse tree.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use pidgin::Pidgin; use std::error::Error;
    /// # fn demo() -> Result<(), Box<Error>> {
    /// let mut p = Pidgin::new();
    /// let g = p.grammar(&vec!["cat", "dog", "camel"]);
    /// p.rule("animal", &g);
    /// let g = p.grammar(&vec!["carpet", "crate", "cartoon"]);
    /// p.rule("thing", &g);
    /// let m = p.grammar(&vec!["animal", "thing"]).matcher()?;
    /// assert!(m.parse("cat").unwrap().has("animal"));
    /// # Ok(())}
    /// ```
    pub fn has(&self, name: &str) -> bool {
        self.rule == name || self.children.is_some() && self
            .children
            .as_ref()
            .unwrap()
            .iter()
            .any(|ref m| m.has(name))
    }
    fn collect(&'t self, name: &str, names: &mut Vec<&'t Match<'t>>) {
        if self.rule == name {
            names.push(self);
        }
        if self.children.is_some() {
            for m in self.children.as_ref().unwrap() {
                m.collect(name, names);
            }
        }
    }
    // display the parse tree with indentation
    fn _fmt(&self, depth: usize, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let own = format!(
            "{}{} ({}, {}): {}\n",
            "  ".repeat(depth),
            self.rule,
            self.start,
            self.end,
            format!("{:?}", self.as_str())
        );
        if let Some(ref children) = &self.children {
            let mut r = write!(f, "{}", own);
            for c in children {
                r = c._fmt(depth + 1, f);
            }
            r
        } else {
            write!(f, "{}", own)
        }
    }
}

impl<'t> std::fmt::Display for Match<'t> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self._fmt(0, f)
    }
}
