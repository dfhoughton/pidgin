extern crate regex;
use regex::{escape, Regex};
#[macro_use]
extern crate lazy_static;
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::collections::BTreeMap;

lazy_static! {
    static ref ATOMIC: Regex =
        Regex::new(r"\A(?:\[(?:[^\]]|\\.)+\]|\((?:[^)]|\\.)+\)|\\?.)\z").unwrap();
}

#[derive(Clone)]
pub struct Pidgin {
    symbols: BTreeMap<Symbol, String>,
}

impl Pidgin {
    pub fn new() -> Pidgin {
        Pidgin {
            symbols: BTreeMap::new(),
        }
    }
    pub fn add_symbol(&mut self, s: Symbol, rx: String) {
        self.symbols.insert(s, rx);
    }
    pub fn compile(&self, phrases: &[&str]) -> String {
        let mut exp = String::new();
        let mut phrases = self.init(phrases);
        phrases.sort();
        phrases.dedup();
        for e in self.recursive_compile(phrases.as_mut()) {
            exp += e.to_string().as_str();
        }
        exp
    }
    fn compress(&self, mut phrase: Vec<Expression>) -> Vec<Expression> {
        if phrase.len() < 2 {
            return phrase;
        }
        let mut rep_length = 1;
        while rep_length <= phrase.len() / 2 {
            let mut v = Vec::with_capacity(phrase.len());
            let mut i = 0;
            while i < phrase.len() {
                let mut match_length = 1;
                let mut j = i + rep_length;
                'outer: while j <= phrase.len() - rep_length {
                    for k in 0..rep_length {
                        if phrase[i + k] != phrase[j + k] {
                            break 'outer;
                        }
                    }
                    match_length += 1;
                    j += rep_length;
                }
                if match_length > 1 {
                    let mut s = phrase[i..i + rep_length]
                        .iter()
                        .map(Expression::to_string)
                        .collect::<Vec<String>>()
                        .join("");
                    let existing_length = s.len();
                    let atomy = ATOMIC.is_match(&s);
                    let threshold_length = if atomy {
                        existing_length + 3
                    } else {
                        existing_length + 7
                    };
                    if threshold_length <= existing_length * match_length {
                        if atomy {
                            s = format!("{}{{{l}}}", s, l = match_length);
                        } else {
                            s = format!("(?:{}){{{l}}}", s, l = match_length);
                        }
                        v.push(Expression::Part(s, false));
                    } else {
                        for j in 0..(match_length * rep_length) {
                            v.push(phrase[i + j].clone());
                        }
                    }
                    i += match_length * rep_length;
                } else {
                    v.push(phrase[i].clone());
                    i += 1;
                }
            }
            phrase = v;
            rep_length += 1;
        }
        phrase
    }
    fn recursive_compile(&self, phrases: &mut Vec<Vec<Expression>>) -> Vec<Expression> {
        if phrases.len() == 0 {
            return Vec::new();
        }
        if phrases.len() == 1 {
            return self.compress(phrases[0].clone());
        }
        let (prefix, suffix) = self.common_adfixes(phrases);
        let mut prefix = self.compress(prefix);
        let mut suffix = self.compress(suffix);
        phrases.sort();
        let mut map: BTreeMap<&Expression, Vec<&Vec<Expression>>> = BTreeMap::new();
        let mut optional = false;
        for phrase in phrases {
            if phrase.len() == 0 {
                optional = true;
            } else {
                let e = &phrase[0];
                if !map.contains_key(e) {
                    map.insert(e, Vec::new());
                }
                map.get_mut(e).unwrap().push(phrase);
            }
        }
        let mut rv = Vec::new();
        for (_, ref mut v) in map.iter_mut() {
            let mut v = v.iter().map(|v| (*v).clone()).collect();
            rv.push(self.recursive_compile(&mut v));
        }
        rv.sort();
        // should pull out character classes at this point
        let alternates: Vec<Expression> = rv
            .iter()
            .map(|v| {
                Expression::Part(
                    v.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(""),
                    false,
                )
            }).collect();
        prefix.push(Expression::Alternation(alternates, optional));
        prefix.append(&mut suffix);
        prefix
    }
    fn common_adfixes(
        &self,
        phrases: &mut Vec<Vec<Expression>>,
    ) -> (Vec<Expression>, Vec<Expression>) {
        let mut len = 0;
        let mut inverted = false;
        phrases.sort_by(|a, b| a.len().cmp(&b.len()));
        'outer1: for i in 0..phrases[0].len() {
            let e = &phrases[0][i];
            for v in &phrases[1..phrases.len()] {
                if e != &v[i] {
                    break 'outer1;
                }
            }
            len += 1;
        }
        let prefix = if len == 0 {
            Vec::new()
        } else {
            let prefix = phrases[0][0..len].to_vec();
            for i in 0..phrases.len() {
                let l = phrases[i].len() - len;
                phrases[i].reverse();
                phrases[i].truncate(l);
            }
            inverted = true;
            // if the shortest vector is the entire prefix, there can be no common suffix
            if phrases[0].len() == 0 {
                for ref mut v in phrases {
                    v.reverse();
                }
                return (prefix, Vec::new());
            }
            prefix
        };
        len = 0;
        'outer2: for i in 0..phrases[0].len() {
            let index = if inverted {
                i
            } else {
                &phrases[0].len() - i - 1
            };
            let e = &phrases[0][index];
            for v in &phrases[1..phrases.len()] {
                let index = if inverted { i } else { v.len() - i - 1 };
                if e != &v[index] {
                    break 'outer2;
                }
            }
            len += 1;
        }
        let suffix = if len == 0 {
            if inverted {
                for mut v in phrases {
                    v.reverse();
                }
            }
            Vec::new()
        } else {
            let mut suffix = if inverted {
                phrases[0][0..len].to_vec()
            } else {
                let l = phrases[0].len();
                phrases[0][(l - len)..l].to_vec()
            };
            for i in 0..phrases.len() {
                let l = phrases[i].len() - len;
                if inverted {
                    phrases[i].reverse();
                }
                phrases[i].truncate(l);
            }
            if inverted {
                suffix.reverse();
            }
            suffix
        };
        (prefix, suffix)
    }
    // initialize
    fn digest(&self, s: &str) -> Vec<Expression> {
        let mut rv = Vec::new();
        rv.push(Expression::Raw(s.to_string()));
        // apply the symbols to the strings
        for (sym, val) in self.symbols.iter() {
            let mut replacement = String::from("");
            let mut nv = Vec::new();
            for e in rv {
                if let Expression::Raw(s) = e {
                    match sym {
                        Symbol::S(name, b) => {
                            if s.contains(name) {
                                if replacement.len() == 0 {
                                    replacement = if *b {
                                        format!("(?P<{}>{})", name, val)
                                    } else {
                                        format!("(?:{})", val)
                                    };
                                }
                                for (i, s) in s.split(name).enumerate() {
                                    if i > 0 {
                                        nv.push(Expression::Part(replacement.clone(), false));
                                    }
                                    if s.len() > 0 {
                                        nv.push(Expression::Raw(s.to_string()))
                                    }
                                }
                            } else {
                                nv.push(Expression::Raw(s));
                            }
                        }
                        Symbol::Rx(rx, o) => {
                            if rx.is_match(s.as_str()) {
                                if replacement.len() == 0 {
                                    replacement = if let Some(name) = o {
                                        format!("(?P<{}>{})", name, val)
                                    } else {
                                        format!("(?:{})", val)
                                    }
                                }
                                for (i, s) in rx.split(s.as_str()).enumerate() {
                                    if i > 0 {
                                        nv.push(Expression::Part(replacement.clone(), false));
                                    }
                                    if s.len() > 0 {
                                        nv.push(Expression::Raw(s.to_string()))
                                    }
                                }
                            } else {
                                nv.push(Expression::Raw(s));
                            }
                        }
                    }
                } else {
                    nv.push(e)
                }
            }
            rv = nv;
        }
        // convert any remaining raw expressions to sequences of characters
        let mut nv = Vec::new();
        for e in rv {
            if let Expression::Raw(s) = e {
                for c in s.chars() {
                    nv.push(Expression::Char(c, false));
                }
            } else {
                nv.push(e);
            }
        }
        nv
    }
    fn init(&self, phrases: &[&str]) -> Vec<Vec<Expression>> {
        phrases.iter().map(|s| self.digest(s)).collect()
    }
}

#[derive(Clone)]
pub enum Symbol {
    S(String, bool),           // String is name; bool is whether to capture this group
    Rx(Regex, Option<String>), // Option<String> is an optional name
}

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Symbol) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// this defines the order of application -- strings before regex, long before short
// this is to apply more specific symbols before less specific
impl Ord for Symbol {
    fn cmp(&self, other: &Symbol) -> Ordering {
        match self {
            &Symbol::S(ref s1, b1) => match other {
                &Symbol::S(ref s2, b2) => {
                    if s1.len() == s2.len() {
                        if s1 < s2 {
                            Ordering::Less
                        } else if s1 > s2 {
                            Ordering::Greater
                        } else if b1 == b2 {
                            Ordering::Equal
                        } else if b1 {
                            Ordering::Less
                        } else {
                            Ordering::Greater
                        }
                    } else if s1.len() > s2.len() {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                }
                Symbol::Rx(_, _) => Ordering::Less,
            },
            Symbol::Rx(r1, o1) => match other {
                Symbol::S(_, _) => Ordering::Greater,
                Symbol::Rx(r2, o2) => {
                    let s1 = r1.to_string();
                    let s2 = r2.to_string();
                    if s1.len() == s2.len() {
                        if s1 < s2 {
                            Ordering::Less
                        } else if s1 > s2 {
                            Ordering::Greater
                        } else {
                            match o1 {
                                None => match o2 {
                                    None => Ordering::Equal,
                                    Some(_) => Ordering::Greater,
                                },
                                Some(s1) => match o2 {
                                    None => Ordering::Less,
                                    Some(s2) => s1.cmp(s2),
                                },
                            }
                        }
                    } else if s1.len() > s2.len() {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                }
            },
        }
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Symbol) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Symbol {}

// part of a partially-composed regular expression
#[derive(Clone, Debug)]
enum Expression {
    Char(char, bool),
    Alternation(Vec<Expression>, bool),
    Part(String, bool), // partial compilation
    Raw(String),        // initial state preceding conversion into other expressions
}

impl Expression {
    fn is_optional(&self) -> bool {
        match *self {
            Expression::Char(_, b) => b,
            Expression::Alternation(_, b) => b,
            Expression::Part(_, b) => b,
            Expression::Raw(_) => false,
        }
    }
    fn to_string(&self) -> String {
        let s = match self {
            Expression::Char(c, _) => escape(c.to_string().as_ref()),
            Expression::Alternation(v, _) => {
                if v.len() == 1 {
                    v[0].to_string()
                } else {
                    let mut s = String::from("(?:");
                    for (i, e) in v.iter().enumerate() {
                        if i > 0 {
                            s.push('|');
                        }
                        s += e.to_string().as_str();
                    }
                    s.push(')');
                    s
                }
            }
            Expression::Part(s, _) => s.clone(),
            Expression::Raw(s) => s.clone(),
        };
        if self.is_optional() {
            if ATOMIC.is_match(s.as_ref()) {
                s + "?"
            } else {
                String::from("(:?") + s.as_ref() + ")?"
            }
        } else {
            s
        }
    }
}

impl PartialOrd for Expression {
    fn partial_cmp(&self, other: &Expression) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Expression {
    fn cmp(&self, other: &Expression) -> Ordering {
        match self {
            &Expression::Char(c1, b1) => match other {
                &Expression::Char(c2, b2) => match c1.cmp(&c2) {
                    Ordering::Less => Ordering::Less,
                    Ordering::Greater => Ordering::Greater,
                    Ordering::Equal => {
                        if b1 == b2 {
                            Ordering::Equal
                        } else if b1 {
                            Ordering::Greater
                        } else {
                            Ordering::Less
                        }
                    }
                },
                Expression::Alternation(_, _) => Ordering::Less,
                Expression::Part(_, _) => Ordering::Less,
                Expression::Raw(_) => Ordering::Less,
            },
            Expression::Alternation(v1, b1) => match other {
                Expression::Char(_, _) => Ordering::Greater,
                Expression::Alternation(v2, b2) => {
                    for (i, e1) in v1.iter().enumerate() {
                        if i == v2.len() {
                            return Ordering::Less;
                        } else {
                            match e1.cmp(&v2[i]) {
                                Ordering::Less => return Ordering::Less,
                                Ordering::Greater => return Ordering::Greater,
                                _ => (),
                            }
                        }
                    }
                    if v2.len() > v1.len() {
                        Ordering::Less
                    } else if b1 == b2 {
                        Ordering::Equal
                    } else if *b1 {
                        Ordering::Greater
                    } else {
                        Ordering::Less
                    }
                }
                Expression::Part(_, _) => Ordering::Less,
                Expression::Raw(_) => Ordering::Less,
            },
            Expression::Part(s1, b1) => match other {
                Expression::Char(_, _) => Ordering::Greater,
                Expression::Alternation(_, _) => Ordering::Greater,
                Expression::Part(s2, b2) => match s1.cmp(s2) {
                    Ordering::Less => Ordering::Less,
                    Ordering::Greater => Ordering::Greater,
                    Ordering::Equal => {
                        if b1 == b2 {
                            Ordering::Equal
                        } else if *b1 {
                            Ordering::Greater
                        } else {
                            Ordering::Less
                        }
                    }
                },
                Expression::Raw(_) => Ordering::Less,
            },
            Expression::Raw(s1) => match other {
                Expression::Char(_, _) => Ordering::Greater,
                Expression::Alternation(_, _) => Ordering::Greater,
                Expression::Part(_, _) => Ordering::Greater,
                Expression::Raw(s2) => s1.cmp(s2),
            },
        }
    }
}

impl PartialEq for Expression {
    fn eq(&self, other: &Expression) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Expression {}
