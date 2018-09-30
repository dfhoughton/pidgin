extern crate regex;
use regex::{escape, Regex};
#[macro_use]
extern crate lazy_static;
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::collections::BTreeMap;

fn is_atomic(s: &str) -> bool {
    lazy_static! {
        static ref ATOMIC: Regex = Regex::new(
            r"(?x)
                \A
                (?:
                    .
                        |
                    \[ (?: [^\]] | \\. )+ \]
                        |
                    \( (?: [^)] | \\. )+ \)
                        |
                    \\ (?: # there are more of these, but we only match the less obscure ones
                        [pP] (?: [a-zA-Z] | \{ [a-zA-Z]+ \})
                            |
                        .
                    )
                )
                \z
            "
        ).unwrap();
    }
    ATOMIC.is_match(s)
}

#[derive(Clone)]
pub struct Pidgin {
    left: Option<Boundary>,
    right: Option<Boundary>,
    symbols: BTreeMap<Symbol, Vec<String>>,
    phrases: Vec<String>,
}

impl Pidgin {
    pub fn new() -> Pidgin {
        Pidgin {
            left: None,
            right: None,
            symbols: BTreeMap::new(),
            phrases: Vec::new(),
        }
    }
    pub fn add(&mut self, phrases: &[&str]) {
        for w in phrases {
            self.phrases.push(w.to_string());
        }
    }
    pub fn clear(&mut self) {
        self.symbols.clear();
        self.phrases.clear();
    }
    pub fn rule(&mut self, name: &str, pattern: &str) {
        self.add_symbol(Symbol::S(name.to_string(), true), pattern);
    }
    pub fn nm_rule(&mut self, name: &str, pattern: &str) {
        self.add_symbol(Symbol::S(name.to_string(), false), pattern);
    }
    pub fn rx_rule(
        &mut self,
        rx: &str,
        pattern: &str,
        name: Option<&str>,
    ) -> Result<&mut Pidgin, regex::Error> {
        match Regex::new(rx) {
            Ok(rx) => {
                let name = match name {
                    Some(n) => Some(n.to_string()),
                    None => None,
                };
                self.add_symbol(Symbol::Rx(rx, name), pattern);
                Ok(self)
            }
            Err(e) => Err(e),
        }
    }
    pub fn normalize_whitespace(mut self) -> Pidgin {
        self.rx_rule(r"\s+", r"\s+", None).unwrap();
        self
    }
    pub fn word_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::Word);
        self.right = Some(Boundary::Word);
        self
    }
    pub fn left_word_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::Word);
        self
    }
    pub fn right_word_bound(mut self) -> Pidgin {
        self.right = Some(Boundary::Word);
        self
    }
    pub fn line_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::Line);
        self.right = Some(Boundary::Line);
        self
    }
    pub fn left_line_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::Line);
        self
    }
    pub fn right_line_bound(mut self) -> Pidgin {
        self.right = Some(Boundary::Line);
        self
    }
    pub fn string_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::String);
        self.right = Some(Boundary::String);
        self
    }
    fn add_boundary_symbols(&self, phrase: &str) -> Vec<Expression> {
        let lb = if let Some(b) = &self.left {
            match b {
                Boundary::Word => {
                    if phrase.len() > 0 {
                        let c = phrase.chars().next().unwrap();
                        if c.is_alphanumeric() || c == '_' {
                            Some(Expression::Part(String::from(r"\b"), false))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                Boundary::Line => Some(Expression::Part(String::from("(?m)^"), false)),
                Boundary::String => Some(Expression::Part(String::from(r"\A"), false)),
            }
        } else {
            None
        };
        let rb = if let Some(b) = &self.right {
            match b {
                Boundary::Word => {
                    if phrase.len() > 0 {
                        let c = phrase.chars().last().unwrap();
                        if c.is_alphanumeric() || c == '_' {
                            Some(Expression::Part(String::from(r"\b"), false))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                Boundary::Line => Some(Expression::Part(String::from("$"), false)),
                Boundary::String => Some(Expression::Part(String::from(r"\z"), false)),
            }
        } else {
            None
        };
        let mut v = Vec::with_capacity(3);
        if let Some(e) = lb {
            v.push(e);
        }
        v.push(Expression::Raw(phrase.to_string()));
        if let Some(e) = rb {
            v.push(e);
        }
        v
    }
    fn add_symbol(&mut self, s: Symbol, rx: &str) {
        if !self.symbols.contains_key(&s) {
            self.symbols.insert(s.clone(), Vec::new());
        }
        self.symbols.get_mut(&s).unwrap().push(rx.to_string());
    }
    pub fn compile(&mut self) -> String {
        let mut exp = String::new();
        let mut phrases = self.init();
        phrases.sort();
        phrases.dedup();
        for e in self.recursive_compile(phrases.as_mut()) {
            exp += &e.to_string();
        }
        self.phrases.clear();
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
                    let atomy = is_atomic(&s);
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
    fn find_character_classes(&self, mut phrases: Vec<Vec<Expression>>) -> Vec<Vec<Expression>> {
        let mut char_count = 0;
        for v in &phrases {
            if v.len() > 1 {
                break;
            }
            if let Expression::Char(_, _) = v[0] {
                char_count += 1;
            } else {
                break;
            }
        }
        if char_count < 2 {
            return phrases;
        }
        if char_count == phrases.len() {
            let e = Expression::Part(format!("[{}]", self.to_character_class(&phrases)), false);
            return vec![vec![e]];
        } else if char_count > 2 {
            let e = Expression::Part(
                format!("[{}]", self.to_character_class(&phrases[0..char_count])),
                false,
            );
            let mut v = vec![vec![e]];
            let l = phrases.len();
            v.append(&mut phrases[char_count..l].to_vec());
            return v;
        } else {
            return phrases;
        }
    }
    fn char_ranges(&self, chars: Vec<char>) -> Vec<CharRange> {
        let mut v: Vec<CharRange> = Vec::with_capacity(chars.len());
        let mut c1i = chars[0] as u8;
        let mut li = c1i;
        let l = chars.len();
        for c2 in chars[1..l].iter() {
            let c2i = *c2 as u8;
            if c2i - 1 == li {
                li = c2i;
            } else {
                if c1i + 1 < li {
                    v.push(CharRange::CC(c1i as char, li as char));
                } else if c1i == li {
                    v.push(CharRange::C(c1i as char));
                } else {
                    v.push(CharRange::C(c1i as char));
                    v.push(CharRange::C(li as char));
                }
                c1i = c2i;
                li = c2i;
            }
        }
        if c1i + 1 < li {
            v.push(CharRange::CC(c1i as char, li as char));
        } else if c1i == li {
            v.push(CharRange::C(c1i as char));
        } else {
            v.push(CharRange::C(c1i as char));
            v.push(CharRange::C(li as char));
        }
        v
    }
    fn to_character_class(&self, phrases: &[Vec<Expression>]) -> String {
        let cv: Vec<char> = phrases
            .iter()
            .map(|v| match v[0] {
                Expression::Char(c, _) => c,
                _ => panic!("we should never get here"),
            }).collect();
        self.char_ranges(cv)
            .iter()
            .map(|cr| match cr {
                CharRange::C(c) => self.character_class_escape(*c),
                CharRange::CC(c1, c2) => format!(
                    "{}-{}",
                    self.character_class_escape(*c1),
                    self.character_class_escape(*c2)
                ),
            }).collect::<Vec<String>>()
            .join("")
    }
    fn character_class_escape(&self, c: char) -> String {
        match c {
            '\\' | '-' | '[' | '^' | ']' | '&' | '~' => format!("\\{}", c),
            _ => format!("{}", c),
        }
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
        rv.sort_by(vec_sort);
        rv = self.find_character_classes(rv);
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
    fn digest(&self, s: &str, symbols: &BTreeMap<Symbol, String>) -> Vec<Expression> {
        let mut rv = self.add_boundary_symbols(s);
        // apply the symbols to the strings
        for (sym, replacement) in symbols.iter() {
            let mut nv = Vec::new();
            for e in rv {
                if let Expression::Raw(s) = e {
                    match sym {
                        Symbol::S(name, _) => {
                            if s.contains(name) {
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
                        Symbol::Rx(rx, _) => {
                            if rx.is_match(s.as_str()) {
                                let mut offset = 0;
                                for m in rx.find_iter(&s) {
                                    if m.start() > offset {
                                        nv.push(Expression::Raw(s[offset..m.start()].to_string()));
                                    }
                                    nv.push(Expression::Part(replacement.clone(), false));
                                    offset = m.end();
                                }
                                if offset < s.len() {
                                    nv.push(Expression::Raw(s[offset..s.len()].to_string()));
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
    fn init(&self) -> Vec<Vec<Expression>> {
        let mut symbols: BTreeMap<Symbol, String> = BTreeMap::new();
        for (sym, v) in self.symbols.iter() {
            let e = if v.len() == 1 {
                v[0].clone()
            } else {
                Expression::Alternation(
                    v.iter()
                        .map(|s| Expression::Part(s.clone(), false))
                        .collect(),
                    false,
                ).to_string()
            };
            let e = match sym {
                Symbol::S(name, b) => {
                    if *b {
                        format!("(?P<{}>{})", name, e)
                    } else if is_atomic(&e) {
                        e
                    } else {
                        format!("(?:{})", e)
                    }
                }
                Symbol::Rx(rx, o) => {
                    if let Some(name) = o {
                        format!("(?P<{}>{})", name, e)
                    } else if is_atomic(&e) {
                        e
                    } else {
                        format!("(?:{})", e)
                    }
                }
            };
            symbols.insert(sym.clone(), e);
        }
        self.phrases
            .iter()
            .map(|s| self.digest(s, &symbols))
            .collect()
    }
}
// sort simple stuff first
// this makes it easier to find character ranges, and has the side effect of
// putting the stuff easier to match earlier in alternations
fn vec_sort(a: &Vec<Expression>, b: &Vec<Expression>) -> Ordering {
    let o = a.len().cmp(&b.len());
    if o != Ordering::Equal {
        return o;
    }
    for i in 0..a.len() {
        let o = a[i].cmp(&b[i]);
        if o != Ordering::Equal {
            return o;
        }
    }
    Ordering::Equal
}

enum CharRange {
    C(char),
    CC(char, char),
}

#[derive(Clone)]
enum Symbol {
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
            Expression::Char(c, _) => escape(&c.to_string()),
            Expression::Alternation(v, _) => {
                if v.len() == 1 {
                    v[0].to_string()
                } else {
                    let mut s = String::from("(?:");
                    for (i, e) in v.iter().enumerate() {
                        if i > 0 {
                            s.push('|');
                        }
                        s += &e.to_string();
                    }
                    s.push(')');
                    s
                }
            }
            Expression::Part(s, _) => s.clone(),
            Expression::Raw(s) => s.clone(),
        };
        if self.is_optional() {
            if is_atomic(&s) {
                s + "?"
            } else {
                String::from("(:?") + &s + ")?"
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

// boundary patterns to attach to words
#[derive(Clone)]
enum Boundary {
    String,
    Line,
    Word,
}
