extern crate regex;
use regex::{Captures, Regex};
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use util::{Flags,Boundary,Symbol,Expression,CharRange,is_atomic,character_class_escape};

#[derive(Clone, Debug)]
pub struct Pidgin {
    flags: Flags,
    left: Option<Boundary>,
    right: Option<Boundary>,
    symbols: BTreeMap<Symbol, Vec<Expression>>,
    phrases: Vec<String>,
}

impl Pidgin {
    pub fn new() -> Pidgin {
        Pidgin {
            flags: Flags::defaults(),
            left: None,
            right: None,
            symbols: BTreeMap::new(),
            phrases: Vec::new(),
        }
    }
    pub fn grammar(words: &[&str]) -> Grammar {
        let mut p = Pidgin::new();
        p.add(words);
        p.compile()
    }
    pub fn rx(words: &[&str]) -> String {
        Pidgin::grammar(words).to_string()
    }
    pub fn add(&mut self, phrases: &[&str]) -> &mut Pidgin {
        for w in phrases {
            self.phrases.push(w.to_string());
        }
        self
    }
    pub fn add_str(&mut self, s: &str) -> &mut Pidgin {
        self.phrases.push(s.to_string());
        self
    }
    pub fn clear(&mut self) {
        self.symbols.clear();
        self.phrases.clear();
    }
    pub fn rule(&mut self, name: &str, g: &Grammar) {
        let mut g = g.clone();
        g.name = Some(name.to_string());
        self.add_symbol(Symbol::S(name.to_string()), Expression::Grammar(g, false));
    }
    pub fn foreign_rule(&mut self, name: &str, pattern: &str) -> Result<(), regex::Error> {
        match Regex::new(pattern) {
            Err(e) => Err(e),
            Ok(_) => {
                let mut flags = Flags::defaults();
                flags.enclosed = !is_atomic(pattern);
                self.add_symbol(
                    Symbol::S(name.to_string()),
                    Expression::Grammar(
                        Grammar {
                            flags,
                            name: Some(name.to_string()),
                            sequence: vec![Expression::Part(pattern.to_string(), false)],
                        },
                        false,
                    ),
                );
                Ok(())
            }
        }
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
                let sequence = vec![Expression::Part(pattern.to_string(), false)];
                let mut flags = Flags::defaults();
                flags.enclosed = !is_atomic(pattern);
                let g = Grammar {
                    name,
                    sequence,
                    flags,
                };
                self.add_symbol(Symbol::Rx(rx), Expression::Grammar(g, false));
                Ok(self)
            }
            Err(e) => Err(e),
        }
    }
    pub fn case_insensitive(mut self, case: bool) -> Pidgin {
        self.flags.case_insensitive = case;
        self
    }
    pub fn multi_line(mut self, case: bool) -> Pidgin {
        self.flags.multi_line = case;
        self
    }
    pub fn dot_all(mut self, case: bool) -> Pidgin {
        self.flags.dot_all = case;
        self
    }
    pub fn unicode(mut self, case: bool) -> Pidgin {
        self.flags.unicode = case;
        self
    }
    pub fn enclosed(mut self, case: bool) -> Pidgin {
        self.flags.enclosed = case;
        self
    }
    pub fn normalize_whitespace(mut self) -> Pidgin {
        self.rx_rule(r"\s+", r"\s+", None).unwrap();
        self
    }
    pub fn unbound(mut self) -> Pidgin {
        self.left = None;
        self.right = None;
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
        self.flags.multi_line = true;
        self
    }
    pub fn left_line_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::Line);
        self.flags.multi_line = true;
        self
    }
    pub fn right_line_bound(mut self) -> Pidgin {
        self.right = Some(Boundary::Line);
        self.flags.multi_line = true;
        self
    }
    pub fn string_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::String);
        self.right = Some(Boundary::String);
        self
    }
    pub fn left_string_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::String);
        self
    }
    pub fn right_string_bound(mut self) -> Pidgin {
        self.right = Some(Boundary::String);
        self
    }
    fn add_boundary_symbols(&self, phrase: &str) -> Vec<Expression> {
        lazy_static! {
            static ref UNICODE_B: Regex = Regex::new(r"\w").unwrap();
            static ref ASCII_B: Regex = Regex::new(r"(?-U)\w").unwrap();
        }
        let lb = if let Some(b) = &self.left {
            match b {
                Boundary::Word => {
                    if phrase.len() > 0 {
                        let c = phrase[0..1].to_string();
                        let is_boundary = self.flags.unicode && UNICODE_B.is_match(&c)
                            || !self.flags.unicode && ASCII_B.is_match(&c);
                        if is_boundary {
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
                        let c = phrase.chars().last().unwrap().to_string();
                        let is_boundary = self.flags.unicode && UNICODE_B.is_match(&c)
                            || !self.flags.unicode && ASCII_B.is_match(&c);
                        if is_boundary {
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
    fn add_symbol(&mut self, s: Symbol, e: Expression) {
        if !self.symbols.contains_key(&s) {
            self.symbols.insert(s.clone(), Vec::new());
        }
        self.symbols.get_mut(&s).unwrap().push(e);
    }
    pub fn compile(&mut self) -> Grammar {
        let mut phrases = self.init();
        phrases.sort();
        phrases.dedup();
        let sequence = self.recursive_compile(phrases.as_mut());
        self.phrases.clear();
        Grammar {
            sequence,
            name: None,
            flags: self.flags.clone(),
        }
    }
	pub fn compile_non_capturing(&self) -> Grammar {
		let g = self.clone().compile().clear_recursive();
        let sequence = self.recursive_condense_sequence(&g.sequence);
        Grammar{ sequence, name: None, flags: g.flags.clone() }
	}
    fn recursive_condense_sequence(&self, v: &Vec<Expression>) -> Vec<Expression> {
        self.condense(v.iter().map(|e| self.recursive_condense_expression(e)).collect())
    }
    fn recursive_condense_expression(&self, e: &Expression) -> Expression {
        match e {
            Expression::Grammar(g, b) => {
                let sequence = g.sequence.iter().map(|e| self.recursive_condense_expression(e)).collect();
                let g = Grammar{sequence, name: g.name.clone(), flags: g.flags.clone()};
                Expression::Grammar(g, *b)
            },
            Expression::Alternation(v, b) => {
                let v = v.iter().map(|e| self.recursive_condense_expression(e)).collect();
                Expression::Alternation(v, *b)
            },
            Expression::Sequence(v, b) => {
                let v = v.iter().map(|e| self.recursive_condense_expression(e)).collect();
                Expression::Sequence(v, *b)
            },
            Expression::Repetition(x, n, b) => {
                Expression::Repetition(Box::new(self.recursive_condense_expression(x)), *n, *b)
            },
            _ => e.clone(),
        }
    }
    // initialize
    fn digest(&self, s: &str, symbols: &BTreeMap<Symbol, Expression>) -> Vec<Expression> {
        let mut rv = self.add_boundary_symbols(s);
        // apply the symbols to the strings
        for (sym, replacement) in symbols.iter() {
            let mut nv = Vec::new();
            for e in rv {
                if let Expression::Raw(s) = e {
                    match sym {
                        Symbol::S(name) => {
                            if s.contains(name) {
                                for (i, s) in s.split(name).enumerate() {
                                    if i > 0 {
                                        nv.push(replacement.clone());
                                    }
                                    if s.len() > 0 {
                                        nv.push(Expression::Raw(s.to_string()))
                                    }
                                }
                            } else {
                                nv.push(Expression::Raw(s));
                            }
                        }
                        Symbol::Rx(rx) => {
                            if rx.is_match(s.as_str()) {
                                let mut offset = 0;
                                for m in rx.find_iter(&s) {
                                    if m.start() > offset {
                                        nv.push(Expression::Raw(s[offset..m.start()].to_string()));
                                    }
                                    nv.push(replacement.clone());
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
        let mut symbols: BTreeMap<Symbol, Expression> = BTreeMap::new();
        for (sym, v) in self.symbols.iter() {
            let mut v = if v.len() > 1 {
                // dedup vector
                let mut v2 = Vec::with_capacity(v.len());
                let mut set = BTreeSet::new();
                for s in v {
                    if !set.contains(s) {
                        v2.push(s.clone());
                        set.insert(s);
                    }
                }
                v2
            } else {
                v.clone()
            };
            let e = if v.len() == 1 {
                v[0].clone()
            } else {
                let name = if let Expression::Grammar(ref mut g, _) = v[0] {
                    g.name.clone()
                } else {
                    panic!("we should only have grammars at this point")
                };
                if name.is_some() {
                    for g in &mut v {
                        if let Expression::Grammar(g, _) = g {
                            g.clear_name();
                        }
                    }
                }
                Expression::Grammar(
                    Grammar {
                        name,
                        flags: self.flags.clone(),
                        sequence: vec![Expression::Alternation(v, false)],
                    },
                    false,
                )
            };
            symbols.insert(sym.clone(), e);
        }
        self.phrases
            .iter()
            .map(|s| self.digest(s, &symbols))
            .collect()
    }
    fn condense(&self, mut phrase: Vec<Expression>) -> Vec<Expression> {
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
                        if phrase[i + k] != phrase[j + k] || phrase[i+k].has_names() {
                            break 'outer;
                        }
                    }
                    match_length += 1;
                    j += rep_length;
                }
                if match_length > 1 {
                    let s = phrase[i..i + rep_length]
                        .iter()
                        .map(|e| e.to_s(&self.flags))
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
                        if rep_length == 1 {
                            v.push(Expression::Repetition(
                                Box::new(phrase[i].clone()),
                                match_length,
                                false,
                            ));
                        } else {
                            let sequence = phrase[i..i + rep_length]
                                .iter()
                                .map(Expression::clone)
                                .collect::<Vec<_>>();
                            let sequence = Expression::Sequence(sequence, false);
                            v.push(Expression::Repetition(
                                Box::new(sequence),
                                match_length,
                                false,
                            ));
                        }
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
            return self.condense(phrases[0].clone());
        }
        let (prefix, suffix) = self.common_adfixes(phrases);
        let mut prefix = self.condense(prefix);
        let mut suffix = self.condense(suffix);
        phrases.sort();
        let mut map: BTreeMap<&Expression, Vec<&Vec<Expression>>> = BTreeMap::new();
        let mut optional = false;
        for phrase in phrases {
            if phrase.len() == 0 {
                optional = true;
            } else {
                map.entry(&phrase[0])
                    .or_insert_with(|| Vec::new())
                    .push(phrase);
            }
        }
        let mut rv = Vec::new();
        for (_, ref mut v) in map.iter_mut() {
            let mut v = v.iter().map(|v| (*v).clone()).collect();
            rv.push(self.recursive_compile(&mut v));
        }
        rv.sort_by(Pidgin::vec_sort);
        rv = self.find_character_classes(rv);
        // should pull out character classes at this point
        let alternates: Vec<Expression> = rv
            .iter()
            .map(|v| Expression::Sequence(v.clone(), false))
            .collect();
        prefix.push(Expression::Alternation(alternates, optional));
        prefix.append(&mut suffix);
        prefix
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

    fn common_adfixes(&self, phrases: &mut Vec<Vec<Expression>>) -> (Vec<Expression>, Vec<Expression>) {
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
                CharRange::C(c) => character_class_escape(*c),
                CharRange::CC(c1, c2) => format!(
                    "{}-{}",
                    character_class_escape(*c1),
                    character_class_escape(*c2)
                ),
            }).collect::<Vec<String>>()
            .join("")
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
}

#[derive(Clone, Debug)]
pub struct Grammar {
    pub(crate) name: Option<String>,
    pub(crate) sequence: Vec<Expression>,
    flags: Flags,
}

impl Grammar {
    fn clear_name(&mut self) {
        self.name = None;
    }
	pub fn clear_recursive(&self) -> Grammar {
		let sequence = self.sequence.iter().map(|e| e.clear_names()).collect();
		Grammar{ sequence, name: None, flags: self.flags.clone()}
	}
    fn needs_closure(&self, context: &Flags) -> bool {
        self.flags.enclosed || self.needs_flags_set(context)
    }
    fn needs_flags_set(&self, context: &Flags) -> bool {
        self.flags.case_insensitive ^ context.case_insensitive
            || self.flags.dot_all ^ context.dot_all
            || self.flags.multi_line ^ context.multi_line
            || self.flags.unicode ^ context.unicode
    }
    // flag string when needed
    fn flags(&self, context: &Flags) -> String {
        if !self.needs_flags_set(context) {
            return String::from("");
        }
        let mut flags_on = Vec::with_capacity(4);
        let mut flags_off = Vec::with_capacity(4);
        if self.flags.case_insensitive ^ context.case_insensitive {
            if self.flags.case_insensitive {
                flags_on.push("i");
            } else if self.flags.enclosed {
                flags_off.push("i");
            }
        }
        if self.flags.multi_line ^ context.multi_line {
            if self.flags.multi_line {
                flags_on.push("m");
            } else if self.flags.enclosed {
                flags_off.push("m");
            }
        }
        if self.flags.dot_all ^ context.dot_all {
            if self.flags.dot_all {
                flags_on.push("s");
            } else if self.flags.enclosed {
                flags_off.push("s");
            }
        }
        if self.flags.unicode ^ context.unicode {
            if !self.flags.unicode {
                flags_off.push("u");
            } else if self.flags.enclosed {
                flags_on.push("u");
            }
        }
        let mut flags = String::new();
        if flags_on.len() > 0 {
            flags.push_str(&flags_on.join(""));
        }
        if flags_off.len() > 0 {
            flags.push('-');
            flags.push_str(&flags_off.join(""));
        }
        flags
    }
    pub fn matcher(&self) -> Result<Matcher, regex::Error> {
        Matcher::new(&self)
    }
    // mostly for debugging -- this is likely not to compile due to repeated names
    pub(crate) fn to_s(&self, context: &Flags) -> String {
        let mut s = if self.name.is_some() {
            format!("(?P<{}>", self.name.as_ref().unwrap())
        } else {
            String::new()
        };
        if self.needs_closure(context) {
            s = s + format!("(?{}:", self.flags(context)).as_str();
        }
        for e in &self.sequence {
            s += e.to_s(&self.flags).as_ref();
        }
        if self.needs_closure(context) {
            s = s + ")"
        }
        if self.name.is_some() {
            s = s + ")"
        }
        s
    }
    pub fn to_string(&self) -> String {
        self.to_s(&Flags::defaults())
    }
}

impl PartialOrd for Grammar {
    fn partial_cmp(&self, other: &Grammar) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Grammar {
    fn cmp(&self, other: &Grammar) -> Ordering {
        let o = self.sequence.len().cmp(&other.sequence.len());
        if o != Ordering::Equal {
            return o;
        }
        if self.name.is_some() ^ other.name.is_some() {
            return if self.name.is_some() {
                Ordering::Greater
            } else {
                Ordering::Less
            };
        }
        if self.name.is_some() {
            let o = self
                .name
                .as_ref()
                .unwrap()
                .cmp(&other.name.as_ref().unwrap());
            if o != Ordering::Equal {
                return o;
            }
        }
        for i in 0..self.sequence.len() {
            let v1 = &self.sequence[i];
            let v2 = &other.sequence[i];
            let o = v1.cmp(&v2);
            if o != Ordering::Equal {
                return o;
            }
        }
        self.flags.cmp(&other.flags)
    }
}

impl PartialEq for Grammar {
    fn eq(&self, other: &Grammar) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Grammar {}

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
    fn new(g: &Grammar) -> Result<Matcher, regex::Error> {
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
