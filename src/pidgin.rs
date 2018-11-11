extern crate regex;
use crate::grammar::Grammar;
use crate::matching::Matcher;
use crate::util::{
    character_class_escape, is_atomic, Boundary, CharRange, Expression, Flags, Symbol,
};
use regex::{Error, Regex};
use std::cmp::{Ord, Ordering};
use std::collections::{BTreeMap, BTreeSet};

/// This is a grammar builder. It keeps track of the rules defined, the
/// alternates participating in the rule currently being defined, whether these
/// alternates should be bounded left and right by word boundaries, string
/// boundaries, or line boundaries, and the set of regex flags -- case-sensitivity,
/// for instance, that will govern the rule it produces.
///
/// Defined rules will be used to process the new rule's alternates. If there is
/// a "foo" rule, the alternate `"foo foo"` will be understood to require that
/// this "foo" rule match twice with a space between matches.
///
/// Because rule names can overlap, they are applied longest to shortest. If there
/// is both a "foo" rule and a "f" rule, "f foo" will be understood to involve
/// one match for each -- the "f" rule only gets the single "f".
///
/// In addition to rules identified like this by a name, there are also regex
/// rules. These are substituted into alternates wherever their definitional
/// pattern matches. Regex rules are sought in alternates only in what is left
/// over after ordinary rules are found. Regex rules are applied in inverse
/// order by the length of their string representation and then in alphabetical
/// order. They may optionally also have names.
///
/// `Pidgin` has numerous configuration methods which consume and return their
/// invocant.

#[derive(Clone, Debug)]
pub(crate) struct Pidgin {
    flags: Flags,
    left: Option<Boundary>,
    right: Option<Boundary>,
    symbols: BTreeMap<Symbol, Vec<Expression>>,
    phrases: Vec<String>,
}

impl Pidgin {
    /// Constructs a new `Pidgin` with the default state: no rules, no alternates
    /// for the current rule, case-sensitive, not multiline, not dot-all (`.`
    /// matches a newline), unicode-compliant, and not enclosed.
    pub(crate) fn new() -> Pidgin {
        Pidgin {
            flags: Flags::defaults(),
            left: None,
            right: None,
            symbols: BTreeMap::new(),
            phrases: Vec::new(),
        }
    }
    /// Adds the given list of alternates to the rule currently under construction.
    ///
    /// This method is chainable.
    pub(crate) fn add(&mut self, phrases: &[&str]) -> &mut Pidgin {
        for w in phrases {
            self.phrases.push(w.to_string());
        }
        self
    }
    /// Adds the given alternate to the rule currently under construction.
    ///
    /// This method is chainable.
    pub(crate) fn add_str(&mut self, s: &str) -> &mut Pidgin {
        self.phrases.push(s.to_string());
        self
    }
    /// Compiles the current rule, clearing the alternate list in preparation
    /// for constructing the next rule.
    pub(crate) fn compile(&mut self) -> Grammar {
        self.compile_bounded(true, true, true)
    }
    pub(crate) fn compile_bounded(&mut self, left: bool, right: bool, apply_symbols: bool) -> Grammar {
        let mut phrases = self.init(left, right, apply_symbols);
        phrases.sort();
        phrases.dedup();
        let sequence = self.recursive_compile(phrases.as_mut());
        self.phrases.clear();
        Grammar {
            sequence,
            name: None,
            flags: self.flags.clone(),
            lower_limit: None,
            upper_limit: None,
            stingy: false,
        }
    }
    /// A convenience method equivalent to `add(&words).compile()`.
    pub(crate) fn grammar(&mut self, words: &[&str]) -> Grammar {
        self.add(words);
        self.compile()
    }
    pub(crate) fn rule(&mut self, name: &str, g: &Grammar) {
        let mut g = g.clone();
        g.name = Some(name.to_string());
        self.add_symbol(Symbol::S(name.to_string()), Expression::Grammar(g, false));
    }
    pub(crate) fn foreign_rule(&mut self, name: &str, pattern: &str) -> Result<(), Error> {
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
                            lower_limit: None,
                            upper_limit: None,
                            stingy: false,
                        },
                        false,
                    ),
                );
                Ok(())
            }
        }
    }
    pub(crate) fn foreign_rx_rule(
        &mut self,
        rx: &str,
        pattern: &str,
        name: Option<&str>,
    ) -> Result<(), Error> {
        match Regex::new(rx) {
            Ok(rx) => match Regex::new(pattern) {
                Err(e) => Err(e),
                Ok(_) => {
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
                        lower_limit: None,
                        upper_limit: None,
                        stingy: false,
                    };
                    self.add_symbol(Symbol::Rx(rx), Expression::Grammar(g, false));
                    Ok(())
                }
            },
            Err(e) => Err(e),
        }
    }
    pub(crate) fn build_rule(&mut self, name: &str, components: Vec<RuleFragment>) {
        let right_limit = components.len() - 1;
        let g = Grammar {
            name: Some(name.to_string()),
            flags: self.flags.clone(),
            lower_limit: None,
            upper_limit: None,
            stingy: false,
            sequence: components
                .iter()
                .enumerate()
                .map(|(i, f)| match f {
                    RuleFragment::G(g) => Expression::Grammar(g.clone(), false),
                    RuleFragment::S(s) => {
                        self.phrases.clear();
                        self.phrases.push(s.clone());
                        Expression::Grammar(
                            self.compile_bounded(i == 0, i == right_limit, false),
                            false,
                        )
                    }
                })
                .collect(),
        };
        self.add_symbol(Symbol::S(name.to_string()), Expression::Grammar(g, false));
    }
    pub(crate) fn remove_rule(&mut self, name: &str) {
        self.symbols.remove(&Symbol::S(name.to_string()));
    }
    pub(crate) fn remove_rx_rule(&mut self, name: &str) -> Result<(), Error> {
        match Regex::new(name) {
            Err(e) => Err(e),
            Ok(rx) => {
                self.symbols.remove(&Symbol::Rx(rx));
                Ok(())
            }
        }
    }
    pub(crate) fn clear(&mut self) {
        self.symbols.clear();
        self.phrases.clear();
    }
    pub(crate) fn case_insensitive(mut self, case: bool) -> Pidgin {
        self.flags.case_insensitive = case;
        self
    }
    pub(crate) fn multi_line(mut self, case: bool) -> Pidgin {
        self.flags.multi_line = case;
        self
    }
    pub(crate) fn dot_all(mut self, case: bool) -> Pidgin {
        self.flags.dot_all = case;
        self
    }
    pub(crate) fn unicode(mut self, case: bool) -> Pidgin {
        self.flags.unicode = case;
        self
    }
    pub(crate) fn enclosed(mut self, case: bool) -> Pidgin {
        self.flags.enclosed = case;
        self
    }
    pub(crate) fn reverse_greed(mut self, case: bool) -> Pidgin {
        self.flags.reverse_greed = case;
        self
    }
    pub(crate) fn normalize_whitespace(mut self, required: bool) -> Pidgin {
        self.remove_rx_rule(r"\s+").unwrap();
        if required {
            self.foreign_rx_rule(r"\s+", r"\s+", None).unwrap();
        } else {
            self.foreign_rx_rule(r"\s+", r"\s*", None).unwrap();
        }
        self
    }
    pub(crate) fn word_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::Word);
        self.right = Some(Boundary::Word);
        self
    }
    pub(crate) fn left_word_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::Word);
        self
    }
    pub(crate) fn right_word_bound(mut self) -> Pidgin {
        self.right = Some(Boundary::Word);
        self
    }
    pub(crate) fn line_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::Line);
        self.right = Some(Boundary::Line);
        self.flags.multi_line = true;
        self
    }
    pub(crate) fn left_line_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::Line);
        self.flags.multi_line = true;
        self
    }
    pub(crate) fn right_line_bound(mut self) -> Pidgin {
        self.right = Some(Boundary::Line);
        self.flags.multi_line = true;
        self
    }
    pub(crate) fn string_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::String);
        self.right = Some(Boundary::String);
        self
    }
    pub(crate) fn left_string_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::String);
        self
    }
    pub(crate) fn right_string_bound(mut self) -> Pidgin {
        self.right = Some(Boundary::String);
        self
    }
    pub(crate) fn unbound(mut self) -> Pidgin {
        self.left = None;
        self.right = None;
        self
    }
    pub(crate) fn compile_non_capturing(&self) -> Grammar {
        let g = self.clone().compile().clear_recursive();
        let sequence = self.recursive_condense_sequence(&g.sequence);
        Grammar {
            sequence,
            name: None,
            flags: g.flags.clone(),
            lower_limit: None,
            upper_limit: None,
            stingy: false,
        }
    }
    pub(crate) fn rx(words: &[&str]) -> String {
        Pidgin::new().grammar(words).to_string()
    }
    pub(crate) fn matcher(&self) -> Result<Matcher, Error> {
        self.clone().compile().matcher()
    }
    fn add_boundary_symbols(&self, left: bool, right: bool, phrase: &str) -> Vec<Expression> {
        lazy_static! {
            static ref UNICODE_B: Regex = Regex::new(r"\w").unwrap();
            static ref ASCII_B: Regex = Regex::new(r"(?-U)\w").unwrap();
        }
        let lb = if left {
            if let Some(b) = &self.left {
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
            }
        } else {
            None
        };
        let rb = if right {
            if let Some(b) = &self.right {
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
    fn recursive_condense_sequence(&self, v: &Vec<Expression>) -> Vec<Expression> {
        self.condense(
            v.iter()
                .map(|e| self.recursive_condense_expression(e))
                .collect(),
        )
    }
    fn recursive_condense_expression(&self, e: &Expression) -> Expression {
        match e {
            Expression::Grammar(g, b) => {
                let g = Grammar {
                    sequence: self.recursive_condense_sequence(&g.sequence),
                    name: g.name.clone(),
                    flags: g.flags.clone(),
                    lower_limit: None,
                    upper_limit: None,
                    stingy: false,
                };
                Expression::Grammar(g, *b)
            }
            Expression::Alternation(v, b) => {
                Expression::Alternation(self.recursive_condense_sequence(&v), *b)
            }
            Expression::Sequence(v, b) => {
                Expression::Sequence(self.recursive_condense_sequence(&v), *b)
            }
            Expression::Repetition(x, n, b) => {
                Expression::Repetition(Box::new(self.recursive_condense_expression(x)), *n, *b)
            }
            _ => e.clone(),
        }
    }
    // initialize
    fn digest(
        &self,
        left: bool,
        right: bool,
        apply_symbols: bool,
        s: &str,
        symbols: &BTreeMap<Symbol, Expression>,
    ) -> Vec<Expression> {
        let mut rv = vec![Expression::Raw(s.to_string())];
        if apply_symbols {
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
                                            nv.push(Expression::Raw(
                                                s[offset..m.start()].to_string(),
                                            ));
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
        }
        if left && self.left.is_some() {
            let first = rv.remove(0);
            if let Expression::Raw(s) = first {
                let mut nv = self.add_boundary_symbols(true, false, &s);
                while nv.len() > 0 {
                    let e = nv.pop().unwrap();
                    rv.insert(0, e);
                }
            } else {
                rv.insert(0, first);
            }
        }
        if right && self.right.is_some() {
            let last = rv.pop().unwrap();
            if let Expression::Raw(s) = last {
                for e in self.add_boundary_symbols(false, true, &s) {
                    rv.push(e);
                }
            } else {
                rv.push(last);
            }
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
    fn init(&self, left: bool, right: bool, apply_symbols: bool) -> Vec<Vec<Expression>> {
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
                        lower_limit: None,
                        upper_limit: None,
                        stingy: false,
                    },
                    false,
                )
            };
            symbols.insert(sym.clone(), e);
        }
        self.phrases
            .iter()
            .map(|s| self.digest(left, right, apply_symbols, s, &symbols))
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
                        if phrase[i + k] != phrase[j + k] || phrase[i + k].has_names() {
                            break 'outer;
                        }
                    }
                    match_length += 1;
                    j += rep_length;
                }
                if match_length > 1 {
                    let s = phrase[i..i + rep_length]
                        .iter()
                        .map(|e| e.to_s(&self.flags, false, false))
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
        let mut aw = 0;
        let mut bw = 0;
        for e in a {
            aw += e.weight();
        }
        for e in b {
            bw += e.weight();
        }
        let o = aw.cmp(&bw);
        if o != Ordering::Equal {
            return o;
        }
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
                for v in phrases {
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
            })
            .collect();
        self.char_ranges(cv)
            .iter()
            .map(|cr| match cr {
                CharRange::C(c) => character_class_escape(*c),
                CharRange::CC(c1, c2) => format!(
                    "{}-{}",
                    character_class_escape(*c1),
                    character_class_escape(*c2)
                ),
            })
            .collect::<Vec<String>>()
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

/// A small enum needed by `Pidgin::build_rule`.
#[derive(Debug)]
pub(crate) enum RuleFragment {
    S(String),
    G(Grammar),
}

/// Constructs a `RuleFragment::S`.
///
/// The name is short for "string fragment". This is just a way to construct a
/// string `RuleFragment` in fewer keystrokes.
pub(crate) fn sf(string: &str) -> RuleFragment {
    RuleFragment::S(string.to_string())
}

/// Constructs a `RuleFragment::G`.
///
/// The name is short for "grammar fragment". This is just a way to construct a
/// grammar `RuleFragment` in fewer keystrokes.
pub(crate) fn gf(g: Grammar) -> RuleFragment {
    RuleFragment::G(g)
}

#[cfg(test)]
mod tests {
    use super::*;
    extern crate lazy_static;
    extern crate regex;
    use regex::Regex;

    fn all_equal(words: &[&str], pattern: &Grammar) {
        let rx = pattern.matcher().unwrap();
        for w in words {
            assert!(rx.is_match(w))
        }
    }

    #[test]
    fn simple_alternation() {
        let words = vec!["cat", "dog", "camel"];
        let mut p = Pidgin::new();
        p.add(&words);
        let pattern = p.compile();
        all_equal(&words, &pattern);
    }

    #[test]
    fn common_suffix() {
        let words = vec!["cats", "dogs"];
        let mut p = Pidgin::new();
        p.add(&words);
        let pattern = p.compile();
        assert!(pattern.to_string().as_str().ends_with("s"));
        all_equal(&words, &pattern);
    }

    #[test]
    fn common_prefix() {
        let words = vec!["scat", "spore"];
        let mut p = Pidgin::new();
        p.add(&words);
        let pattern = p.compile();
        assert!(pattern.to_string().as_str().starts_with("s"));
        all_equal(&words, &pattern);
    }

    #[test]
    fn both_prefix_and_suffix() {
        let words = vec!["scats", "spores"];
        let mut p = Pidgin::new();
        p.add(&words);
        let pattern = p.compile();
        let rx = pattern.to_string();
        assert!(rx.as_str().starts_with("s"));
        assert!(rx.as_str().ends_with("s"));
        all_equal(&words, &pattern);
    }

    #[test]
    fn short_character_class() {
        let words = vec!["a", "b"];
        let mut p = Pidgin::new();
        p.add(&words);
        let pattern = p.compile();
        assert_eq!("[ab]", &pattern.to_string());
        all_equal(&words, &pattern);
    }

    #[test]
    fn long_character_class() {
        let words = vec!["a", "b", "c"];
        let mut p = Pidgin::new();
        p.add(&words);
        let pattern = p.compile();
        assert_eq!("[a-c]", &pattern.to_string());
        all_equal(&words, &pattern);
    }

    #[test]
    fn complex_character_class() {
        let words = vec!["a", "b", "c", "g"];
        let mut p = Pidgin::new();
        p.add(&words);
        let pattern = p.compile();
        assert_eq!("[a-cg]", &pattern.to_string());
        all_equal(&words, &pattern);
    }

    #[test]
    fn character_class_in_alternation() {
        let words = vec!["Ant", "a", "b", "c", "g"];
        let mut p = Pidgin::new();
        p.add(&words);
        let pattern = p.compile();
        assert_eq!("(?:[a-cg]|Ant)", &pattern.to_string());
        all_equal(&words, &pattern);
    }

    #[test]
    fn small_repeat_ignored() {
        let words = vec!["aaa"];
        let mut p = Pidgin::new();
        p.add(&words);
        let pattern = p.compile();
        assert_eq!("aaa", &pattern.to_string());
        all_equal(&words, &pattern);
    }

    #[test]
    fn longer_repeat_found() {
        let words = vec!["aaaa"];
        let mut p = Pidgin::new();
        p.add(&words);
        let pattern = p.compile();
        assert_eq!("a{4}", &pattern.to_string());
        all_equal(&words, &pattern);
    }

    #[test]
    fn complex_repeat() {
        let words = vec!["aaaabbbbaaaabbbb"];
        let mut p = Pidgin::new();
        p.add(&words);
        let pattern = p.compile();
        assert_eq!("(?:a{4}b{4}){2}", &pattern.to_string());
        all_equal(&words, &pattern);
    }

    #[test]
    fn simple_string_symbol_capturing() {
        let words = vec!["foo"];
        let mut p = Pidgin::new();
        p.rule("foo", &Pidgin::new().grammar(&["bar"]));
        p.add(&words);
        let pattern = p.compile();
        assert_eq!("(?P<foo>bar)", pattern.to_string());
    }

    #[test]
    fn string_string_symbol_ordering() {
        let words = vec!["foo f"];
        let mut p = Pidgin::new();
        p.rule("foo", &Pidgin::new().grammar(&["bar"]));
        p.rule("f", &Pidgin::new().grammar(&["plugh"]));
        p.add(&words);
        let pattern = p.compile();
        assert_eq!("(?P<foo>bar) (?P<f>plugh)", pattern.to_string());
    }

    #[test]
    fn string_regex_symbol_ordering() {
        let words = vec!["foo f"];
        let mut p = Pidgin::new();
        p.foreign_rx_rule("foo", "bar", None).unwrap();
        p.rule("f", &Pidgin::new().grammar(&["plugh"]));
        p.add(&words);
        let pattern = p.compile();
        assert_eq!("(?P<f>plugh)oo (?P<f>plugh)", pattern.to_string());
    }

    #[test]
    fn regex_regex_symbol_ordering() {
        let words = vec!["foo f"];
        let mut p = Pidgin::new();
        p.foreign_rx_rule("foo", "bar", None).unwrap();
        p.foreign_rx_rule("f", "plugh", None).unwrap();
        p.add(&words);
        let pattern = p.compile();
        assert_eq!("(?:bar) (?:plugh)", pattern.to_string());
    }

    #[test]
    fn normalize_whitespace() {
        let words = vec!["foo bar", "baz   plugh"];
        let mut p = Pidgin::new().normalize_whitespace(true);
        p.add(&words);
        let pattern = p.compile().to_string();
        let rx = Regex::new(&pattern).unwrap();
        assert!(rx.is_match("foo    bar"));
        assert!(rx.is_match("baz plugh"));
    }

    #[test]
    fn word_boundaries() {
        let words = vec!["tardigrade", "onomatopoeia"];
        let mut p = Pidgin::new().word_bound();
        p.add(&words);
        let pattern = p.compile().to_string();
        let rx = Regex::new(&pattern).unwrap();
        for w in words {
            assert!(rx.is_match(w));
            let s = String::from("a") + w;
            assert!(!rx.is_match(&s));
            let s = w.to_string() + "a";
            assert!(!rx.is_match(&s));
            let s = String::from(" ") + w;
            assert!(rx.is_match(&s));
            let s = w.to_string() + " ";
            assert!(rx.is_match(&s));
        }
    }

    #[test]
    fn line_boundaries() {
        let words = vec!["tardigrade", "onomatopoeia"];
        let mut p = Pidgin::new().line_bound();
        p.add(&words);
        let pattern = p.compile().to_string();
        let rx = Regex::new(&pattern).unwrap();
        for w in words {
            assert!(rx.is_match(w), format!("{} matches '{}", pattern, w));
            let s = String::from(" ") + w;
            assert!(
                !rx.is_match(&s),
                format!("{} doesn't match '{}' with space before", pattern, w)
            );
            let s = w.to_string() + " ";
            assert!(
                !rx.is_match(&s),
                "{} doesn't match '{}' with space after",
                pattern,
                w
            );
            let s = String::from("\n") + w;
            assert!(
                rx.is_match(&s),
                format!("{} matches '{}' with newline before", pattern, w)
            );
            let s = w.to_string() + "\n";
            assert!(
                rx.is_match(&s),
                format!("{} matches '{}' with newline after", pattern, w)
            );
        }
    }

    #[test]
    fn string_boundaries() {
        let words = vec!["tardigrade", "onomatopoeia"];
        let mut p = Pidgin::new().string_bound();
        p.add(&words);
        let pattern = p.compile().to_string();
        let rx = Regex::new(&pattern).unwrap();
        for w in words {
            assert!(rx.is_match(w), format!("{} matches '{}", pattern, w));
            let s = String::from(" ") + w;
            assert!(
                !rx.is_match(&s),
                format!("{} doesn't match '{}' with space before", pattern, w)
            );
            let s = w.to_string() + " ";
            assert!(
                !rx.is_match(&s),
                "{} doesn't match '{}' with space after",
                pattern,
                w
            );
            let s = String::from("\n") + w;
            assert!(
                !rx.is_match(&s),
                format!("{} doesn't match '{}' with newline before", pattern, w)
            );
            let s = w.to_string() + "\n";
            assert!(
                !rx.is_match(&s),
                format!("{} doesn't match '{}' with newline after", pattern, w)
            );
        }
    }

    #[test]
    fn rule_ordering() {
        let mut p = Pidgin::new();
        p.foreign_rule("foo", "(?P<alpha>[a-zA-Z]+)").unwrap();
        p.foreign_rule("foo", r"(?P<numeric>\d+)").unwrap();
        p.foreign_rule("foo", r"(?P<alphanumeric>[\da-zA-Z]+)")
            .unwrap();
        p.add(&["foo"]);
        let pattern = p.compile().to_string();
        let rx = Regex::new(&pattern).unwrap();
        let cap = rx.captures("1234").unwrap();
        assert!(cap.name("foo").is_some(), format!("{} matched", rx));
        assert!(cap.name("numeric").is_some(), "right order");
        let cap = rx.captures("abc").unwrap();
        assert!(cap.name("foo").is_some(), "pattern matched");
        assert!(cap.name("alpha").is_some(), "right order");
        p.clear();
        p.foreign_rule("foo", r"(?P<alphanumeric>[\da-zA-Z]+)")
            .unwrap();
        p.foreign_rule("foo", "(?P<alpha>[a-zA-Z]+)").unwrap();
        p.foreign_rule("foo", r"(?P<numeric>\d+)").unwrap();
        p.add(&["foo"]);
        let pattern = p.compile().to_string();
        let rx = Regex::new(&pattern).unwrap();
        let cap = rx.captures("1234").unwrap();
        assert!(cap.name("foo").is_some(), "pattern matched");
        assert!(cap.name("numeric").is_none(), "right order");
        let cap = rx.captures("abc").unwrap();
        assert!(cap.name("foo").is_some(), "pattern matched");
        assert!(cap.name("alpha").is_none(), "right order");
    }

    #[test]
    fn case_sensitivity() {
        let words = vec!["cat", "dog"];
        let mut p = Pidgin::new().case_insensitive(true);
        p.add(&words);
        let pattern = p.compile();
        all_equal(&vec!["CAT", "DOG"], &pattern);
    }

    #[test]
    fn nested_capturing() {
        let mut p = Pidgin::new()
            .word_bound()
            .normalize_whitespace(true)
            .case_insensitive(true);
        let apples = p.add(&vec!["pippin", "northern spy", "crab"]).compile();
        let oranges = p.add(&vec!["blood", "navel", "valencia"]).compile();
        p.rule("apple", &apples);
        p.rule("orange", &oranges);
        let fruit = p.add(&vec!["apple", "orange"]).compile();
        let lettuces = p.add(&vec!["red", "green", "boston", "romaine"]).compile();
        let tomatoes = p
            .add(&vec!["cherry", "brandywine", "beefsteak", "roma"])
            .compile();
        p.rule("lettuce", &lettuces);
        p.rule("tomatoe", &tomatoes);
        let vegetables = p.add(&vec!["lettuce", "tomatoe"]).compile();
        p.rule("vegetable", &vegetables);
        p.rule("fruit", &fruit);
        let matcher = p
            .add(&vec!["fruit", "vegetable"])
            .compile()
            .matcher()
            .unwrap();
        assert!(matcher.is_match("cherry"));
        let captures = matcher.parse("cherry").unwrap();
        assert_eq!(captures.name("vegetable").unwrap().as_str(), "cherry");
        assert_eq!(captures.name("tomatoe").unwrap().as_str(), "cherry");
        assert!(matcher.is_match("Brandywine"));
        let captures = matcher.parse("  Northern  Spy  ").unwrap();
        assert_eq!(captures.name("fruit").unwrap().as_str(), "Northern  Spy");
        assert_eq!(captures.name("apple").unwrap().as_str(), "Northern  Spy");
        assert!(!matcher.is_match("tomatoes"));
    }

    #[test]
    fn reverse_greed() {
        let mut p = Pidgin::new().reverse_greed(true);
        let g = p.grammar(&vec!["bar"]);
        assert_eq!("(?U:bar)", g.to_string());
    }

    #[test]
    fn build_rule() {
        let mut p = Pidgin::new().word_bound().normalize_whitespace(true);
        let animal = p.grammar(&vec!["cat", "cow", "camel", "mongoose"]);
        p.rule("animal", &animal);
        let animal_space = p.add_str("animal ").compile();
        p.build_rule(
            "animal_proof",
            vec![gf(animal_space.reps_min(1).unwrap()), sf("QED")],
        );
        let m = p.add_str("animal_proof").matcher().unwrap();
        assert!(m.is_match("camel QED"));
        assert!(m.is_match("camel  camel   cat cow mongoose    QED"));
        assert!(!m.is_match("scamel QED"));
    }

    #[test]
    fn min_reps_0() {
        let mut p = Pidgin::new();
        let g = p.grammar(&vec!["foo", "bar", "baz"]);
        p.build_rule(
            "foo",
            vec![sf("xyzzy "), gf(g.reps_min(0).unwrap()), sf(" plugh")],
        );
        let g = p.grammar(&vec!["foo"]);
        assert!(format!("{}", g).contains("*"));
        let m = g.matcher().unwrap();
        assert!(m.is_match("xyzzy  plugh"));
        assert!(m.is_match("xyzzy foo plugh"));
        assert!(m.is_match("xyzzy foobar plugh"));
        assert!(m.is_match("xyzzy foobarbaz plugh"));
    }

    #[test]
    fn min_reps_1() {
        let mut p = Pidgin::new();
        let g = p.grammar(&vec!["foo", "bar", "baz"]);
        p.build_rule(
            "foo",
            vec![sf("xyzzy "), gf(g.reps_min(1).unwrap()), sf(" plugh")],
        );
        let g = p.grammar(&vec!["foo"]);
        assert!(format!("{}", g).contains("+"));
        let m = g.matcher().unwrap();
        assert!(!m.is_match("xyzzy  plugh"));
        assert!(m.is_match("xyzzy foo plugh"));
        assert!(m.is_match("xyzzy foobar plugh"));
        assert!(m.is_match("xyzzy foobarbaz plugh"));
    }

    #[test]
    fn min_reps_2() {
        let mut p = Pidgin::new();
        let g = p.grammar(&vec!["foo", "bar", "baz"]);
        p.build_rule(
            "foo",
            vec![sf("xyzzy "), gf(g.reps_min(2).unwrap()), sf(" plugh")],
        );
        let g = p.grammar(&vec!["foo"]);
        assert!(format!("{}", g).contains("{2,}"));
        let m = g.matcher().unwrap();
        assert!(!m.is_match("xyzzy  plugh"));
        assert!(!m.is_match("xyzzy foo plugh"));
        assert!(m.is_match("xyzzy foobar plugh"));
        assert!(m.is_match("xyzzy foobarbaz plugh"));
    }

    #[test]
    fn max_reps_1() {
        let mut p = Pidgin::new();
        let g = p.grammar(&vec!["foo", "bar", "baz"]);
        p.build_rule(
            "foo",
            vec![sf("xyzzy "), gf(g.reps_max(1).unwrap()), sf(" plugh")],
        );
        let g = p.grammar(&vec!["foo"]);
        assert!(format!("{}", g).contains("?"));
        let m = g.matcher().unwrap();
        assert!(m.is_match("xyzzy  plugh"));
        assert!(m.is_match("xyzzy foo plugh"));
        assert!(!m.is_match("xyzzy foobar plugh"));
        assert!(!m.is_match("xyzzy foobarbaz plugh"));
    }

    #[test]
    fn max_reps_2() {
        let mut p = Pidgin::new();
        let g = p.grammar(&vec!["foo", "bar", "baz"]);
        p.build_rule(
            "foo",
            vec![sf("xyzzy "), gf(g.reps_max(2).unwrap()), sf(" plugh")],
        );
        let g = p.grammar(&vec!["foo"]);
        assert!(format!("{}", g).contains("{0,2}"));
        let m = g.matcher().unwrap();
        assert!(m.is_match("xyzzy  plugh"));
        assert!(m.is_match("xyzzy foo plugh"));
        assert!(m.is_match("xyzzy foobar plugh"));
        assert!(!m.is_match("xyzzy foobarbaz plugh"));
    }

    #[test]
    fn build_rule_bound() {
        let mut p = Pidgin::new();
        let g = p.grammar(&vec!["foo", "bar", "baz"]);
        p = p.word_bound();
        p.build_rule("foo", vec![sf("xyzzy"), gf(g), sf("plugh")]);
        let g = p.grammar(&vec!["foo"]);
        print!("{}", g);
        let m = g.matcher().unwrap();
        assert!(m.is_match("xyzzyfooplugh"));
        assert!(!m.is_match("_xyzzyfooplugh"));
        assert!(!m.is_match("xyzzyfooplugh_"));
        assert!(!m.is_match("xyzzy foo plugh"));
    }

    #[test]
    fn boundaries_only_on_leaves() {
        let mut p = Pidgin::new().word_bound();
        let words = vec!["foo", "@bar", "baz"];
        let g = p.grammar(&words);
        p.rule("foo", &g);
        let m = p.grammar(&vec!["foo"]).matcher().unwrap();
        for w in words {
            assert!(m.is_match(w), w);
            let right_bound = w.to_string() + "b";
            assert!(!m.is_match(&right_bound), right_bound);
        }
        assert!(!m.is_match("bfoo"), "bfoo");
        assert!(!m.is_match("bbaz"), "bbaz");
        assert!(m.is_match("b@bar"), "b@bar");
        assert!(!m.is_match("camel QEDs"));
    }

    #[test]
    fn has_test() {
        let mut p = Pidgin::new();
        let g = p.grammar(&vec!["cat", "dog", "camel"]);
        p.rule("animal", &g);
        let g = p.grammar(&vec!["carpet", "crate", "cartoon"]);
        p.rule("thing", &g);
        let m = p.grammar(&vec!["animal", "thing"]).matcher().unwrap();
        assert!(m.parse("cat").unwrap().has("animal"));
    }

}
