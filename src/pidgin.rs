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
/// ```rust
/// # use pidgin::Pidgin;
/// let mut p = Pidgin::new()
///    .enclosed(true)
///    .word_bound()
///    .case_insensitive(true);
/// ```

#[derive(Clone, Debug)]
pub struct Pidgin {
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
    pub fn new() -> Pidgin {
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
    pub fn add(&mut self, phrases: &[&str]) -> &mut Pidgin {
        for w in phrases {
            self.phrases.push(w.to_string());
        }
        self
    }
    /// Adds the given alternate to the rule currently under construction.
    ///
    /// This method is chainable.
    pub fn add_str(&mut self, s: &str) -> &mut Pidgin {
        self.phrases.push(s.to_string());
        self
    }
    /// Compiles the current rule, clearing the alternate list in preparation
    /// for constructing the next rule.
    pub fn compile(&mut self) -> Grammar {
        self.compile_bounded(true, true, true)
    }
    pub fn compile_bounded(&mut self, left: bool, right: bool, apply_symbols: bool) -> Grammar {
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
        }
    }
    /// A convenience method equivalent to `add(&words).compile()`.
    pub fn grammar(&mut self, words: &[&str]) -> Grammar {
        self.add(words);
        self.compile()
    }
    /// Define the rule `name`.
    ///
    /// ***NOTE*** Multiple rules defined with the same name are treated as
    /// alternates. The order of their adding will define the order in which
    /// they are tried. See `remove_rule`.
    pub fn rule(&mut self, name: &str, g: &Grammar) {
        let mut g = g.clone();
        g.name = Some(name.to_string());
        self.add_symbol(Symbol::S(name.to_string()), Expression::Grammar(g, false));
    }
    /// Defines a rule replacing matched portion's of the
    /// rule's alternates with the given regex.
    ///
    /// The `rx` argument finds matched portions of an alternate. The `g`
    /// argument defines the rule. The `name` argument
    /// provides the optional name for the rule.
    ///
    /// For an example of its practical use, the `normalize_whitespace` method
    /// is implemented via `foreign_rx_rule`.
    ///
    /// ```rust
    /// # use pidgin::Pidgin;
    /// # use std::error::Error;
    /// # fn demo() -> Result<(),Box<Error>> {
    /// let mut p = Pidgin::new();
    /// let g = p.grammar(&vec!["foo", "bar"]);
    /// p.rx_rule(r"\s+", &g, Some("whitespace_is_weird"))?;
    /// let m = p.grammar(&vec!["FUNKY CHICKEN"]).matcher()?;
    /// let mtch = m.parse("FUNKYfooCHICKEN").unwrap();
    /// assert!(mtch.has("whitespace_is_weird"));
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// # Errors
    ///
    /// `foreign_rx_rule` returns an error if `rx` fails to compile.
    pub fn rx_rule(&mut self, rx: &str, g: &Grammar, name: Option<&str>) -> Result<(), Error> {
        match Regex::new(rx) {
            Err(e) => Err(e),
            Ok(rx) => {
                let mut g = g.clone();
                g.name = match name {
                    Some(name) => Some(name.to_string()),
                    None => None,
                };
                self.add_symbol(Symbol::Rx(rx), Expression::Grammar(g.clone(), false));
                Ok(())
            }
        }
    }
    /// Defines a rule based on an ad hoc regular expression.
    ///
    /// Currently `foreign_rule` is the only way to define a rule with unbounded
    /// repetition.
    ///
    /// ```rust
    /// # use pidgin::Pidgin;
    /// # use std::error::Error;
    /// # fn demo() -> Result<(),Box<Error>> {
    /// # let mut pidgin = Pidgin::new();
    /// pidgin.foreign_rule("us_local_phone", r"\b[0-9]{3}-?[0-9]{4}\b")?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// # Errors
    ///
    /// `foreign_rule` returns an error if the foreign regex fails to compile.
    pub fn foreign_rule(&mut self, name: &str, pattern: &str) -> Result<(), Error> {
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
                        },
                        false,
                    ),
                );
                Ok(())
            }
        }
    }
    /// Defines a rule, optionally named, replacing matched portion's of the
    /// rule's alternates with the given regex.
    ///
    /// The `rx` argument finds matched portions of an alternate. The `pattern`
    /// argument defines the regular expression of the rule. The `name` argument
    /// provides the optional name for the rule.
    ///
    /// ```rust
    /// # use pidgin::Pidgin;
    /// # use std::error::Error;
    /// # fn demo() -> Result<(),Box<Error>> {
    /// # let mut pidgin = Pidgin::new();
    /// pidgin.foreign_rx_rule(r"\s+", r"\t+", Some("whitespace_means_tabs"))?;
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// # Errors
    ///
    /// `foreign_rx_rule` returns an error if either `rx` or `pattern` fails to compile.
    pub fn foreign_rx_rule(
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
                    };
                    self.add_symbol(Symbol::Rx(rx), Expression::Grammar(g, false));
                    Ok(())
                }
            },
            Err(e) => Err(e),
        }
    }
    /// Defines a rule using a vector of `RuleFragment`s. This facilitates the
    /// insertion of rules with defined repetition limits.
    ///
    /// #Examples
    ///
    /// ```rust
    /// # use pidgin::{gf, sf, Pidgin};
    /// # use std::error::Error;
    /// # fn demo() -> Result<(),Box<Error>> {
    /// let mut p = Pidgin::new().word_bound().normalize_whitespace(true);
    /// let animal = p.grammar(&vec!["cat", "cow", "camel", "mongoose"]);
    /// p.rule("animal", &animal);
    /// let animal_space = p.add_str("animal ").compile();
    /// p.build_rule("animal_proof", vec![gf(animal_space.reps_min(1)?), sf("QED")]);
    /// let m = p.add_str("animal_proof").matcher()?;
    /// assert!(m.is_match("camel  camel   cat cow mongoose    QED"));
    /// # Ok(())}
    /// ```
    pub fn build_rule(&mut self, name: &str, components: Vec<RuleFragment>) {
        let right_limit = components.len() - 1;
        let g = Grammar {
            name: Some(name.to_string()),
            flags: self.flags.clone(),
            lower_limit: None,
            upper_limit: None,
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
    /// Removes a rule from the list known to the `Pidgin`.
    pub fn remove_rule(&mut self, name: &str) {
        self.symbols.remove(&Symbol::S(name.to_string()));
    }
    /// Like `remove_rule` but the rule identifier is a regex rather than a
    /// rule name.
    pub fn remove_rx_rule(&mut self, name: &str) -> Result<(), Error> {
        match Regex::new(name) {
            Err(e) => Err(e),
            Ok(rx) => {
                self.symbols.remove(&Symbol::Rx(rx));
                Ok(())
            }
        }
    }
    /// Removes all alternates and rule definitions from the `Pidgin`. Flags
    /// controlling case sensitivity and such remain.
    pub fn clear(&mut self) {
        self.symbols.clear();
        self.phrases.clear();
    }
    /// Toggles whether `Pidgin` creates case-insensitive rules.
    ///
    /// By default this is false.
    pub fn case_insensitive(mut self, case: bool) -> Pidgin {
        self.flags.case_insensitive = case;
        self
    }
    /// Toggles whether `Pidgin` creates multi-line rules. This governs the
    /// behavior of `^` and `$` anchors, whether they match string boundaries
    /// or after and before newline characters.
    ///
    /// By default this is false.
    pub fn multi_line(mut self, case: bool) -> Pidgin {
        self.flags.multi_line = case;
        self
    }
    /// Toggles whether `Pidgin` creates rules wherein `.` can match newline
    /// characters. This is the so-called "single line" mode of Perl-compatible
    /// regular expressions.
    ///
    /// By default this is false.
    pub fn dot_all(mut self, case: bool) -> Pidgin {
        self.flags.dot_all = case;
        self
    }
    /// Toggles whether `Pidgin` creates Unicode-compliant rules.
    ///
    /// By default this is true.
    pub fn unicode(mut self, case: bool) -> Pidgin {
        self.flags.unicode = case;
        self
    }
    /// Toggles whether `Pidgin` creates rules that can safely be modified by
    /// a repetition expression. `(?:ab)` is enclosed. `ab` is not.
    ///
    /// This parameter is generally of interest only when using `Pidgin` to
    /// create elements of other regular expressions.
    ///
    /// By default this is false.
    pub fn enclosed(mut self, case: bool) -> Pidgin {
        self.flags.enclosed = case;
        self
    }
    /// Toggles the U flag of Rust regexen. Per the documentation, U "swap\[s\] the
    /// meaning of x* and x*?", thus turning a stingy match greedy and a greedy
    /// match stingy.
    ///
    /// By default this is false.
    pub fn reverse_greed(mut self, case: bool) -> Pidgin {
        self.flags.reverse_greed = case;
        self
    }
    /// Treat any white space found in an alternate as "some amount of white space".
    /// if the `required` parameter is `true`, it means "at least some white
    /// space". If it is false, it means "maybe some white space".
    pub fn normalize_whitespace(mut self, required: bool) -> Pidgin {
        self.remove_rx_rule(r"\s+").unwrap();
        if required {
            self.foreign_rx_rule(r"\s+", r"\s+", None).unwrap();
        } else {
            self.foreign_rx_rule(r"\s+", r"\s*", None).unwrap();
        }
        self
    }
    /// The left and right edges of all alternates, when applicable, should be
    /// word boundaries -- `\b`. If the alternate has a non-word character at the
    /// boundary in question, such as "@" or "(", then it is left alone, but if
    /// it is a word character, it should be bounded by a `\b` in the regular
    /// expression generated.
    pub fn word_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::Word);
        self.right = Some(Boundary::Word);
        self
    }
    /// Alternates should have word boundaries, where applicable, on the left margin.
    pub fn left_word_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::Word);
        self
    }
    /// Alternates should have word boundaries, where applicable, on the right margin.
    pub fn right_word_bound(mut self) -> Pidgin {
        self.right = Some(Boundary::Word);
        self
    }
    /// Alternates should match entire lines.
    ///
    /// ***NOTE*** This turns multi-line matching on for the rule.
    pub fn line_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::Line);
        self.right = Some(Boundary::Line);
        self.flags.multi_line = true;
        self
    }
    /// Alternates should match at the beginning of the line on their left margin.
    ///
    /// ***NOTE*** This turns multi-line matching on for the rule.
    pub fn left_line_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::Line);
        self.flags.multi_line = true;
        self
    }
    /// Alternates should match at the beginning of the line on their right margin.
    ///
    /// ***NOTE*** This turns multi-line matching on for the rule.
    pub fn right_line_bound(mut self) -> Pidgin {
        self.right = Some(Boundary::Line);
        self.flags.multi_line = true;
        self
    }
    /// The rule should match the entire string.
    pub fn string_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::String);
        self.right = Some(Boundary::String);
        self
    }
    /// The left margin of every alternate should be the beginning of the line.
    pub fn left_string_bound(mut self) -> Pidgin {
        self.left = Some(Boundary::String);
        self
    }
    /// The right margin of every alternate should be the beginning of the line.
    pub fn right_string_bound(mut self) -> Pidgin {
        self.right = Some(Boundary::String);
        self
    }
    /// Clears any expectation that alternates have boundary anchors.
    pub fn unbound(mut self) -> Pidgin {
        self.left = None;
        self.right = None;
        self
    }
    // Compiles the rule to a grammar with no capturing groups.
    pub fn compile_non_capturing(&self) -> Grammar {
        let g = self.clone().compile().clear_recursive();
        let sequence = self.recursive_condense_sequence(&g.sequence);
        Grammar {
            sequence,
            name: None,
            flags: g.flags.clone(),
            lower_limit: None,
            upper_limit: None,
        }
    }
    /// Convenience method for generating non-backtracking regular expressions.
    /// ```rust
    /// # use pidgin::Pidgin;
    /// Pidgin::rx(&vec!["cat", "camel", "aaaabbbbaaaabbbb"]); // (?:ca(?:t|mel)|(?:a{4}b{4}){2})
    /// ```
    pub fn rx(words: &[&str]) -> String {
        Pidgin::new().grammar(words).to_string()
    }
    /// Convenience method equivalent to `compile().matcher()`
    ///
    /// # Errors
    ///
    /// `matcher` throws errors where `Grammar::matcher` throws errors.
    pub fn matcher(&self) -> Result<Matcher, Error> {
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
pub enum RuleFragment {
    S(String),
    G(Grammar),
}

/// Constructs a `RuleFragment::S`.
///
/// The name is short for "string fragment". This is just a way to construct a
/// string `RuleFragment` in fewer keystrokes.
pub fn sf(string: &str) -> RuleFragment {
    RuleFragment::S(string.to_string())
}

/// Constructs a `RuleFragment::G`.
///
/// The name is short for "grammar fragment". This is just a way to construct a
/// grammar `RuleFragment` in fewer keystrokes.
pub fn gf(g: Grammar) -> RuleFragment {
    RuleFragment::G(g)
}
