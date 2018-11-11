use crate::matching::Matcher;
use crate::util::{is_atomic, Expression, Flags};
use regex::{Error, Regex};
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::collections::{BTreeSet, VecDeque};
use std::fmt;

/// A compiled collection of rules ready for the building of a
/// [`Matcher`] or for use in the definition of a new rule.
///
/// You do not build new `Grammar`s directly. Rather, the [`grammar!`] macro compiles them for you.
/// 
/// [`Matcher`]:  ../pidgin/struct.Matcher.html
/// [`grammar!`]: ../pidgin/macro.grammar.html

#[derive(Clone, Debug)]
pub struct Grammar {
    ///
    pub name: Option<String>,
    pub(crate) sequence: Vec<Expression>,
    pub(crate) flags: Flags,
    pub(crate) stingy: bool,
    pub(crate) lower_limit: Option<usize>,
    pub(crate) upper_limit: Option<usize>,
}

impl Grammar {
	pub(crate) fn rx_rule(rx: String) -> Grammar {
		Grammar{
			name: None,
			flags: Flags::defaults(),
			stingy: false,
			lower_limit: None,
			upper_limit: None,
			sequence: vec![Expression::Part(rx, false)],
		}
	}
    pub(crate) fn name(&mut self, name: &str) {
        self.name = Some(name.to_string());
    }
    /// Compiles a `Matcher` based on the `Grammar`'s rules.
    ///
	/// # Examples
	/// 
	/// ```rust
    /// # #[macro_use] extern crate pidgin;
    /// # use std::error::Error;
    /// # fn demo() -> Result<(), Box<Error>> {
	/// let m = grammar!{
	/// 	(?wbB)
	/// 	noun   => <person> | <place> | <thing>
	/// 	person => [["Moe", "Larry", "Curly", "Sade", "Diana Ross"]]
	/// 	place  => [["Brattleboro, Vermont"]]
	/// 	thing  => [["tiddly wink", "muffin", "kazoo"]]
	/// }.matcher()?;
	/// # Ok(()) }
	/// ```
	/// 
    /// # Errors
    ///
    /// If the `Grammar` contains an ill-formed `r(rx)` or one with a named capture
	/// which is repeated, an error will be returned.
    pub fn matcher(&self) -> Result<Matcher, Error> {
        Matcher::new(&self)
    }
    pub(crate) fn reps_min(&self, r: usize) -> Result<Grammar, String> {
        if self.upper_limit.is_none() || self.lower_limit.unwrap() >= r {
            let mut flags = self.flags.clone();
            flags.enclosed = true;
            Ok(Grammar {
                flags,
                name: self.name.clone(),
                sequence: self.sequence.clone(),
                lower_limit: Some(r),
                upper_limit: self.upper_limit.clone(),
                stingy: self.stingy,
            })
        } else {
            Err(format!(
                "minimum repetitions {} is greater than maximum repetitions {}",
                r,
                self.upper_limit.unwrap()
            ))
        }
    }
    pub(crate) fn reps_max(&self, r: usize) -> Result<Grammar, String> {
        if self.lower_limit.is_none() || self.lower_limit.unwrap() <= r {
            if r == 0 {
                Err(String::from(
                    "the maximum number of repetitions must be greater than 0",
                ))
            } else {
                let mut flags = self.flags.clone();
                flags.enclosed = true;
                Ok(Grammar {
                    flags,
                    name: self.name.clone(),
                    sequence: self.sequence.clone(),
                    lower_limit: self.lower_limit.clone(),
                    upper_limit: Some(r),
                    stingy: self.stingy,
                })
            }
        } else {
            Err(format!(
                "minimum repetitions {} is greater than maximum repetitions {}",
                self.lower_limit.unwrap(),
                r
            ))
        }
    }
    pub(crate) fn stingy(mut self, stingy: bool) -> Grammar {
        self.stingy = stingy;
        self
    }
    pub(crate) fn any_suffix(&self) -> bool {
        (self.lower_limit.is_some() || self.upper_limit.is_some()) && !(self.lower_limit.is_some()
            && self.upper_limit.is_some()
            && self.lower_limit.unwrap() == 1
            && self.upper_limit.unwrap() == 1)
    }
    pub(crate) fn repetition_suffix(&self) -> String {
        let stingy_modifier = if self.stingy { "?" } else { "" };
        if self.lower_limit.is_none() {
            if self.upper_limit.is_none() {
                String::from("")
            } else {
                let r = self.upper_limit.unwrap();
                if r == 1 {
                    format!("?{}", stingy_modifier)
                } else {
                    format!("{{0,{}}}{}", r, stingy_modifier)
                }
            }
        } else if self.upper_limit.is_none() {
            let r = self.lower_limit.unwrap();
            match r {
                0 => format!("*{}", stingy_modifier),
                1 => format!("+{}", stingy_modifier),
                _ => format!("{{{},}}{}", r, stingy_modifier),
            }
        } else {
            let min = self.lower_limit.unwrap();
            let max = self.upper_limit.unwrap();
            match min {
                0 => match max {
                    1 => format!("?{}", stingy_modifier),
                    _ => format!("{{0,{}}}{}", max, stingy_modifier),
                },
                _ => {
                    if min == max {
                        if min == 1 {
                            String::from("")
                        } else {
                            format!("{{{}}}", min)
                        }
                    } else {
                        format!("{{{},{}}}{}", min, max, stingy_modifier)
                    }
                }
            }
        }
    }
    fn rules(&self) -> Vec<String> {
        let mut rules = Vec::new();
        let mut expressions = VecDeque::new();
        for e in &self.sequence {
            expressions.push_back(e);
        }
        loop {
            if let Some(e) = expressions.pop_front() {
                match e {
                    Expression::Grammar(ref g, _) => {
                        if let Some(ref n) = g.name {
                            rules.push(n.to_string());
                        }
                    }
                    Expression::Alternation(v, _) | Expression::Sequence(v, _) => {
                        for e in v {
                            expressions.push_back(e);
                        }
                    }
                    Expression::Repetition(e, _, _) => expressions.push_back(e),
                    _ => (),
                }
            } else {
                break;
            }
        }
        rules
    }
    /// Return a copy of one of the rules used by the grammar.
    ///
    /// This is chiefly useful when combining grammars generated by the macro.
    ///
    /// # Examples
    ///
    /// ```rust
    /// #[macro_use] extern crate pidgin;
    /// let library = grammar!{
    ///     books => <cat> | <dog> | <camel>
    ///     cat   => [["persian", "siamese", "calico", "tabby"]]
    ///     dog   => [["dachshund", "chihuahua", "corgi", "malamute"]]
    ///     camel => [["bactrian", "dromedary"]]
    /// };
    /// let g = grammar!{
    ///     seen -> ("I saw a") g(library.rule("cat").unwrap()) (".")
    /// };
    /// let matcher = g.matcher().unwrap();
    /// assert!(matcher.is_match("I saw a calico."));
    /// ```
    pub fn rule(&self, rule: &str) -> Option<Grammar> {
        let mut expressions = VecDeque::new();
        for e in &self.sequence {
            expressions.push_back(e);
        }
        loop {
            if let Some(e) = expressions.pop_front() {
                match e {
                    Expression::Grammar(ref g, _) => {
                        if let Some(ref n) = g.name {
                            if n == rule {
                                return Some(g.clone());
                            }
                        }
                        for e in &g.sequence {
                            expressions.push_back(&e);
                        }
                    }
                    Expression::Alternation(v, _) | Expression::Sequence(v, _) => {
                        for e in v {
                            expressions.push_back(e);
                        }
                    }
                    Expression::Repetition(e, _, _) => expressions.push_back(e),
                    _ => (),
                }
            } else {
                break;
            }
        }
        None
    }
    /// Generates a non-capturing regex matching what the grammar matches.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # #[macro_use] extern crate pidgin;
    /// # use std::error::Error;
    /// # fn dem() -> Result<(),Box<Error>> {
    /// let g = grammar!{
    ///     foo -> r(r"\A") <bar> r(r"\z")
    ///     bar => (?i) [["cat", "camel", "corn"]]
    /// };
    /// let rx = g.rx()?.to_string();
    /// assert_eq!(r"\A(?i:\s*c(?:orn|a(?:t|mel)))\s*\z", rx);
    /// # Ok(()) }
    /// ```
    ///
    /// ```rust
    /// # #[macro_use] extern crate pidgin;
    /// # use std::error::Error;
    /// # fn dem() -> Result<(),Box<Error>> {
    /// let g = grammar!{
    ///     sentence    -> <capitalized_word> <other_words>? <terminal_punctuation>
    ///     other_words -> <other_word>+
    ///     other_word  -> <non_terminal_punctuation>? <word>
    ///     capitalized_word         => r(r"\b[A-Z]\w*\b")
    ///     word                     => r(r"\b\w+\b")
    ///     terminal_punctuation     => r(r"[.?!]")
    ///     non_terminal_punctuation => r("(?:--?|[,;'\"])")
    /// };
    /// let rx = g.rule("word").unwrap().rx().unwrap();
    /// let p = g
    ///     .matcher()?
    ///     .parse("John, don't forget to pick up chips.")
    ///     .unwrap();
    /// let other_words = p.name("other_words").unwrap().as_str();
    /// let other_words = rx
    ///     .find_iter(other_words)
    ///     .map(|m| m.as_str())
    ///     .collect::<Vec<_>>();
    /// assert_eq!(
    ///     vec!["don", "t", "forget", "to", "pick", "up", "chips"],
    ///     other_words
    /// );
    /// # Ok(()) }
    /// ```
    pub fn rx(&self) -> Result<Regex, Error> {
        let mut g = self.clear_recursive();
        g.clear_name();
        Regex::new(g.to_string().as_str())
    }
    fn describable_grammar(&self) -> Grammar {
        let mut g = self.clone();
        g.name = None;
        g.flags.enclosed = false;
        g
    }
    fn _describe(&self) -> String {
        let top = self.sequence.len() == 1;
        self.sequence
            .iter()
            .map(|e| e.to_s(&self.flags, true, top))
            .collect::<Vec<_>>()
            .join("")
    }
    fn find(&self, name: &str) -> Option<Grammar> {
        let mut expressions = VecDeque::new();
        for e in &self.sequence {
            expressions.push_back(e);
        }
        loop {
            if let Some(e) = expressions.pop_front() {
                match e {
                    Expression::Grammar(ref g, _) => {
                        if let Some(ref n) = g.name {
                            if n == name {
                                let mut g = g.clone();
                                g.name = None;
                                return Some(g);
                            }
                        }
                    }
                    Expression::Alternation(v, _) | Expression::Sequence(v, _) => {
                        for e in v {
                            expressions.push_back(e);
                        }
                    }
                    Expression::Repetition(e, _, _) => expressions.push_back(e),
                    _ => (),
                }
            } else {
                break;
            }
        }
        None
    }
    /// Returns a quasi-regex representation of the grammar. This is intended
    /// mostly for debugging. Rules will be identifiable by named groups, but
    /// group names may repeat, in which case the stringification cannot be
    /// compiled into a regular expression.
	/// 
	/// # Examples
	/// 
    /// ```rust
    /// # #[macro_use] extern crate pidgin;
    /// # use std::error::Error;
    /// # fn dem() -> Result<(),Box<Error>> {
    /// let g = grammar!{
    ///     foo -> <bar> <baz> | <baz> <bar>
    ///     bar => ("baz")
	///     baz => ("bar")
    /// };
    /// println!("{}", g.to_string());
    /// # Ok(()) }
    /// ```
    pub(crate) fn to_string(&self) -> String {
        self.to_s(&Flags::defaults(), false, false)
    }
    pub(crate) fn clear_recursive(&self) -> Grammar {
        let sequence = self.sequence.iter().map(|e| e.clear_names()).collect();
        Grammar {
            sequence,
            name: None,
            flags: self.flags.clone(),
            upper_limit: self.upper_limit.clone(),
            lower_limit: self.lower_limit.clone(),
            stingy: self.stingy,
        }
    }
    pub(crate) fn clear_name(&mut self) {
        self.name = None;
    }
    fn needs_closure(&self, context: &Flags) -> bool {
        self.flags.enclosed
            || self.needs_flags_set(context)
            || self.any_suffix() && !(self.sequence.len() == 1
                && is_atomic(&self.sequence[0].to_s(context, true, true)))
    }
    fn needs_flags_set(&self, context: &Flags) -> bool {
        self.flags.case_insensitive ^ context.case_insensitive
            || self.flags.dot_all ^ context.dot_all
            || self.flags.multi_line ^ context.multi_line
            || self.flags.unicode ^ context.unicode
            || self.flags.reverse_greed ^ context.reverse_greed
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
            } else {
                flags_off.push("i");
            }
        }
        if self.flags.multi_line ^ context.multi_line {
            if self.flags.multi_line {
                flags_on.push("m");
            } else {
                flags_off.push("m");
            }
        }
        if self.flags.dot_all ^ context.dot_all {
            if self.flags.dot_all {
                flags_on.push("s");
            } else {
                flags_off.push("s");
            }
        }
        if self.flags.unicode ^ context.unicode {
            if !self.flags.unicode {
                flags_off.push("u");
            } else {
                flags_on.push("u");
            }
        }
        if self.flags.reverse_greed ^ context.reverse_greed {
            if self.flags.reverse_greed {
                flags_on.push("U");
            } else {
                flags_off.push("U");
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
    pub(crate) fn to_s(&self, context: &Flags, describing: bool, top: bool) -> String {
        let mut s = if self.name.is_some() {
            format!("(?P<{}>", self.name.as_ref().unwrap())
        } else {
            String::new()
        };
        let closure_skippable = top && !self.any_suffix();
        if !closure_skippable && self.needs_closure(context) {
            s = s + format!("(?{}:", self.flags(context)).as_str();
        }
        for e in &self.sequence {
            s += e.to_s(&self.flags, describing, top).as_ref();
        }
        if !closure_skippable && self.needs_closure(context) {
            s = s + ")"
        }
        if self.name.is_some() {
            s = s + ")"
        }
        s + self.repetition_suffix().as_str()
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

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.rx().unwrap())
    }
}
