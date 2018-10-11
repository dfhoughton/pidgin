use matching::Matcher;
use regex::Error;
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::collections::{BTreeSet, VecDeque};
use util::{Expression, Flags};

/// A compiled collection of rules ready for the building of a
/// `pidgin::Matcher` or for use in the definition of a new rule.
///
/// You do not build new `Grammar`s directly. Rather, the `compile` or `grammar`
/// method of a `Pidgin` produces them for you.

#[derive(Clone, Debug)]
pub struct Grammar {
    pub(crate) name: Option<String>,
    pub(crate) sequence: Vec<Expression>,
    pub(crate) flags: Flags,
}

impl Grammar {
    /// Compiles a `Matcher` based on the `Grammar`'s rules.
    ///
    /// # Errors
    ///
    /// If the `Grammar` contains a "foreign rule" with a named capture, an
    /// error may be returned.
    pub fn matcher(&self) -> Result<Matcher, Error> {
        Matcher::new(&self)
    }
    /// Produces a quasi-BNF representation of the grammar.
    ///
    /// ```rust
    /// # use pidgin::Pidgin;
    /// let mut p = Pidgin::new();
    /// let g = p.grammar(&vec!["bar", "baz"]);
    /// p.rule("foo", &g);
    /// let g = p.grammar(&vec!["ping", "pang", "pong"]);
    /// p.rule("xyzzy", &g);
    /// let g = p.grammar(&vec!["xyzzy", "qux"]);
    /// p.rule("plugh", &g);
    /// let g = p.grammar(&vec!["foo", "plugh"]);
    /// println!("{}", g.describe());
    /// //   TOP := {foo}|{plugh}
    /// //   foo := ba[rz]
    /// // plugh := qux|{xyzzy}
    /// // xyzzy := p[aio]ng
    /// ```
    pub fn describe(&self) -> String {
        let g = self.describable_grammar();
        let mut rules = vec![(String::from("TOP"), g._describe())];
        let mut seen: BTreeSet<String> = BTreeSet::new();
        let mut queue = VecDeque::new();
        for r in self.rules() {
            let og = g.find(&r).unwrap().describable_grammar();
            queue.push_back((r, og));
        }
        loop {
            if let Some((r, g)) = queue.pop_front() {
                if !seen.contains(&r) {
                    rules.push((r.clone(), g._describe()));
                    seen.insert(r);
                    for r in g.rules() {
                        let og = g.find(&r).unwrap().describable_grammar();
                        queue.push_back((r, og));
                    }
                }
            } else {
                break;
            }
        }
        let mut max = 3;
        for (ref n, _) in &rules {
            let l = n.len();
            if l > max {
                max = l;
            }
        }
        let mut s = String::new();
        for (n, d) in rules {
            s = s + &format!("{: >width$} := {}", n, d, width = max) + "\n";
        }
        s
    }
    fn describable_grammar(&self) -> Grammar {
        let mut g = self.clone();
        g.name = None;
        g.flags.enclosed = false;
        g
    }
    fn _describe(&self) -> String {
        self.sequence
            .iter()
            .map(|e| e.to_s(&self.flags, true, true))
            .collect::<Vec<_>>()
            .join("")
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
    pub fn to_string(&self) -> String {
        self.to_s(&Flags::defaults(), false, false)
    }
    pub(crate) fn clear_recursive(&self) -> Grammar {
        let sequence = self.sequence.iter().map(|e| e.clear_names()).collect();
        Grammar {
            sequence,
            name: None,
            flags: self.flags.clone(),
        }
    }
    pub(crate) fn clear_name(&mut self) {
        self.name = None;
    }
    fn needs_closure(&self, context: &Flags) -> bool {
        self.flags.enclosed || self.needs_flags_set(context)
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
        if self.flags.reverse_greed ^ context.reverse_greed {
            if !self.flags.reverse_greed {
                flags_off.push("U");
            } else if self.flags.reverse_greed {
                flags_on.push("U");
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
        if !top && self.needs_closure(context) {
            s = s + format!("(?{}:", self.flags(context)).as_str();
        }
        for e in &self.sequence {
            s += e.to_s(&self.flags, describing, top).as_ref();
        }
        if !top && self.needs_closure(context) {
            s = s + ")"
        }
        if self.name.is_some() {
            s = s + ")"
        }
        s
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
