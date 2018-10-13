use matching::Matcher;
use regex::Error;
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::collections::{BTreeSet, VecDeque};
use std::fmt;
use util::{is_atomic, Expression, Flags};

/// A compiled collection of rules ready for the building of a
/// `pidgin::Matcher` or for use in the definition of a new rule.
///
/// You do not build new `Grammar`s directly. Rather, the `compile` or `grammar`
/// method of a `Pidgin` produces them for you.

#[derive(Clone, Debug)]
pub struct Grammar {
    pub name: Option<String>,
    pub(crate) sequence: Vec<Expression>,
    pub(crate) flags: Flags,
    pub(crate) lower_limit: Option<usize>,
    pub(crate) upper_limit: Option<usize>,
}

impl Grammar {
    /// Assigns a name to the grammar.
    ///
    /// This will have no effect on any uses of the grammar already used in
    /// rules.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use pidgin::Pidgin;
    /// let mut p = Pidgin::new();
    /// let g = p.grammar(&vec!["foo", "bar", "baz"]);
    /// p.rule("foo", &g);
    /// let mut g = p.grammar(&vec!["foo cat", "foo dog"]);
    /// print!("{}", g);
    /// // TOP := {foo} (?:cat|dog)
    /// // foo := foo|ba[rz]
    /// g.name("Bartholemew");
    /// print!("{}", g);
    /// // Bartholemew := {foo} (?:cat|dog)
    /// //         foo := foo|ba[rz]
    /// ```
    pub fn name(&mut self, name: &str) {
        self.name = Some(name.to_string());
    }
    /// Compiles a `Matcher` based on the `Grammar`'s rules.
    ///
    /// # Errors
    ///
    /// If the `Grammar` contains a "foreign rule" with a named capture, an
    /// error may be returned.
    pub fn matcher(&self) -> Result<Matcher, Error> {
        Matcher::new(&self)
    }
    /// Sets the required number of repetitions of the grammar in the string
    /// matched against to exactly `r`.
    ///
    /// This is chiefly useful in conjunction with `Pidgin::build_rule`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use pidgin::{Pidgin, sf, gf};
    /// # use std::error::Error;
    /// # fn dem() -> Result<(),Box<Error>> {
    /// let mut p = Pidgin::new();
    /// let g = p.grammar(&vec!["foo", "bar", "baz"]);
    /// p.build_rule("foo", vec![sf("xyzzy "), gf(g.reps(3)), sf(" plugh")]);
    /// let g = p.grammar(&vec!["foo"]);
    /// let m = g.matcher()?;
    /// assert!(m.is_match("xyzzy foobarbaz plugh"));
    /// assert!(!m.is_match("xyzzy foo plugh"));
    /// assert!(!m.is_match("xyzzy foobarbazfoo plugh"));
    /// # Ok(()) }
    /// ```
    pub fn reps(&self, r: usize) -> Grammar {
        let mut g = self.clone();
        g.lower_limit = Some(r);
        g.upper_limit = Some(r);
        g
    }
    /// Sets the minimum required number of repetitions of the grammar in the string
    /// matched against to `r`. If no maximum is set, this will be equivalent
    /// to the regex repetition suffix `{r,}` -- there will be no upper limit.
    ///
    /// If you set the lower limit to 0 and set no upper limit, this will be
    /// equivalent to the regex repetition suffix `*`. Likewise, 1 and no upper
    /// limit will be equivalent to `+`.
    ///
    /// This is chiefly useful in conjunction with `Pidgin::build_rule`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use pidgin::{Pidgin, sf, gf};
    /// # use std::error::Error;
    /// # fn dem() -> Result<(),Box<Error>> {
    /// let mut p = Pidgin::new();
    /// let g = p.grammar(&vec!["foo", "bar", "baz"]);
    /// p.build_rule("foo", vec![sf("xyzzy "), gf(g.reps_min(3)?), sf(" plugh")]);
    /// let m = p.matcher()?;
    /// assert!(m.is_match("xyzzy foobarbaz plugh"));
    /// assert!(m.is_match("xyzzy foobarbazfoo plugh"));
    /// assert!(!m.is_match("xyzzy foobar plugh"));
    /// # Ok(()) }
    /// ```
    ///
    /// # Errors
    ///
    /// If an upper limit has been set and it is lower than the minimum, an
    /// error will be returned
    pub fn reps_min(&self, r: usize) -> Result<Grammar, String> {
        if self.upper_limit.is_none() || self.lower_limit.unwrap() >= r {
            let mut flags = self.flags.clone();
            flags.enclosed = true;
            Ok(Grammar {
                flags,
                name: self.name.clone(),
                sequence: self.sequence.clone(),
                lower_limit: Some(r),
                upper_limit: self.upper_limit.clone(),
            })
        } else {
            Err(format!(
                "minimum repetitions {} is greater than maximum repetitions {}",
                r,
                self.upper_limit.unwrap()
            ))
        }
    }
    /// Sets the maximum number of repetitions of the grammar in the string
    /// matched against to `r`. If no minimum is set, this will be equivalent
    /// to the regex repetition suffix `{0,r}` -- there will be no lower limit.
    ///
    /// This is chiefly useful in conjunction with `Pidgin::build_rule`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use pidgin::{Pidgin, sf, gf};
    /// # use std::error::Error;
    /// # fn dem() -> Result<(),Box<Error>> {
    /// let mut p = Pidgin::new();
    /// let g = p.grammar(&vec!["foo", "bar", "baz"]);
    /// p.build_rule("foo", vec![sf("xyzzy "), gf(g.reps_max(3)?), sf(" plugh")]);
    /// let m = p.matcher()?;
    /// assert!(m.is_match("xyzzy foobarbaz plugh"));
    /// assert!(m.is_match("xyzzy foobar plugh"));
    /// assert!(!m.is_match("xyzzy foobarbazfoo plugh"));
    /// # Ok(()) }
    /// ```
    ///
    /// # Errors
    ///
    /// If an lower limit has been set and it is higher than the maximum, an
    /// error will be returned. Also, if the maximum is set to 0 an error will
    /// be returned.
    pub fn reps_max(&self, r: usize) -> Result<Grammar, String> {
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
    /// Sets the minimum number of repetitions of the grammar in the string
    /// matched against to `min` and the maximum to `max`. If no minimum is set,
    /// this will be equivalent to the regex repetition suffix `{min,max}`.
    ///
    /// This is chiefly useful in conjunction with `Pidgin::build_rule`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use pidgin::{Pidgin, sf, gf};
    /// # use std::error::Error;
    /// # fn dem() -> Result<(),Box<Error>> {
    /// let mut p = Pidgin::new();
    /// let g = p.grammar(&vec!["foo", "bar", "baz"]);
    /// p.build_rule("foo", vec![sf("xyzzy "), gf(g.reps_min_max(1, 3)?), sf(" plugh")]);
    /// let m = p.matcher()?;
    /// assert!(m.is_match("xyzzy foo plugh"));
    /// assert!(m.is_match("xyzzy foobar plugh"));
    /// assert!(m.is_match("xyzzy foobarbaz plugh"));
    /// assert!(!m.is_match("xyzzy  plugh"));
    /// assert!(!m.is_match("xyzzy foobarbazfoo plugh"));
    /// # Ok(()) }
    /// ```
    ///
    /// # Errors
    ///
    /// If the minimum is greater than the maximum or the maximum is 0, an errors
    /// will be returned.
    pub fn reps_min_max(&self, min: usize, max: usize) -> Result<Grammar, String> {
        if min <= max {
            if max == 0 {
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
                    lower_limit: Some(min),
                    upper_limit: Some(max),
                })
            }
        } else {
            Err(format!(
                "minimum repetitions {} is greater than maximum repetitions {}",
                min, max
            ))
        }
    }
    pub(crate) fn any_suffix(&self) -> bool {
        (self.lower_limit.is_some() || self.upper_limit.is_some()) && !(self.lower_limit.is_some()
            && self.upper_limit.is_some()
            && self.lower_limit.unwrap() == 1
            && self.upper_limit.unwrap() == 1)
    }
    pub(crate) fn repetition_suffix(&self) -> String {
        if self.lower_limit.is_none() {
            if self.upper_limit.is_none() {
                String::from("")
            } else {
                let r = self.upper_limit.unwrap();
                if r == 1 {
                    String::from("?")
                } else {
                    format!("{{0,{}}}", r)
                }
            }
        } else if self.upper_limit.is_none() {
            let r = self.lower_limit.unwrap();
            match r {
                0 => String::from("*"),
                1 => String::from("+"),
                _ => format!("{{{},}}", r),
            }
        } else {
            let min = self.lower_limit.unwrap();
            let max = self.upper_limit.unwrap();
            match min {
                0 => match max {
                    1 => String::from("?"),
                    _ => format!("{{0,{}}}", max),
                },
                _ => {
                    if min == max {
                        if min == 1 {
                            String::from("")
                        } else {
                            format!("{{{}}}", min)
                        }
                    } else {
                        format!("{{{},{}}}", min, max)
                    }
                }
            }
        }
    }
    /// Produces a quasi-BNF representation of the grammar.
    ///
    /// This is how `pidging::Grammar` implements `std::fmt::Display`, so a
    /// grammar's description is what you get if you feed it to the `{}` pattern.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use pidgin::Pidgin;
    /// let mut p = Pidgin::new();
    /// let g = p.grammar(&vec!["bar", "baz"]);
    /// p.rule("foo", &g);
    /// p = p.case_insensitive(true);
    /// let g = p.grammar(&vec!["ping", "pang", "pong"]);
    /// p = p.case_insensitive(false);
    /// p.rule("xyzzy", &g);
    /// let g = p.grammar(&vec!["xyzzy", "qux"]);
    /// p.rule("plugh", &g);
    /// let g = p.grammar(&vec!["foo", "plugh"]);
    /// println!("{}", g.describe());
    /// //   TOP :=      {foo}|{plugh}
    /// //   foo :=      ba[rz]
    /// // plugh :=      qux|{xyzzy}
    /// // xyzzy := (?i) p[aio]ng
    /// println!("{}", g);
    /// //   TOP :=      {foo}|{plugh}
    /// //   foo :=      ba[rz]
    /// // plugh :=      qux|{xyzzy}
    /// // xyzzy := (?i) p[aio]ng
    /// ```
    pub fn describe(&self) -> String {
        let g = self.describable_grammar();
        let mut flags = g.flags(&Flags::defaults());
        if flags.len() > 0 {
            flags = format!("(?{})", flags);
        }
        let mut rules = vec![(
            self.name
                .as_ref()
                .unwrap_or(&String::from("TOP"))
                .to_string(),
            flags,
            g._describe(),
        )];
        let mut seen: BTreeSet<String> = BTreeSet::new();
        let mut queue = VecDeque::new();
        for r in self.rules() {
            let og = g.find(&r).unwrap().describable_grammar();
            queue.push_back((r, og));
        }
        loop {
            if let Some((r, g)) = queue.pop_front() {
                if !seen.contains(&r) {
                    let mut flags = g.flags(&Flags::defaults());
                    if flags.len() > 0 {
                        flags = format!("(?{})", flags);
                    }
                    rules.push((r.clone(), flags, g._describe()));
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
        let mut flag_max = 0;
        for (ref n, flags, _) in &rules {
            let l = n.len();
            if l > max {
                max = l;
            }
            let l = flags.len();
            if l > flag_max {
                flag_max = l;
            }
        }
        if flag_max > 0 {
            flag_max += 1;
        }
        let mut s = String::new();
        for (n, f, d) in rules {
            s =
                s + &format!(
                    "{: >width$} := {: <flag_width$}{}",
                    n,
                    f,
                    d,
                    width = max,
                    flag_width = flag_max
                ) + "\n";
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
        let top = self.sequence.len() == 1;
        self.sequence
            .iter()
            .map(|e| e.to_s(&self.flags, true, top))
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
            upper_limit: self.upper_limit.clone(),
            lower_limit: self.lower_limit.clone(),
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
        write!(f, "{}", self.describe())
    }
}
