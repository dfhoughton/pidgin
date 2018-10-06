extern crate regex;
use grammar::Grammar;
use regex::{escape, Regex};
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};

#[cfg(test)]
mod tests {
    use util::is_atomic;
    #[test]
    fn single_characters_are_atomic() {
        for c in String::from("abc.").chars() {
            assert!(is_atomic(&c.to_string()));
        }
    }
    #[test]
    fn two_chars_where_first_is_not_slash_non_atomic() {
        let words = vec!["aa", "bc", ".."];
        for w in words {
            assert!(!is_atomic(w));
        }
    }
    #[test]
    fn leading_slash_then_char_is_atomic() {
        let words = vec![r"\a", r"\c", r"\."];
        for w in words {
            assert!(is_atomic(w));
        }
    }
    #[test]
    fn simple_char_class_is_atomic() {
        let words = vec![r"[.]", r"[asdf]", r"[0-9]", r"[^0-9]", r"[(as)]"];
        for w in words {
            assert!(is_atomic(w));
        }
    }
    #[test]
    fn sequence_of_char_class_is_not_atomic() {
        let words = vec![
            r"[.][.]",
            r"[asdf][asdf]",
            r"[0-9][0-9]",
            r"[^0-9][^0-9]",
            r"[(as)][(as)]",
        ];
        for w in words {
            assert!(!is_atomic(w));
        }
    }
    #[test]
    fn simple_char_class_with_escape_is_atomic() {
        let words = vec![r"[\[.]", r"[\]asdf]", r"[\.0-9]", r"[\^0-9]", r"[(as\)]"];
        for w in words {
            assert!(is_atomic(w));
        }
    }
    #[test]
    fn group_is_atomic() {
        let words = vec![r"([x[^xyz]])", r"(....)"];
        for w in words {
            assert!(is_atomic(w));
        }
    }
    #[test]
    fn sequence_of_groups_is_not_atomic() {
        let words = vec![r"([x[^xyz]])(as)", r"(....)(foo)"];
        for w in words {
            assert!(!is_atomic(w));
        }
    }
    #[test]
    fn p_named_char_class_is_atomic() {
        let words = vec![r"\pN", r"\PN", r"\p{Greek}", r"\P{Greek}"];
        for w in words {
            assert!(is_atomic(w));
        }
    }
    #[test]
    fn char_and_p_named_char_class_is_not_atomic() {
        let words = vec![r"\pNa", r"a\PN", r"\p{Greek}a", r"a\P{Greek}"];
        for w in words {
            assert!(!is_atomic(w));
        }
    }
    #[test]
    fn one_level_char_class_nesting() {
        let words = vec![r"[[:alpha:]]", r"[x[^xyz]]", r"[0-9&&[^4]"];
        for w in words {
            assert!(is_atomic(w));
        }
    }
    #[test]
    fn one_level_group_nesting() {
        let words = vec![r"(a(b))", r"((abc))", r"((abc)defg(geh))"];
        for w in words {
            assert!(is_atomic(w));
        }
    }
}

pub(crate) fn is_atomic(s: &str) -> bool {
    lazy_static! {
        static ref ATOMIC: Regex = Regex::new(
            r"(?x)
                \A
                (?:
                    .                                         # single characters
                        |
                    \[ (?: [^\]] | \\. | \[[^\[\]]+\] )+ \]   # char classes
                        |
                    \( (?: [^)] | \\. | \([^()]+\) )+ \)      # groups
                        |
                    \\ (?:                                    # forward slash patterns

                        # there are more of these, but we only match the less obscure ones

                        [pP] (?: [a-zA-Z] | \{ [a-zA-Z]+ \})  # unicode properties
                            |
                        .                                     # escaped characters
                    )
                )
                \z
            "
        )
        .unwrap();
    }
    ATOMIC.is_match(s)
}

pub(crate) fn character_class_escape(c: char) -> String {
    match c {
        '\\' | '-' | '[' | '^' | ']' | '&' | '~' => format!("\\{}", c),
        _ => format!("{}", c),
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Flags {
    pub(crate) case_insensitive: bool,
    pub(crate) dot_all: bool,
    pub(crate) multi_line: bool,
    pub(crate) unicode: bool,
    pub(crate) enclosed: bool,
}

impl Flags {
    pub(crate) fn defaults() -> Flags {
        Flags {
            case_insensitive: false,
            dot_all: false,
            multi_line: false,
            unicode: true,
            enclosed: false,
        }
    }
}

impl PartialOrd for Flags {
    fn partial_cmp(&self, other: &Flags) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Flags {
    fn cmp(&self, other: &Flags) -> Ordering {
        let o = self.case_insensitive.cmp(&other.case_insensitive);
        if o != Ordering::Equal {
            return o;
        }
        let o = self.dot_all.cmp(&other.dot_all);
        if o != Ordering::Equal {
            return o;
        }
        let o = self.multi_line.cmp(&other.multi_line);
        if o != Ordering::Equal {
            return o;
        }
        let o = self.unicode.cmp(&other.unicode);
        if o != Ordering::Equal {
            return o;
        }
        let o = self.enclosed.cmp(&other.enclosed);
        if o != Ordering::Equal {
            return o;
        }
        Ordering::Equal
    }
}

impl PartialEq for Flags {
    fn eq(&self, other: &Flags) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Flags {}

pub(crate) enum CharRange {
    C(char),
    CC(char, char),
}

#[derive(Clone, Debug)]
pub(crate) enum Symbol {
    S(String), // String is name; bool is whether to capture this group
    Rx(Regex), // Option<String> is an optional name
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
            &Symbol::S(ref s1) => match other {
                &Symbol::S(ref s2) => {
                    let o = s2.len().cmp(&s1.len());
                    if o != Ordering::Equal {
                        o
                    } else {
                        s1.cmp(s2)
                    }
                }
                _ => Ordering::Less,
            },
            Symbol::Rx(r1) => match other {
                Symbol::Rx(r2) => {
                    let s1 = r1.to_string();
                    let s2 = r2.to_string();
                    if s1.len() == s2.len() {
                        s1.cmp(&s2)
                    } else if s1.len() > s2.len() {
                        Ordering::Less
                    } else {
                        Ordering::Greater
                    }
                }
                _ => Ordering::Greater,
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
pub(crate) enum Expression {
    Char(char, bool),
    Alternation(Vec<Expression>, bool),
    Part(String, bool),                       // partial compilation
    Grammar(Grammar, bool),                   // a sub-grammar compiled into the current grammar
    Repetition(Box<Expression>, usize, bool), //
    Sequence(Vec<Expression>, bool),          //
    Raw(String), // initial state preceding conversion into other expressions
}

impl Expression {
    fn is_optional(&self) -> bool {
        match *self {
            Expression::Char(_, b) => b,
            Expression::Alternation(_, b) => b,
            Expression::Part(_, b) => b,
            Expression::Grammar(_, b) => b,
            Expression::Repetition(_, _, b) => b,
            Expression::Sequence(_, b) => b,
            Expression::Raw(_) => false,
        }
    }
    pub(crate) fn to_s(&self, context: &Flags) -> String {
        // String::new()
        let s = match self {
            Expression::Char(c, _) => escape(&c.to_string()),
            Expression::Alternation(v, _) => {
                if v.len() == 1 {
                    v[0].to_s(context)
                } else {
                    let mut s = String::from("(?:");
                    for (i, e) in v.iter().enumerate() {
                        if i > 0 {
                            s.push('|');
                        }
                        s += &e.to_s(context);
                    }
                    s.push(')');
                    s
                }
            }
            Expression::Repetition(e, n, _) => {
                let mut s = e.to_s(context);
                let reps = format!("{{{}}}", n);
                if is_atomic(&s) {
                    s + reps.as_str()
                } else {
                    format!("(?:{}){}", s, reps)
                }
            }
            Expression::Sequence(v, _) => v
                .iter()
                .map(|e| e.to_s(context))
                .collect::<Vec<_>>()
                .join(""),
            Expression::Grammar(g, _) => g.to_s(context),
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
    pub(crate) fn clear_names(&self) -> Expression {
        match self {
            Expression::Grammar(g, b) => Expression::Grammar(g.clear_recursive(), *b),
            Expression::Alternation(v, b) => {
                let v = v.iter().map(|e| e.clear_names()).collect();
                Expression::Alternation(v, *b)
            }
            Expression::Sequence(v, b) => {
                let v = v.iter().map(|e| e.clear_names()).collect();
                Expression::Sequence(v, *b)
            }
            Expression::Repetition(x, n, b) => {
                Expression::Repetition(Box::new(x.clear_names()), *n, *b)
            }
            _ => self.clone(),
        }
    }
    pub(crate) fn has_names(&self) -> bool {
        match self {
            Expression::Grammar(g, _) => {
                g.name.is_some() || g.sequence.iter().any(|e| e.has_names())
            }
            Expression::Alternation(v, _) => v.iter().any(|e| e.has_names()),
            Expression::Sequence(v, _) => v.iter().any(|e| e.has_names()),
            Expression::Repetition(x, _, _) => x.has_names(),
            _ => false,
        }
    }
    pub(crate) fn weight(&self) -> usize {
        match self {
            Expression::Char(_, _) => 1,
            Expression::Alternation(v, _) | Expression::Sequence(v, _) => {
                let mut sum = 0;
                for e in v {
                    sum += e.weight();
                }
                sum
            }
            Expression::Part(s, _) => s.len() * 2,
            Expression::Grammar(ref g, _) => {
                let mut sum = 0;
                for e in g.sequence.iter() {
                    sum += e.weight();
                }
                sum
            }
            Expression::Repetition(x, n, _) => x.weight() * n,
            Expression::Raw(s) => s.len(),
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
                    Ordering::Equal => b1.cmp(&b2),
                },
                _ => Ordering::Less,
            },
            Expression::Repetition(e1, c1, b1) => match other {
                Expression::Char(_, _) => Ordering::Greater,
                Expression::Repetition(e2, c2, b2) => {
                    let o = e1.cmp(&e2);
                    if o != Ordering::Equal {
                        return o;
                    }
                    let o = c1.cmp(&c2);
                    if o != Ordering::Equal {
                        return o;
                    }
                    b1.cmp(&b2)
                }
                _ => Ordering::Less,
            },
            Expression::Sequence(v1, b1) => match other {
                Expression::Char(_, _) | Expression::Repetition(_, _, _) => Ordering::Greater,
                Expression::Sequence(v2, b2) => {
                    let o = v1.len().cmp(&v2.len());
                    if o != Ordering::Equal {
                        return o;
                    }
                    for i in 0..v1.len() {
                        let e1 = &v1[i];
                        let e2 = &v2[i];
                        let o = e1.cmp(&e2);
                        if o != Ordering::Equal {
                            return o;
                        }
                    }
                    b1.cmp(&b2)
                }
                _ => Ordering::Less,
            },
            Expression::Alternation(v1, b1) => match other {
                Expression::Char(_, _)
                | Expression::Repetition(_, _, _)
                | Expression::Sequence(_, _) => Ordering::Greater,
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
                    let o = v1.len().cmp(&v2.len());
                    if o != Ordering::Equal {
                        o
                    } else {
                        b1.cmp(&b2)
                    }
                }
                _ => Ordering::Less,
            },
            Expression::Part(s1, b1) => match other {
                Expression::Grammar(_, _) | Expression::Raw(_) => Ordering::Less,
                Expression::Part(s2, b2) => match s1.cmp(s2) {
                    Ordering::Less => Ordering::Less,
                    Ordering::Greater => Ordering::Greater,
                    Ordering::Equal => b1.cmp(&b2),
                },
                _ => Ordering::Greater,
            },
            Expression::Grammar(g1, b1) => match other {
                Expression::Raw(_) => Ordering::Less,
                Expression::Grammar(g2, b2) => match g1.cmp(g2) {
                    Ordering::Less => Ordering::Less,
                    Ordering::Greater => Ordering::Greater,
                    Ordering::Equal => b1.cmp(&b2),
                },
                _ => Ordering::Greater,
            },
            Expression::Raw(s1) => match other {
                Expression::Raw(s2) => s1.cmp(s2),
                _ => Ordering::Greater,
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
#[derive(Clone, Debug)]
pub(crate) enum Boundary {
    String,
    Line,
    Word,
}
