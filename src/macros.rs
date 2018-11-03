use crate::grammar::Grammar;
use crate::pidgin::{Pidgin, RuleFragment};

#[macro_export]
macro_rules! grammar {
    // common state has been set, proceed to recursively nibbling away bits of the grammar
    ( @initialized $mflags:expr, $($rest:tt)+ ) => ({
        let mut l: Vec<String> = Vec::new();
        let mut m: std::collections::HashMap<String,Vec<($crate::macros::MacroFlags,Vec<$crate::macros::Part>)>> = std::collections::HashMap::new();
        grammar!(@rules l, m, $($rest)+ );
        $crate::macros::build_grammar(l, m, $mflags)
    });

    // the general way to start a new rule
    (@start_rule $l:expr, $m:expr, $name:ident, $mf:expr, $($rest:tt)+) => (
        let name = stringify!($name).to_string();
        if !$m.contains_key(&name) {
            $l.push(name.clone());
        }
        {
            let alternates = $m.entry(name).or_insert_with(|| Vec::new());
            let v: Vec<$crate::macros::Part>= Vec::new();
            alternates.push(($mf, v));
        }
        grammar!(@rules $l, $m, $($rest)+)
    );

    // extract rule name and any flags particular to that rule
    // => rules don't mess with space
    (@rules $l:expr, $m:expr, $name:ident => (?$on:ident-$off:ident) $($rest:tt)+) => (
        let new_flags = $crate::macros::MacroFlags::from_strings(stringify!($on), stringify!($off));
        grammar!(@start_rule $l, $m, $name, new_flags, $($rest)+)
    );
    (@rules $l:expr, $m:expr, $name:ident => (?$on:ident) $($rest:tt)+) => (
        let new_flags = $crate::macros::MacroFlags::from_strings(stringify!($on), "");
        grammar!(@start_rule $l, $m, $name, new_flags, $($rest)+)
    );
    (@rules $l:expr, $m:expr, $name:ident => (?-$off:ident) $($rest:tt)+) => (
        let new_flags = $crate::macros::MacroFlags::from_strings("", stringify!($off));
        grammar!(@start_rule $l, $m, $name, new_flags, $($rest)+)
    );
    (@rules $l:expr, $m:expr, $name:ident => $($rest:tt)+) => (
        let new_flags = $crate::macros::MacroFlags::defaults();
        grammar!(@start_rule $l, $m, $name, new_flags, $($rest)+)
    );
    // -> allow optional space between tokens
    (@rules $l:expr, $m:expr, $name:ident -> (?$on:ident-$off:ident) $($rest:tt)+) => (
        let mut new_flags = $crate::macros::MacroFlags::from_strings(stringify!($on), stringify!($off));
        new_flags.add_space = true;
        grammar!(@start_rule $l, $m, $name, new_flags, $($rest)+)
    );
    (@rules $l:expr, $m:expr, $name:ident -> (?$on:ident) $($rest:tt)+) => (
        let mut new_flags = $crate::macros::MacroFlags::from_strings(stringify!($on), "");
        new_flags.add_space = true;
        grammar!(@start_rule $l, $m, $name, new_flags, $($rest)+)
    );
    (@rules $l:expr, $m:expr, $name:ident -> (?-$off:ident) $($rest:tt)+) => (
        let mut new_flags = $crate::macros::MacroFlags::from_strings("", stringify!($off));
        new_flags.add_space = true;
        grammar!(@start_rule $l, $m, $name, new_flags, $($rest)+)
    );
    (@rules $l:expr, $m:expr, $name:ident -> $($rest:tt)+) => (
        let mut new_flags = $crate::macros::MacroFlags::defaults();
        new_flags.add_space = true;
        grammar!(@start_rule $l, $m, $name, new_flags, $($rest)+)
    );

    // general method for adding a rule part
    ( @add_part $l:expr, $m:expr, $p:expr, $($parts:tt)* ) => (
        let v = &mut $m.get_mut($l.last().unwrap()).unwrap().last_mut().unwrap().1;
        v.push($p);
        grammar!(@rules $l, $m, $($parts)*)
    );

    // general rule for adding a <rule>
    // repetition suffix is optional
    ( @add_grammar $l:expr, $m:expr, $e:ident, $low:expr, $high:expr, $stingy:expr, $($parts:tt)* ) => (
        grammar!(
            @add_part
            $l,
            $m,
            $crate::macros::Part::G(stringify!($e).to_string(), $low, $high, $stingy),
            $($parts)*
        )
    );

    ( @rules $l:expr, $m:expr, <$e:ident>?? $($parts:tt)* ) => (
        grammar!(@add_grammar $l, $m, $e, None, Some(1), true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, <$e:ident>? $($parts:tt)* ) => (
        grammar!(@add_grammar $l, $m, $e, None, Some(1), false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, <$e:ident>*? $($parts:tt)*) => (
        grammar!(@add_grammar $l, $m, $e, Some(0), None, true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, <$e:ident>* $($parts:tt)*) => (
        grammar!(@add_grammar $l, $m, $e, Some(0), None, false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, <$e:ident>+? $($parts:tt)*) => (
        grammar!(@add_grammar $l, $m, $e, Some(1), None, true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, <$e:ident>+ $($parts:tt)*) => (
        grammar!(@add_grammar $l, $m, $e, Some(1), None, false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, <$e:ident>{$low:expr,$high:expr}? $($parts:tt)*) => (
        grammar!(@add_grammar $l, $m, $e, Some($low), Some($high), true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, <$e:ident>{$low:expr,$high:expr} $($parts:tt)*) => (
        grammar!(@add_grammar $l, $m, $e, Some($low), Some($high), false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, <$e:ident>{$low:expr,}? $($parts:tt)*) => (
        grammar!(@add_grammar $l, $m, $e, Some($low), None, true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, <$e:ident>{$low:expr,} $($parts:tt)*) => (
        grammar!(@add_grammar $l, $m, $e, Some($low), None, false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, <$e:ident>{$n:expr} $($parts:tt)*) => (
        grammar!(@add_grammar $l, $m, $e, Some($n), Some($n), false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, <$e:ident> $($parts:tt)*) => (
        grammar!(@add_grammar $l, $m, $e, None, None, false, $($parts)*)
    );

    // general rule for adding a (rule)
    // (string) introduces a single leaf; repetition suffix is optional
    ( @add_string $l:expr, $m:expr, $e:expr, $low:expr, $high:expr, $stingy:expr, $($parts:tt)* ) => (
        grammar!(
            @add_part
            $l,
            $m,
            $crate::macros::Part::S($e.to_string(), $low, $high, $stingy),
            $($parts)*
        )
    );

    ( @rules $l:expr, $m:expr, ($e:expr)?? $($parts:tt)*) => (
        grammar!(@add_string $l, $m, $e, None, Some(1), true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, ($e:expr)? $($parts:tt)*) => (
        grammar!(@add_string $l, $m, $e, None, Some(1), false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, ($e:expr)*? $($parts:tt)*) => (
        grammar!(@add_string $l, $m, $e, Some(0), None, true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, ($e:expr)* $($parts:tt)*) => (
        grammar!(@add_string $l, $m, $e, Some(0), None, false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, ($e:expr)+? $($parts:tt)*) => (
        grammar!(@add_string $l, $m, $e, Some(1), None, true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, ($e:expr)+ $($parts:tt)*) => (
        grammar!(@add_string $l, $m, $e, Some(1), None, false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, ($e:expr){$low:expr,$high:expr}? $($parts:tt)*) => (
        grammar!(@add_string $l, $m, $e, Some($low), Some($high), true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, ($e:expr){$low:expr,$high:expr} $($parts:tt)*) => (
        grammar!(@add_string $l, $m, $e, Some($low), Some($high), false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, ($e:expr){$low:expr,}? $($parts:tt)*) => (
        grammar!(@add_string $l, $m, $e, Some($low), None, true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, ($e:expr){$low:expr,} $($parts:tt)*) => (
        grammar!(@add_string $l, $m, $e, Some($low), None, false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, ($e:expr){$n:expr} $($parts:tt)*) => (
        grammar!(@add_string $l, $m, $e, Some($n), Some($n), false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, ($e:expr) $($parts:tt)*) => (
        grammar!(@add_string $l, $m, $e, None, None, false, $($parts)*)
    );

    // general rule for adding a [rule]
    // [leaves] ingest a list of leaves; repetition suffix is optional
    ( @add_vec $l:expr, $m:expr, $e:expr, $low:expr, $high:expr, $stingy:expr, $($parts:tt)* ) => (
        let terms: &[&str] = $e;
        grammar!(
            @add_part
            $l,
            $m,
            $crate::macros::Part::V(terms.iter().map(|s| s.to_string()).collect(), $low, $high, $stingy),
            $($parts)*
        )
    );

    ( @rules $l:expr, $m:expr, [$e:expr]?? $($parts:tt)*) => (
        grammar!(@add_vec $l, $m, $e, None, Some(1), true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, [$e:expr]? $($parts:tt)*) => (
        grammar!(@add_vec $l, $m, $e, None, Some(1), false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, [$e:expr]*? $($parts:tt)*) => (
        grammar!(@add_vec $l, $m, $e, Some(0), None, true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, [$e:expr]* $($parts:tt)*) => (
        grammar!(@add_vec $l, $m, $e, Some(0), None, false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, [$e:expr]+? $($parts:tt)*) => (
        grammar!(@add_vec $l, $m, $e, Some(1), None, true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, [$e:expr]+ $($parts:tt)*) => (
        grammar!(@add_vec $l, $m, $e, Some(1), None, false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, [$e:expr]{$low:expr,$high:expr}? $($parts:tt)*) => (
        grammar!(@add_vec $l, $m, $e, Some($low), Some($high), true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, [$e:expr]{$low:expr,$high:expr} $($parts:tt)*) => (
        grammar!(@add_vec $l, $m, $e, Some($low), Some($high), false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, [$e:expr]{$low:expr,}? $($parts:tt)*) => (
        grammar!(@add_vec $l, $m, $e, Some($low), None, true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, [$e:expr]{$low:expr,} $($parts:tt)*) => (
        grammar!(@add_vec $l, $m, $e, Some($low), None, false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, [$e:expr]{$n:expr} $($parts:tt)*) => (
        grammar!(@add_vec $l, $m, $e, Some($n), Some($n), false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, [$e:expr] $($parts:tt)*) => (
        grammar!(@add_vec $l, $m, $e, None, None, false, $($parts)*)
    );

    // r(pattern) allows the introduction of an externally generated regex; no repetition suffix
    ( @rules $l:expr, $m:expr, r($e:expr) $($parts:tt)*) => (
        grammar!(@add_part $l, $m, $crate::macros::Part::R($e.to_string()), $($parts)*)
    );

    // | provides an alternate way to define an alternate (other than repeating a rule)
    ( @rules $l:expr, $m:expr, | $($parts:tt)* ) => (
        let flags = $m.get($l.last().unwrap()).unwrap().last().unwrap().0.clone();
        $m.get_mut($l.last().unwrap()).unwrap().push((flags, Vec::new()));
        grammar!(@rules $l, $m, $($parts)*)
    );

    // basement rule that should never consume any token trees
    // if we hit this, either the macro rules don't cover all cases or the
    // conventions of the macro rules have not been followed
    ( @rules $l:expr, $m:expr, $($parts:tt)*) => (
        $(
            panic!("unused token tree: {}", stringify!($parts))
        )*
    );

    // rules that match initially -- they define any flags common to all rules
    ( (?$on:ident) $($rest:tt)+ ) => ({
        let mut mflags = $crate::macros::MacroFlags::defaults();
        mflags.set(stringify!($on), "");
        grammar!(@initialized mflags, $($rest)+ )
    });
    ( (?-$off:ident) $($rest:tt)+ ) => ({
        let mut mflags = $crate::macros::MacroFlags::defaults();
        mflags.set("", stringify!($off));
        grammar!(@initialized mflags, $($rest)+ )
    });
    ( (?$on:ident-$off:ident) $($rest:tt)+ ) => ({
        let mut mflags = $crate::macros::MacroFlags::defaults();
        mflags.set(stringify!($on), stringify!($off));
        grammar!(@initialized mflags, $($rest)+ )
    });
    ( $($rest:tt)+ ) => ({
        let mflags = $crate::macros::MacroFlags::defaults();
        grammar!(@initialized mflags, $($rest)+ )
    });
}

pub fn build_grammar(
    rules: Vec<String>,
    mut names_to_parts: std::collections::HashMap<String, Vec<(MacroFlags, Vec<Part>)>>,
    macro_flags: MacroFlags,
) -> Grammar {
    let base_pidgin = macro_flags.adjust(Pidgin::new());
    let mut compiled: std::collections::HashMap<String, Grammar> =
        std::collections::HashMap::with_capacity(rules.len());
    for rule in rules.iter().rev() {
        let alternates: Vec<(MacroFlags, Vec<Part>)> = names_to_parts.remove(rule).unwrap();
        let mut grammars = vec![];
        for (flags, parts) in alternates {
            let mut pidgin = flags.adjust(base_pidgin.clone());
            let fragments = parts
                .iter()
                .map(|p| match p {
                    Part::R(s) => {
                        let mut p = Pidgin::new();
                        p.foreign_rule("_", &s).expect(&format!("bad rx: {}", &s));
                        let mut g = p.add_str("_").compile();
                        g.clear_name();
                        RuleFragment::G(g)
                    }
                    Part::V(v, low, high, _) => {
                        // FIXME handle boundary stuff
                        let mut g = pidgin
                            .clone()
                            .grammar(&v.iter().map(|s| s.as_str()).collect::<Vec<_>>());
                        if low.is_some() {
                            g = g.reps_min(low.unwrap()).unwrap();
                        }
                        if high.is_some() {
                            g = g.reps_max(high.unwrap()).unwrap();
                        }
                        RuleFragment::G(g)
                    }
                    Part::G(g, low, high, _) => {
                        let mut g = compiled.get(g).unwrap().clone();
                        if low.is_some() {
                            g = g.reps_min(low.unwrap()).unwrap();
                        }
                        if high.is_some() {
                            g = g.reps_max(high.unwrap()).unwrap();
                        }
                        RuleFragment::G(g)
                    }
                    Part::S(s, low, high, _) => {
                        // FIXME handle boundary stuff
                        if !(low.is_some() || high.is_some()) {
                            RuleFragment::S(s.clone())
                        } else {
                            let mut g = pidgin.clone().grammar(&vec![s.as_str()]);
                            if low.is_some() {
                                g = g.reps_min(low.unwrap()).unwrap();
                            }
                            if high.is_some() {
                                g = g.reps_max(high.unwrap()).unwrap();
                            }
                            RuleFragment::G(g)
                        }
                    }
                })
                .collect::<Vec<RuleFragment>>();
            pidgin.build_rule(rule, fragments);
            grammars.push(pidgin.add_str(rule).compile());
        }
        let g = if grammars.len() == 1 {
            grammars.pop().unwrap()
        } else {
            let mut pidgin = base_pidgin.clone();
            for g in grammars {
                pidgin.rule(rule, &g);
            }
            pidgin.add_str(rule).compile()
        };
        compiled.insert(rule.clone(), g);
    }
    let mut g = compiled.remove(rules.first().expect("no rules")).unwrap();
    g.name(rules.first().unwrap());
    g
}

#[derive(Debug)]
#[doc(hidden)]
pub enum Part {
    R(String),
    V(Vec<String>, Option<usize>, Option<usize>, bool),
    G(String, Option<usize>, Option<usize>, bool),
    S(String, Option<usize>, Option<usize>, bool),
}

#[derive(Clone)]
#[doc(hidden)]
pub struct MacroFlags {
    case_insensitive: Option<bool>, //i
    multi_line: Option<bool>,       //m
    unicode: Option<bool>,          //u
    reverse_greed: Option<bool>,    //U
    dot_all: Option<bool>,          //s
    whitespace_some: Option<bool>,  //w
    whitespace_maybe: Option<bool>, //W
    word_left: Option<bool>,        //b
    word_right: Option<bool>,       //B
    pub add_space: bool,
}

impl MacroFlags {
    pub fn defaults() -> MacroFlags {
        MacroFlags {
            case_insensitive: None,
            multi_line: None,
            unicode: None,
            reverse_greed: None,
            dot_all: None,
            whitespace_some: None,
            whitespace_maybe: None,
            word_left: None,
            word_right: None,
            add_space: false,
        }
    }
    // make the pidgin suit the flags
    pub fn adjust(&self, mut p: Pidgin) -> Pidgin {
        if self.case_insensitive.is_some() {
            p = p.case_insensitive(self.case_insensitive.unwrap());
        }
        if self.multi_line.is_some() {
            p = p.multi_line(self.multi_line.unwrap());
        }
        if self.unicode.is_some() {
            p = p.unicode(self.unicode.unwrap());
        }
        if self.reverse_greed.is_some() {
            p = p.reverse_greed(self.reverse_greed.unwrap());
        }
        if self.dot_all.is_some() {
            p = p.dot_all(self.dot_all.unwrap());
        }
        if self.whitespace_some.unwrap_or_default() {
            p = p.normalize_whitespace(true);
        } else if self.whitespace_maybe.unwrap_or_default() {
            p = p.normalize_whitespace(false);
        }
        if self.word_left.unwrap_or_default() {
            p = p.left_word_bound();
        }
        if self.word_right.unwrap_or_default() {
            p = p.right_word_bound();
        }
        p
    }
    pub fn from_strings(on: &str, off: &str) -> MacroFlags {
        let mut mf = MacroFlags::defaults();
        mf.set(on, off);
        mf
    }
    pub fn set(&mut self, on: &str, off: &str) {
        for (s, dir) in vec![(on, true), (off, false)] {
            for c in s.chars() {
                match c {
                    'i' => self.case_insensitive = Some(dir),
                    'm' => self.multi_line = Some(dir),
                    'u' => self.unicode = Some(dir),
                    'U' => self.reverse_greed = Some(dir),
                    's' => self.dot_all = Some(dir),
                    'w' => {
                        if dir {
                            self.whitespace_some = Some(true);
                            self.whitespace_maybe = Some(false);
                        } else {
                            self.whitespace_some = Some(false);
                        }
                    }
                    'W' => {
                        if dir {
                            self.whitespace_some = Some(false);
                            self.whitespace_maybe = Some(true);
                        } else {
                            self.whitespace_maybe = Some(false);
                        }
                    }
                    'b' => self.word_left = Some(dir),
                    'B' => self.word_right = Some(dir),
                    _ => panic!("unfamiliar flag: {}", c),
                }
            }
        }
    }
}

impl std::fmt::Display for MacroFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut on_parts = vec![];
        let mut off_parts = vec![];
        if self.case_insensitive.is_some() {
            (if self.case_insensitive.unwrap() {
                &mut on_parts
            } else {
                &mut off_parts
            })
            .push("i");
        }
        if self.multi_line.is_some() {
            (if self.multi_line.unwrap() {
                &mut on_parts
            } else {
                &mut off_parts
            })
            .push("m");
        }
        if self.dot_all.is_some() {
            (if self.dot_all.unwrap() {
                &mut on_parts
            } else {
                &mut off_parts
            })
            .push("s");
        }
        if self.reverse_greed.is_some() {
            (if self.reverse_greed.unwrap() {
                &mut on_parts
            } else {
                &mut off_parts
            })
            .push("U");
        }
        if self.word_left.is_some() {
            (if self.word_left.unwrap() {
                &mut on_parts
            } else {
                &mut off_parts
            })
            .push("b");
        }
        if self.word_right.is_some() {
            (if self.word_right.unwrap() {
                &mut on_parts
            } else {
                &mut off_parts
            })
            .push("B");
        }
        if self.whitespace_some.is_some() {
            (if self.whitespace_some.unwrap() {
                &mut on_parts
            } else {
                &mut off_parts
            })
            .push("w");
        }
        if self.whitespace_maybe.is_some() {
            (if self.whitespace_maybe.unwrap() {
                &mut on_parts
            } else {
                &mut off_parts
            })
            .push("W");
        }
        if self.add_space {
            on_parts.push("a");
        }
        if self.unicode.is_some() {
            (if self.unicode.unwrap() {
                &mut on_parts
            } else {
                &mut off_parts
            })
            .push("u");
        }
        write!(
            f,
            "(?{}{}{})",
            on_parts.join(""),
            (if off_parts.len() > 0 { "-" } else { "" }),
            off_parts.join("")
        )
    }
}

impl std::fmt::Debug for MacroFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
