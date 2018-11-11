use crate::grammar::Grammar;
use crate::pidgin::{Pidgin, RuleFragment};
use crate::util::Expression;

/// Compiles a `pidgin::Grammar`. This is likely all that you want.
///
/// # Examples
///
/// ```rust
/// #![recursion_limit = "256"] // if your grammar is big, you may need to bump this limit
/// #[macro_use] extern crate pidgin;
/// let mdays = &(1..=31)
///     .into_iter()
///     .map(|i| i.to_string())
///     .collect::<Vec<_>>();
/// let g = grammar!{
///     (?i)
///     // top rule -- each thing matched against is expected only to be a time expression
///     time -> r(r"\A") <type> r(r"\z")
///
///     // sub-rules
///     type           => <relative> | <absolute>
///     relative       -> <modifier> <unit> | <modifier> <period>
///     period         => <weekday> | <month>
///     absolute       => <month_day> | <day_month_year> | <month_year>
///     absolute       => <month_day_year> | <year>
///     month_day      -> <month> <mday>
///     day_month_year -> <mday> <month> <year>
///     month_year     -> <month> <year>
///     month_day_year -> <month> <mday> (",") <year>
///
///     // leaves
///     mday     => (?bB) [mdays]
///     modifier => (?bB) [["this", "last", "next"]]
///     unit     => (?bB) [["day", "week", "month", "year"]]
///     year     => r(r"\b\d{4}\b")
///     weekday  => (?bB) [[
///                         "sunday",
///                         "monday",
///                         "tuesday",
///                         "wednesday",
///                         "thursday",
///                         "friday",
///                         "saturday"
///                       ]]
///     month    => (?bB) [[
///                         "january",
///                         "february",
///                         "march",
///                         "april",
///                         "may",
///                         "june",
///                         "july",
///                         "august",
///                         "september",
///                         "october",
///                         "november",
///                         "december",
///                       ]]
/// };
/// let matcher = g.matcher().unwrap();
/// assert!(matcher.is_match("May 6, 1969"));
/// assert!(matcher.is_match("May 6"));
/// assert!(matcher.is_match("1969"));
/// assert!(matcher.is_match("last Saturday"));
/// let p = matcher.parse("May 6, 1969").unwrap();
/// assert!(p.name("absolute").is_some());
/// assert!(p.name("month").is_some());
/// ```
///
/// # Conventions
///
/// ## Structure
///
/// ```rust
/// # #[macro_use] extern crate pidgin;
/// grammar!{
///   // optional default flags for all rules
///   (?ims)
///
///   // a master rule that must match
///   TOP => <cat> | <dog>
///
///   // optional sub-rules used by the master rule
///   cat =>
///          (?-i) // optional rule-specific flats
///          [["calico", "tabby"]]
///   dog => [["dachshund", "malamute"]]
/// };
/// ```
/// ## Order
///
/// The macro requires only that you provide a master rule and that the remaining
/// rules all be sortable such that every rule that is referenced is defined
/// and that there is no recursion. Recursion will cause a panic.
///
/// ## Conventions
/// ### Flags
///
/// Flags take the form `(?<on>-<off>)` just as in regular expressions. They may
/// appear before all the rules in the grammar as defaults and after the arrow
/// of a particular rule, in which case they apply to that rule alone. Rules
/// do not inherit the flags of rules they are contained in. In
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// grammar!{
///   foo -> (?i) ("the") <cat>
///   cat => ("cat")
/// };
/// ```
/// the `<cat>` rule is not case-insensitive.
///
/// The flags understood are the standard regex flags minus `x` plus a few
/// peculiar to grammars.
/// #### `b` and `B`
/// The `b` and `B` flags correspond to the `\b` regex anchor, the `b` flag being
/// for the left word boundary anchor and the `B` flag for the right. They only
/// have an effect on `("string literal")` and `[&str_vector]` elements of a
/// rule, and only when these elements are on the left or right margin of their
/// rule.
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// grammar!{
///   foo -> (?bB) ("cat") ("dog") ("donkey")
/// };
/// ```
/// will produce a regex equivalent to `\bcat\s*dog\s*donkey\b`.
/// #### `w` and `W`
/// The `w` and `W` flags, which are mutually incompatible, control the normalization
/// of whitespace in `[vec]` elements. `w` means whitespace means "some whitespace"
/// and `W` means "maybe some whitespace".
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// grammar!{
///   foo => (?w) [["cat a log"]]
/// };
/// ```
/// is equivalent to `cat\s+a\s+log`.
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// grammar!{
///   foo => (?W) [["cat a log"]]
/// };
/// ```
/// is equivalent to `cat\s*a\s*log`.
///
/// As in regular expressions, the order of flags, or repetition of flags, in
/// "on" and "off" parts is irrelevant. `(?bB)` means the same thing as `(?Bb)`, which
/// means the same thing as `(?bbbbbBBBB)`, which means the same thing as `(?bBbB)`.
///
/// ### Identifiers
///
/// Rule names in grammars must be legal rust identifiers: `rule` is good; `7ule` and `r*le` are bad.
///
/// ### Arrows
///
/// Rule names are separated from their definition by one of two arrows.
///
/// #### `=>`
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// grammar!{
///   foo => ("cat") ("dog") // equivalent to the regex catdog
/// };
/// ```
///
/// This is the usual separator.
///
/// #### `->`
///
/// The skinny arrow separator indicates that the elements of the rule may
/// optionally be separated by whitespace.
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// grammar!{
///   foo -> ("cat") ("dog") // equivalent to the regex cat\s*dog
/// };
/// ```
///
/// Repeated elements may also be separated by whitespace.
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// grammar!{
///   foo -> ("cat") ("dog")+ // equivalent to the regex cat(?:\s*dog)+
/// };
/// ```
///
/// Normally the first element of a `->` rule is not preceded by any optional
/// whitespace, but this is not so for repeated elements.
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// grammar!{
///   foo -> ("dog")+ // equivalent to the regex (?:\s*dog)+
/// };
/// ```
///
/// If you combine the skinny arrow with word boundaries, the optional space
/// may become obligatory.
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// grammar!{
///   foo  -> <word>+
///   word => (?bB) [vec!["cat", "dog", "tortoise"]]
/// };
/// ```
///
/// In order for there to be a word boundary between the repeated words in this
/// case, there must be some space.
///
/// ### Elements
///
/// A rule definition, after the optional flags, consists of a sequence of
/// rule elements.
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// let qux = grammar!{
///   something => ("or other")
/// };
/// let illustration = grammar!{
///   foo => <bar> r("baz") [["plugh"]] g(qux)
///   bar => ("baz")
/// };
/// ```
///
/// #### `(literal)`
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// grammar!{
///   foo => ("bar")
/// };
/// ```
///
/// An element delimited by bare parentheses provides a literal. The parentheses
/// must contain an expression that can be converted to a `String` with `to_string`.
/// The value of this string will not be further manipulated. Whitespace characters
/// are preserved as is. This literal will be interpolated into the constructed
/// regular expression with appropriate escaping, so, for instance, `.` will
/// become `\.`. Flags such as `(?i)` may still apply.
///
/// #### `r(regex)`
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// grammar!{
///   foo => r(r"\d+")
/// };
/// ```
///
/// An element delimited by a pair of parentheses proceeded by an `r` provides a regular.
/// expression literal. The parentheses again must contain an expression that can
/// converted to a `String` via `to_string`. Unlike the `(literal)` expression,
/// the characters in the `r(regex)` literal will not be escaped.
///
/// Take care to avoid named captures in `r(regex)` elements, as Rust's `regex`
/// does not allow the repetition of group names. Also, it is possible the name
/// you choose will conflict with those inserted by the macro. These all consist
/// of an `m` followed by an integer.
///
/// #### `<rule>`
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// grammar!{
///   foo => <bar>
///   bar => ("baz")
/// };
/// ```
///
/// An element delimited by angle brackets refers to another rule.
/// The parentheses again must contain an expression that can
/// converted to a `String` via `to_string`. Unlike the `(literal)` expression,
/// the characters in the `r(regex)` literal will not be escaped.
///
/// #### `[vec]`
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// grammar!{
///   foo => [["cat", "dog", "tortoise"]]
/// };
/// ```
///
/// An element delimited by square brackets introduces a list of elements to
/// be condensed into a regular expression. Meta-characters in the items in the
/// list will be escaped, but white space may be normalized. The square brackets
/// must contain a Rust expression to which `.iter().map(|i| i.to_string()).collect()`
/// may be applied to get a `Vec` of `String`s.
///
/// #### `g(grammar)`
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// let sub_grammar = grammar!{
///   foo => ("bar")
/// };
/// grammar!{
///   foo => g(sub_grammar)
/// };
/// ```
///
/// The expression inside a `g(grammar)` element must be a `pidgin::Grammar`
/// or a reference to the same. It will be converted into an owned `Grammar`
/// via `clone`.
///
/// The `g(grammar)` element provides a means to reuse grammars in other grammars.
/// Note that `pidgin::Grammar::rule` provides a mechanism to extract a useful
/// piece of one grammar for re-use in another.
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// let names = grammar!{
///   name       => <east_asian> | <western>
///   east_asian -> <surname> <given_name>
///   western    -> <given_name> <surname>
///   given_name => (?bB) [["Sally", "Wataru", "Jo"]]
///   surname    => (?bB) [["Ng", "Smith", "Salasie"]]
/// };
/// grammar!{
///   foo => g(names.rule("surname").unwrap())
/// };
/// ```
///
/// Nothing ensures a name used in both grammars is used the same way in each:
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// let g1 = grammar!{
///     foo => ("bar")   // here foo is bar
/// };
/// let g2 = grammar!{
///     words -> <word>+
///     word  => <foo> | <bar>
///     foo   => ("baz") // here foo is baz
///     bar   => g(g1)   // but the other foo gets in here
/// };
/// let matcher = g2.matcher().unwrap();
/// let p = matcher.parse("bar baz").unwrap();
/// assert_eq!(
///     vec!["bar", "baz"],
///     p.all_names("foo")
///         .iter()
///         .map(|m| m.as_str())
///         .collect::<Vec<_>>()
/// );
/// ```
///
/// ### Repetition
///
/// All element types *except `r(regex)`* can be followed by a repetition suffix.
/// These are identical to the repetition suffixes allowed by regexes.
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// let g1 = grammar!{
///   foo => ("bar")
/// };
/// grammar!{
///   foo => <r> | <s>? | <v>* | <g>+
///   g   => g(g1){2}   g(g1){3,}   g(g1){4,5}
///   s   => ("foo")??   ("bar")*?   ("baz")+?
///   v   => [["plugh"]]{6,}?   [["quux"]]{7,8}?
///   r   => r(r"no suffix for me*")
/// };
/// ```
///
/// There is no need to add repetition suffixes to `r(regex)` rules, since you
/// can put these in the regex itself. If you absolutely insist, however, you can
/// add a repetition to a reference to the rule.
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// grammar!{
///   foo => <r>+?
///   r   => r(r"no suffix for me*")
/// };
/// ```
///
/// ### Alternation
///
/// Alternation is offering a choice of rules. There are two ways to represent
/// alternation in a `grammar!`:
///
/// ```rust
/// #  #[macro_use] extern crate pidgin;
/// grammar!{
///   foo => ("a") | ("b")
///   foo => ("c")
/// };
/// ```
///
/// Each definition of a rule is an alternate. Also, within one definition one
/// may separate alternates with `|`.
///
/// Unlike in regexes there is no grouping construction in a `grammar!` aside from
/// the definition of rules.
///
/// ## Recursion
///
/// There are two points that bear mentioning regarding recursion and grammars.
///
/// 1. You cannot write a recursive grammar. There is no mechanism in `regex::Regex`
/// which would allow it and the macro could not compile it.
/// 2. The `grammar!` macro works by recursion, nibbling elements off the
/// beginning of the grammar definition and then recursing until all token are
/// consumed. This means if your grammar is large, and not terribly large, it
/// may require a larger stack during compilation than the compiler provides by
/// default. In this case one need only request more space.
///
/// ```rust
/// #![recursion_limit = "256"] // bump this up until your grammar compiles
/// #[macro_use] extern crate pidgin;
/// // ...
/// ```
///
/// ## `lazy_static!`
///
/// As with regexes, the definition of grammars is not something you want to do
/// repeatedly at runtime. The best practice is to compile them once and then
/// reuse them with the [`lazy_static`](https://crates.io/crates/lazy_static) macro.
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

    // general rule for adding a g(&foreign_grammar)
    // repetition suffix is optional
    ( @add_foreign_grammar $l:expr, $m:expr, $e:expr, $low:expr, $high:expr, $stingy:expr, $($parts:tt)* ) => (
        grammar!(
            @add_part
            $l,
            $m,
            $crate::macros::Part::F($e.clone(), $low, $high, $stingy),
            $($parts)*
        )
    );

    ( @rules $l:expr, $m:expr, g($e:expr)?? $($parts:tt)* ) => (
        grammar!(@add_foreign_grammar $l, $m, $e, None, Some(1), true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, g($e:expr)? $($parts:tt)* ) => (
        grammar!(@add_foreign_grammar $l, $m, $e, None, Some(1), false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, g($e:expr)*? $($parts:tt)*) => (
        grammar!(@add_foreign_grammar $l, $m, $e, Some(0), None, true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, g($e:expr)* $($parts:tt)*) => (
        grammar!(@add_foreign_grammar $l, $m, $e, Some(0), None, false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, g($e:expr)+? $($parts:tt)*) => (
        grammar!(@add_foreign_grammar $l, $m, $e, Some(1), None, true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, g($e:expr)+ $($parts:tt)*) => (
        grammar!(@add_foreign_grammar $l, $m, $e, Some(1), None, false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, g($e:expr){$low:expr,$high:expr}? $($parts:tt)*) => (
        grammar!(@add_foreign_grammar $l, $m, $e, Some($low), Some($high), true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, g($e:expr){$low:expr,$high:expr} $($parts:tt)*) => (
        grammar!(@add_foreign_grammar $l, $m, $e, Some($low), Some($high), false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, g($e:expr){$low:expr,}? $($parts:tt)*) => (
        grammar!(@add_foreign_grammar $l, $m, $e, Some($low), None, true, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, g($e:expr){$low:expr,} $($parts:tt)*) => (
        grammar!(@add_foreign_grammar $l, $m, $e, Some($low), None, false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, g($e:expr){$n:expr} $($parts:tt)*) => (
        grammar!(@add_foreign_grammar $l, $m, $e, Some($n), Some($n), false, $($parts)*)
    );
    ( @rules $l:expr, $m:expr, g($e:expr) $($parts:tt)*) => (
        grammar!(@add_foreign_grammar $l, $m, $e, None, None, false, $($parts)*)
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
        grammar!(
            @add_part
            $l,
            $m,
            $crate::macros::Part::V($e.iter().map(|s| s.to_string()).collect(), $low, $high, $stingy),
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
            compile_error!(stringify!(unused token $parts))
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

#[doc(hidden)]
pub fn build_grammar(
    rules: Vec<String>,
    mut names_to_parts: std::collections::HashMap<String, Vec<(MacroFlags, Vec<Part>)>>,
    macro_flags: MacroFlags,
) -> Grammar {
    let rules = sort_rules(rules, &names_to_parts);
    let base_pidgin = macro_flags.adjust(Pidgin::new());
    let mut compiled: std::collections::HashMap<String, Grammar> =
        std::collections::HashMap::with_capacity(rules.len());
    for rule in &rules {
        let alternates: Vec<(MacroFlags, Vec<Part>)> = names_to_parts.remove(rule).unwrap();
        let mut grammars = vec![];
        for (flags, parts) in alternates {
            let mut pidgin = flags.adjust(base_pidgin.clone());
            let last_index = parts.len() - 1;
            let fragments = parts
                .iter()
                .enumerate()
                .map(|(i, p)| match p {
                    Part::R(s) => {
						let mut g = Grammar::rx_rule(s.to_string());
                        if flags.add_space && i != 0 {
                            g = left_pad(g);
                        }
                        RuleFragment::G(g)
                    }
                    Part::V(v, low, high, stingy) => {
                        let mut g = pidgin
                            .clone()
                            .add(&v.iter().map(|s| s.as_str()).collect::<Vec<_>>())
                            .compile_bounded(i == 0, i == last_index, true);
                        if flags.add_space && (i != 0 || low.is_some() || high.is_some()) {
                            g = left_pad(g);
                        }
                        if low.is_some() {
                            g = g.reps_min(low.unwrap()).unwrap();
                        }
                        if high.is_some() {
                            g = g.reps_max(high.unwrap()).unwrap();
                        }
                        if *stingy {
                            g = g.stingy(true);
                        }
                        RuleFragment::G(g)
                    }
                    Part::G(g, low, high, stingy) => {
                        let mut g = compiled.get(g).unwrap().clone();
                        if flags.add_space && (i != 0 || low.is_some() || high.is_some()) {
                            g = left_pad(g);
                        }
                        if low.is_some() {
                            g = g.reps_min(low.unwrap()).unwrap();
                        }
                        if high.is_some() {
                            g = g.reps_max(high.unwrap()).unwrap();
                        }
                        if *stingy {
                            g = g.stingy(true);
                        }
                        RuleFragment::G(g)
                    }
                    Part::F(g, low, high, stingy) => {
                        let mut g = g.clone();
                        if flags.add_space && (i != 0 || low.is_some() || high.is_some()) {
                            g = left_pad(g);
                        }
                        if low.is_some() {
                            g = g.reps_min(low.unwrap()).unwrap();
                        }
                        if high.is_some() {
                            g = g.reps_max(high.unwrap()).unwrap();
                        }
                        if *stingy {
                            g = g.stingy(true);
                        }
                        RuleFragment::G(g)
                    }
                    Part::S(s, low, high, stingy) => {
                        if !(flags.add_space && i != 0 || low.is_some() || high.is_some()) {
                            RuleFragment::S(s.clone())
                        } else {
                            let mut g = pidgin.clone().add(&vec![s.as_str()]).compile_bounded(
                                i == 0,
                                i == last_index,
                                false,
                            );
                            if flags.add_space && (i != 0 || low.is_some() || high.is_some()) {
                                g = left_pad(g);
                            }
                            if low.is_some() {
                                g = g.reps_min(low.unwrap()).unwrap();
                            }
                            if high.is_some() {
                                g = g.reps_max(high.unwrap()).unwrap();
                            }
                            if *stingy {
                                g = g.stingy(true);
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
    let mut g = compiled.remove(rules.last().expect("no rules")).unwrap();
    g.name(rules.last().unwrap());
    g
}

// sort rules from least to most dependent
#[doc(hidden)]
pub fn sort_rules(
    mut rules: Vec<String>,
    names_to_parts: &std::collections::HashMap<String, Vec<(MacroFlags, Vec<Part>)>>,
) -> Vec<String> {
    let top = rules.remove(0);
    let mut sorted = Vec::new();
    let mut handled: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    while rules.len() > 0 {
        let mut added = Vec::new();
        let mut retained = Vec::new();
        'outer: for r in rules {
            if let Some(v) = names_to_parts.get(&r) {
                for (_, v) in v {
                    for p in v {
                        if let Part::G(n, _, _, _) = p {
                            if !handled.contains(n) {
                                retained.push(r);
                                continue 'outer;
                            }
                        }
                    }
                }
                handled.insert(r.clone());
                added.push(r);
            }
        }
        if added.len() == 0 {
            panic!(
                "could not find rules referred to in these rules: {:?}",
                retained
            );
        } else {
            rules = retained;
            for r in added {
                sorted.push(r);
            }
        }
    }
    sorted.push(top);
    sorted
}
// add optional whitespace to the left of an element
#[doc(hidden)]
pub fn left_pad(g: Grammar) -> Grammar {
    Grammar {
        name: None,
        flags: g.flags.clone(),
        stingy: g.stingy,
        lower_limit: None,
        upper_limit: None,
        sequence: vec![
            Expression::Part(String::from(r"\s*"), false),
            Expression::Grammar(g, false),
        ],
    }
}

#[derive(Debug)]
#[doc(hidden)]
pub enum Part {
    R(String),                                          // regex
    V(Vec<String>, Option<usize>, Option<usize>, bool), // vector of strs
    G(String, Option<usize>, Option<usize>, bool),      // rule reference
    F(Grammar, Option<usize>, Option<usize>, bool),     // foreign (external to macro) grammar
    S(String, Option<usize>, Option<usize>, bool),      // string literal
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
    pub(crate) fn adjust(&self, mut p: Pidgin) -> Pidgin {
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
