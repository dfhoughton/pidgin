#![feature(test)]
#[macro_use]
extern crate pidgin;

#[test]
fn foo_bar() {
    let g = grammar!{
        foo => ("bar")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("bar"), g.describe());
    let p = matcher.parse("bar").unwrap();
    assert_eq!(p.as_str(), "bar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
}

#[test]
fn foo_foo() {
    let g = grammar!{
        foo => ("foo")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foo"), g.describe());
    let p = matcher.parse("foo").unwrap();
    assert_eq!(p.as_str(), "foo");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
}

#[test]
fn foo_foo2() {
    let g = grammar!{
        foo => ("foo"){2}
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foofoo"), g.describe());
    let p = matcher.parse("foofoo").unwrap();
    assert_eq!(p.as_str(), "foofoo");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
}

#[test]
fn foo_maybe_bar() {
    let g = grammar!{
        foo => ("foo")? ("bar")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foobar"), g.describe());
    let p = matcher.parse("foobar").unwrap();
    assert_eq!(p.as_str(), "foobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("bar"), g.describe());
    let p = matcher.parse("bar").unwrap();
    assert_eq!(p.as_str(), "bar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
}

#[test]
fn foo_star_bar() {
    let g = grammar!{
        foo => ("foo")* ("bar")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foofoobar"), g.describe());
    let p = matcher.parse("foofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("foobar"), g.describe());
    let p = matcher.parse("foobar").unwrap();
    assert_eq!(p.as_str(), "foobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("bar"), g.describe());
    let p = matcher.parse("bar").unwrap();
    assert_eq!(p.as_str(), "bar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
}

#[test]
fn foo_plus_bar() {
    let g = grammar!{
        foo => ("foo")+ ("bar")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foofoobar"), g.describe());
    let p = matcher.parse("foofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("foobar"), g.describe());
    let p = matcher.parse("foobar").unwrap();
    assert_eq!(p.as_str(), "foobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(!matcher.is_match("bar"), g.describe());
}

#[test]
fn foo_at_least_2_bar() {
    let g = grammar!{
        foo => ("foo"){2,} ("bar")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foofoobar"), g.describe());
    let p = matcher.parse("foofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("foofoofoobar"), g.describe());
    let p = matcher.parse("foofoofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(!matcher.is_match("foobar"), g.describe());
}

#[test]
fn foo_at_2_to_4_bar() {
    let g = grammar!{
        foo => ("foo"){2,4} ("bar")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("foofoobar"), g.describe());
    let p = matcher.parse("foofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("foofoofoobar"), g.describe());
    let p = matcher.parse("foofoofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("foofoofoofoobar"), g.describe());
    let p = matcher.parse("foofoofoofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoofoofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("foofoofoofoofoobar"), g.describe());
    let p = matcher.parse("foofoofoofoofoobar").unwrap();
    assert_eq!(p.as_str(), "foofoofoofoobar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(!matcher.is_match("foobar"), g.describe());
}

#[test]
fn foo_bar_foo() {
    let g = grammar!{
        foo => ("bar") | ("foo")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("bar"), g.describe());
    let p = matcher.parse("bar").unwrap();
    assert_eq!(p.as_str(), "bar");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
    assert!(matcher.is_match("foo"), g.describe());
    let p = matcher.parse("foo").unwrap();
    assert_eq!(p.as_str(), "foo");
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
}

#[test]
fn rule_specific_flags() {
    let g = grammar!{
        TOP => <foo> <bar>
        foo => (?i) ("foo")
        bar => ("bar")
    };
    let matcher = g.matcher().unwrap();
    let p = matcher.parse("foobar").unwrap();
    assert_eq!(p.as_str(), "foobar");
    assert!(p.name("foo").is_some());
}
