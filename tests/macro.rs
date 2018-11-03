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
    assert!(p.name("foo").is_some());
    assert_eq!(p.all_names("foo").len(), 1, "simple parse tree");
}

#[test]
fn foo_bar_foo() {
    let g = grammar!{
        foo => ("bar") | ("foo")
    };
    let matcher = g.matcher().unwrap();
    assert!(matcher.is_match("bar"), g.describe());
    assert!(matcher.is_match("foo"), g.describe());
}
