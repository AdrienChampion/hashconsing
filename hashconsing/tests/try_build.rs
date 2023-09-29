//! Try-build tests.

#[test]
fn issues() {
    let t = trybuild::TestCases::new();

    t.compile_fail("tests/try_build/issue_1.rs");
}
