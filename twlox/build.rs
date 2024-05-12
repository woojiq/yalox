use std::path::PathBuf;

fn main() {
    generate_ui_tests();
}

// Generates tests from ".lox" files. We need to use build script to have all
// benefits from "cargo test".
fn generate_ui_tests() {
    let gen_path = PathBuf::from(&std::env::var_os("OUT_DIR").unwrap()).join("codegen_ui_tests.rs");
    let tests_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests").join("ui");

    println!("cargo:rerun-if-changed={}", tests_dir.to_str().unwrap());

    let tests = walk_test_dirs(tests_dir);
    std::fs::write(gen_path, tests).unwrap();
}

fn walk_test_dirs(cur_path: PathBuf) -> String {
    let mut module = String::new();
    module += "mod test {\n";

    for entry in std::fs::read_dir(cur_path).unwrap() {
        let entry = entry.unwrap().path();
        if entry.is_file() {
            module += &generate_test_from_file(entry);
        } else if entry.is_dir() {
            module += &walk_test_dirs(entry);
        }
    }

    module += "}\n";
    module
}

fn generate_test_from_file(path: PathBuf) -> String {
    let test_name = path.file_stem().unwrap().to_str().unwrap();
    let path_str = path.to_str().unwrap();

    let mut test = String::new();
    test += &format!("#[test]\nfn {test_name}() {{\n");
    test += &format!("super::UiTest::new(\"{path_str}\").run();");
    test += "\n}\n";
    test
}
