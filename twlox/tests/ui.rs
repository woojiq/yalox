// TODO: Support stderr.
pub struct UiTest {
    stdout: Vec<String>,
    test_file: std::path::PathBuf,
}

impl UiTest {
    pub fn new<P: AsRef<std::path::Path>>(path: P) -> Self {
        let path = path.as_ref();
        let mut test = Self { stdout: Vec::default(), test_file: path.to_path_buf() };
        test.parse_file(path);
        test
    }

    pub fn run(self) {
        let app = assert_cmd::Command::cargo_bin(env!("CARGO_PKG_NAME"))
            .expect("failed to create app")
            .args([&self.test_file])
            .assert();
        let stdout = self.stdout.into_iter().map(|s| s + "\n").collect::<String>();
        app.stdout(stdout).success();
    }

    fn parse_file(&mut self, path: &std::path::Path) {
        let stdout_regex = regex::Regex::new("(?m)// OUT: (.*)$").expect("regex failed to compile");
        let test_content = std::fs::read_to_string(path)
            .unwrap_or_else(|_| panic!("failed to read file: {path:?}"));
        for (_, [expect]) in stdout_regex.captures_iter(&test_content).map(|c| c.extract()) {
            self.expect_stdout(expect);
        }
    }

    fn expect_stdout(&mut self, value: &str) {
        self.stdout.push(value.to_string());
    }
}

include!(concat!(env!("OUT_DIR"), "/codegen_ui_tests.rs"));
