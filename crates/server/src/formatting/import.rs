//! Import statement formatting
//!
//! Handles PEP8-compliant import sorting, grouping, and formatting.
//! Groups imports into: stdlib, third-party, and local imports.

use beacon_parser::AstNode;
use std::cmp::Ordering;

/// Import category for PEP8 grouping
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ImportCategory {
    /// __future__ imports (must appear first)
    Future,
    /// Standard library imports
    StandardLibrary,
    /// Third-party package imports
    ThirdParty,
    /// Local/application imports
    Local,
}

/// Represents a single import statement
#[derive(Debug, Clone)]
pub struct ImportStatement {
    /// The import category (stdlib, third-party, local)
    pub category: ImportCategory,
    /// The module being imported
    pub module: String,
    /// Optional alias for simple imports
    pub alias: Option<String>,
    /// Names imported from the module (for `from` imports)
    pub names: Vec<String>,
    /// Whether this is a `from` import
    pub is_from_import: bool,
    /// Original line number
    pub line: usize,
}

impl PartialEq for ImportStatement {
    fn eq(&self, other: &Self) -> bool {
        self.category == other.category
            && self.module == other.module
            && self.alias == other.alias
            && self.names == other.names
            && self.is_from_import == other.is_from_import
    }
}

impl Eq for ImportStatement {}

impl ImportStatement {
    /// Create import statements from an AST node (splits multi-imports)
    pub fn from_ast_multi(node: &AstNode) -> Vec<Self> {
        match node {
            AstNode::Import { module, alias, extra_modules, line, .. } => {
                let mut statements = Vec::new();
                statements.push(Self {
                    category: categorize_import(module),
                    module: module.clone(),
                    alias: alias.clone(),
                    names: Vec::new(),
                    is_from_import: false,
                    line: *line,
                });
                for (module, alias) in extra_modules {
                    statements.push(Self {
                        category: categorize_import(module),
                        module: module.clone(),
                        alias: alias.clone(),
                        names: Vec::new(),
                        is_from_import: false,
                        line: *line,
                    });
                }
                statements
            }
            AstNode::ImportFrom { module, names, line, .. } => vec![Self {
                category: categorize_import(module),
                module: module.clone(),
                alias: None,
                names: names.iter().map(|n| n.name.clone()).collect(),
                is_from_import: true,
                line: *line,
            }],
            _ => Vec::new(),
        }
    }

    /// Create a single import statement from an AST node
    pub fn from_ast(node: &AstNode) -> Option<Self> {
        Self::from_ast_multi(node).into_iter().next()
    }

    /// Format the import statement as a string
    pub fn format(&self, max_line_length: usize) -> String {
        if self.is_from_import {
            self.format_from_import(max_line_length)
        } else {
            self.format_simple_import()
        }
    }

    fn format_simple_import(&self) -> String {
        match &self.alias {
            Some(alias) => format!("import {} as {}", self.module, alias),
            None => format!("import {}", self.module),
        }
    }

    fn format_from_import(&self, max_line_length: usize) -> String {
        let mut sorted_names = self.names.clone();
        sorted_names.sort();

        let single_line = format!("from {} import {}", self.module, sorted_names.join(", "));

        if single_line.len() <= max_line_length {
            single_line
        } else {
            let mut result = format!("from {} import (\n", self.module);
            for (i, name) in sorted_names.iter().enumerate() {
                result.push_str(&format!("    {name}"));
                if i < sorted_names.len() - 1 {
                    result.push(',');
                }
                result.push('\n');
            }
            result.push(')');
            result
        }
    }

    /// Get sort key for alphabetical ordering within groups
    fn sort_key(&self) -> String {
        if self.is_from_import {
            format!("1_{}", self.module.to_lowercase())
        } else {
            format!("0_{}", self.module.to_lowercase())
        }
    }
}

impl PartialOrd for ImportStatement {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ImportStatement {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.category.cmp(&other.category) {
            Ordering::Equal => self.sort_key().cmp(&other.sort_key()),
            other_ordering => other_ordering,
        }
    }
}

/// Categorize an import module as stdlib, third-party, or local
pub(crate) fn categorize_import(module: &str) -> ImportCategory {
    let base_module = module.split('.').next().unwrap_or(module);

    if base_module == "__future__" {
        return ImportCategory::Future;
    }

    if module.starts_with('.') || module.starts_with("..") {
        return ImportCategory::Local;
    }

    if is_stdlib_module(base_module) {
        ImportCategory::StandardLibrary
    } else {
        ImportCategory::ThirdParty
    }
}

/// Check if a module is a known standard library module
fn is_stdlib_module(module: &str) -> bool {
    matches!(
        module,
        "abc"
            | "aifc"
            | "argparse"
            | "array"
            | "ast"
            | "asyncio"
            | "atexit"
            | "base64"
            | "bdb"
            | "binascii"
            | "bisect"
            | "builtins"
            | "bz2"
            | "calendar"
            | "cgi"
            | "cgitb"
            | "chunk"
            | "cmath"
            | "cmd"
            | "code"
            | "codecs"
            | "codeop"
            | "collections"
            | "colorsys"
            | "compileall"
            | "concurrent"
            | "configparser"
            | "contextlib"
            | "contextvars"
            | "copy"
            | "copyreg"
            | "cProfile"
            | "crypt"
            | "csv"
            | "ctypes"
            | "curses"
            | "dataclasses"
            | "datetime"
            | "dbm"
            | "decimal"
            | "difflib"
            | "dis"
            | "distutils"
            | "doctest"
            | "email"
            | "encodings"
            | "enum"
            | "errno"
            | "faulthandler"
            | "fcntl"
            | "filecmp"
            | "fileinput"
            | "fnmatch"
            | "formatter"
            | "fractions"
            | "ftplib"
            | "functools"
            | "gc"
            | "getopt"
            | "getpass"
            | "gettext"
            | "glob"
            | "graphlib"
            | "grp"
            | "gzip"
            | "hashlib"
            | "heapq"
            | "hmac"
            | "html"
            | "http"
            | "imaplib"
            | "imghdr"
            | "imp"
            | "importlib"
            | "inspect"
            | "io"
            | "ipaddress"
            | "itertools"
            | "json"
            | "keyword"
            | "lib2to3"
            | "linecache"
            | "locale"
            | "logging"
            | "lzma"
            | "mailbox"
            | "mailcap"
            | "marshal"
            | "math"
            | "mimetypes"
            | "mmap"
            | "modulefinder"
            | "multiprocessing"
            | "netrc"
            | "nis"
            | "nntplib"
            | "numbers"
            | "operator"
            | "optparse"
            | "os"
            | "ossaudiodev"
            | "pathlib"
            | "pdb"
            | "pickle"
            | "pickletools"
            | "pipes"
            | "pkgutil"
            | "platform"
            | "plistlib"
            | "poplib"
            | "posix"
            | "posixpath"
            | "pprint"
            | "profile"
            | "pstats"
            | "pty"
            | "pwd"
            | "py_compile"
            | "pyclbr"
            | "pydoc"
            | "queue"
            | "quopri"
            | "random"
            | "re"
            | "readline"
            | "reprlib"
            | "resource"
            | "rlcompleter"
            | "runpy"
            | "sched"
            | "secrets"
            | "select"
            | "selectors"
            | "shelve"
            | "shlex"
            | "shutil"
            | "signal"
            | "site"
            | "smtpd"
            | "smtplib"
            | "sndhdr"
            | "socket"
            | "socketserver"
            | "spwd"
            | "sqlite3"
            | "ssl"
            | "stat"
            | "statistics"
            | "string"
            | "stringprep"
            | "struct"
            | "subprocess"
            | "sunau"
            | "symbol"
            | "symtable"
            | "sys"
            | "sysconfig"
            | "syslog"
            | "tabnanny"
            | "tarfile"
            | "telnetlib"
            | "tempfile"
            | "termios"
            | "test"
            | "textwrap"
            | "threading"
            | "time"
            | "timeit"
            | "tkinter"
            | "token"
            | "tokenize"
            | "tomllib"
            | "trace"
            | "traceback"
            | "tracemalloc"
            | "tty"
            | "turtle"
            | "turtledemo"
            | "types"
            | "typing"
            | "typing_extensions"
            | "unicodedata"
            | "unittest"
            | "urllib"
            | "uu"
            | "uuid"
            | "venv"
            | "warnings"
            | "wave"
            | "weakref"
            | "webbrowser"
            | "winreg"
            | "winsound"
            | "wsgiref"
            | "xdrlib"
            | "xml"
            | "xmlrpc"
            | "zipapp"
            | "zipfile"
            | "zipimport"
            | "zlib"
            | "zoneinfo"
    )
}

/// Group and sort imports according to PEP8
pub struct ImportSorter {
    imports: Vec<ImportStatement>,
}

impl ImportSorter {
    /// Create a new import sorter
    pub fn new() -> Self {
        Self { imports: Vec::new() }
    }

    /// Add an import from an AST node
    pub fn add_import(&mut self, node: &AstNode) {
        for import in ImportStatement::from_ast_multi(node) {
            self.imports.push(import);
        }
    }

    /// Sort and group imports, returning formatted output
    pub fn format(&mut self, max_line_length: usize) -> String {
        self.imports.sort();
        self.imports.dedup();

        let mut output = String::new();
        let mut current_category = None;

        for import in &self.imports {
            if let Some(prev_category) = current_category
                && prev_category != import.category
            {
                output.push('\n');
            }

            output.push_str(&import.format(max_line_length));
            output.push('\n');

            current_category = Some(import.category);
        }

        output
    }

    /// Get the imports grouped by category
    pub fn grouped_imports(&self) -> (Vec<&ImportStatement>, Vec<&ImportStatement>, Vec<&ImportStatement>) {
        let stdlib: Vec<_> = self
            .imports
            .iter()
            .filter(|i| matches!(i.category, ImportCategory::Future | ImportCategory::StandardLibrary))
            .collect();

        let third_party: Vec<_> = self
            .imports
            .iter()
            .filter(|i| i.category == ImportCategory::ThirdParty)
            .collect();

        let local: Vec<_> = self
            .imports
            .iter()
            .filter(|i| i.category == ImportCategory::Local)
            .collect();

        (stdlib, third_party, local)
    }
}

impl Default for ImportSorter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_categorize_stdlib() {
        assert_eq!(categorize_import("__future__"), ImportCategory::Future);
        assert_eq!(categorize_import("os"), ImportCategory::StandardLibrary);
        assert_eq!(categorize_import("sys"), ImportCategory::StandardLibrary);
        assert_eq!(categorize_import("json"), ImportCategory::StandardLibrary);
        assert_eq!(categorize_import("pathlib"), ImportCategory::StandardLibrary);
    }

    #[test]
    fn test_categorize_third_party() {
        assert_eq!(categorize_import("numpy"), ImportCategory::ThirdParty);
        assert_eq!(categorize_import("django"), ImportCategory::ThirdParty);
        assert_eq!(categorize_import("requests"), ImportCategory::ThirdParty);
    }

    #[test]
    fn test_categorize_local() {
        assert_eq!(categorize_import(".models"), ImportCategory::Local);
        assert_eq!(categorize_import("..utils"), ImportCategory::Local);
    }

    #[test]
    fn test_import_from_ast_simple() {
        let node = AstNode::Import {
            module: "os".to_string(),
            alias: None,
            extra_modules: Vec::new(),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 9,
        };
        let import = ImportStatement::from_ast(&node).unwrap();
        assert_eq!(import.module, "os");
        assert_eq!(import.alias, None);
        assert!(!import.is_from_import);
        assert_eq!(import.category, ImportCategory::StandardLibrary);
    }

    #[test]
    fn test_import_from_ast_with_alias() {
        let node = AstNode::Import {
            module: "numpy".to_string(),
            alias: Some("np".to_string()),
            extra_modules: Vec::new(),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 16,
        };

        let import = ImportStatement::from_ast(&node).unwrap();
        assert_eq!(import.module, "numpy");
        assert_eq!(import.alias, Some("np".to_string()));
    }

    #[test]
    fn test_import_from_ast_from_import() {
        let node = AstNode::ImportFrom {
            module: "os".to_string(),
            names: vec![
                beacon_parser::ImportName { name: "path".to_string(), line: 1, col: 0, end_line: 1, end_col: 4 },
                beacon_parser::ImportName { name: "environ".to_string(), line: 1, col: 6, end_line: 1, end_col: 13 },
            ],
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 30,
        };

        let import = ImportStatement::from_ast(&node).unwrap();
        assert_eq!(import.module, "os");
        assert!(import.is_from_import);
        assert_eq!(import.names.len(), 2);
        assert!(import.names.contains(&"path".to_string()));
    }

    #[test]
    fn test_format_simple_import() {
        let import = ImportStatement {
            category: ImportCategory::StandardLibrary,
            module: "os".to_string(),
            alias: None,
            names: Vec::new(),
            is_from_import: false,
            line: 1,
        };
        assert_eq!(import.format(88), "import os");
    }

    #[test]
    fn test_format_import_with_alias() {
        let import = ImportStatement {
            category: ImportCategory::ThirdParty,
            module: "numpy".to_string(),
            alias: Some("np".to_string()),
            names: Vec::new(),
            is_from_import: false,
            line: 1,
        };
        assert_eq!(import.format(88), "import numpy as np");
    }

    #[test]
    fn test_format_from_import_single_line() {
        let import = ImportStatement {
            category: ImportCategory::StandardLibrary,
            module: "os".to_string(),
            alias: None,
            names: vec!["path".to_string(), "environ".to_string()],
            is_from_import: true,
            line: 1,
        };
        assert_eq!(import.format(88), "from os import environ, path");
    }

    #[test]
    fn test_format_from_import_multi_line() {
        let import = ImportStatement {
            category: ImportCategory::StandardLibrary,
            module: "collections".to_string(),
            alias: None,
            names: vec![
                "OrderedDict".to_string(),
                "defaultdict".to_string(),
                "Counter".to_string(),
                "namedtuple".to_string(),
            ],
            is_from_import: true,
            line: 1,
        };

        let formatted = import.format(40);
        assert!(formatted.contains("(\n"));
        assert!(formatted.contains("    Counter"));
        assert!(formatted.contains("    OrderedDict"));
    }

    #[test]
    fn test_import_sorting() {
        let mut import1 = ImportStatement {
            category: ImportCategory::ThirdParty,
            module: "numpy".to_string(),
            alias: None,
            names: Vec::new(),
            is_from_import: false,
            line: 1,
        };

        let import2 = ImportStatement {
            category: ImportCategory::StandardLibrary,
            module: "os".to_string(),
            alias: None,
            names: Vec::new(),
            is_from_import: false,
            line: 2,
        };

        assert!(import2 < import1);

        let import3 = ImportStatement {
            category: ImportCategory::StandardLibrary,
            module: "sys".to_string(),
            alias: None,
            names: Vec::new(),
            is_from_import: false,
            line: 3,
        };

        assert!(import2 < import3);

        import1.category = ImportCategory::StandardLibrary;
        import1.module = "ast".to_string();

        assert!(import1 < import2);
    }

    #[test]
    fn test_import_sorter() {
        let mut sorter = ImportSorter::new();

        sorter.add_import(&AstNode::Import {
            module: "numpy".to_string(),
            alias: Some("np".to_string()),
            extra_modules: Vec::new(),
            line: 3,
            col: 0,
            end_line: 3,
            end_col: 16,
        });

        sorter.add_import(&AstNode::Import {
            module: "os".to_string(),
            alias: None,
            extra_modules: Vec::new(),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 9,
        });

        sorter.add_import(&AstNode::ImportFrom {
            module: "sys".to_string(),
            names: vec![beacon_parser::ImportName {
                name: "argv".to_string(),
                line: 2,
                col: 0,
                end_line: 2,
                end_col: 4,
            }],
            line: 2,
            col: 0,
            end_line: 2,
            end_col: 20,
        });

        let formatted = sorter.format(88);

        assert!(formatted.find("import os").unwrap() < formatted.find("from sys").unwrap());
        assert!(formatted.find("from sys").unwrap() < formatted.find("import numpy").unwrap());
        assert!(formatted.contains("\n\nimport numpy"));
    }

    #[test]
    fn test_grouped_imports() {
        let mut sorter = ImportSorter::new();

        sorter.add_import(&AstNode::Import {
            module: "os".to_string(),
            alias: None,
            extra_modules: Vec::new(),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 9,
        });

        sorter.add_import(&AstNode::Import {
            module: "numpy".to_string(),
            alias: None,
            extra_modules: Vec::new(),
            line: 2,
            col: 0,
            end_line: 2,
            end_col: 12,
        });

        let (stdlib, third_party, local) = sorter.grouped_imports();

        assert_eq!(stdlib.len(), 1);
        assert_eq!(third_party.len(), 1);
        assert_eq!(local.len(), 0);
    }

    #[test]
    fn test_duplicate_removal() {
        let mut sorter = ImportSorter::new();

        sorter.add_import(&AstNode::Import {
            module: "os".to_string(),
            alias: None,
            extra_modules: Vec::new(),
            line: 1,
            col: 0,
            end_line: 1,
            end_col: 9,
        });

        sorter.add_import(&AstNode::Import {
            module: "os".to_string(),
            alias: None,
            extra_modules: Vec::new(),
            line: 5,
            col: 0,
            end_line: 5,
            end_col: 9,
        });

        let formatted = sorter.format(88);
        assert_eq!(formatted.matches("import os").count(), 1);
    }
}
