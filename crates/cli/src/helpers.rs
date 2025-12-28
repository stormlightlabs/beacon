use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

/// Discovers Python files from input paths.
///
/// Accepts individual files, multiple files, or directories (recursively finds all .py files while respecting .gitignore).
pub fn discover_python_files(paths: &[PathBuf]) -> Result<Vec<PathBuf>> {
    let mut python_files = Vec::new();

    for path in paths {
        if !path.exists() {
            anyhow::bail!("Path does not exist: {}", path.display());
        }

        if path.is_file() {
            if is_python_file(path) {
                python_files.push(path.canonicalize()?);
            } else {
                anyhow::bail!("Not a Python file: {}", path.display());
            }
        } else if path.is_dir() {
            let discovered = discover_files_in_directory(path)?;
            python_files.extend(discovered);
        }
    }

    if python_files.is_empty() {
        anyhow::bail!("No Python files found");
    }

    Ok(python_files)
}

/// Checks if a file has a .py extension
fn is_python_file(path: &Path) -> bool {
    path.extension().is_some_and(|ext| ext == "py")
}

/// Discovers all Python files in a directory, respecting .gitignore
fn discover_files_in_directory(root: &Path) -> Result<Vec<PathBuf>> {
    let mut python_files = Vec::new();

    let mut builder = ignore::WalkBuilder::new(root);
    builder
        .hidden(false)
        .git_ignore(true)
        .git_global(false)
        .git_exclude(false);

    let exclude_patterns = vec![
        "**/__pycache__/",
        "**/*.pyc",
        "**/.pytest_cache/",
        "**/.mypy_cache/",
        "**/.ruff_cache/",
        "**/venv/",
        "**/.venv/",
        "**/env/",
        "**/.env/",
    ];

    for pattern in exclude_patterns {
        builder.add_ignore(pattern);
    }

    for entry in builder.build() {
        let entry = entry.with_context(|| "Failed to read directory entry")?;
        let path = entry.path();

        if path.is_file() && is_python_file(path) {
            python_files.push(path.canonicalize()?);
        }
    }

    python_files.sort();
    Ok(python_files)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use tempfile::TempDir;

    #[test]
    fn test_discover_python_files_relative_path() -> Result<()> {
        let temp_dir = TempDir::new()?;
        let file_path = temp_dir.path().join("test.py");
        File::create(&file_path)?;

        let current_dir = std::env::current_dir()?;
        let relative_path = pathdiff::diff_paths(temp_dir.path(), &current_dir)
            .ok_or_else(|| anyhow::anyhow!("Failed to get relative path"))?;

        let discovered = discover_python_files(&[relative_path])?;

        assert_eq!(discovered.len(), 1);
        assert!(discovered[0].is_absolute());
        assert_eq!(discovered[0], file_path.canonicalize()?);

        Ok(())
    }
}
