//! Python interpreter detection
//!
//! TODO: Support configurable interpreter path via LSP settings

use std::path::{Path, PathBuf};
use std::process::Command;
use tracing::{debug, warn};

/// Find the Python interpreter to use for introspection
///
/// Searches for Python in the following order:
/// 1. Poetry virtual environment (if pyproject.toml with poetry exists)
/// 2. Pipenv virtual environment (if Pipfile exists)
/// 3. UV virtual environment (if uv.lock exists)
/// 4. System Python (python3 or python in PATH)
pub fn find_python_interpreter(workspace_root: Option<&Path>) -> Option<PathBuf> {
    if let Some(root) = workspace_root {
        if root.join("pyproject.toml").exists() {
            if let Some(path) = try_poetry(root) {
                debug!("Found Python via poetry: {}", path.display());
                return Some(path);
            }
        }

        if root.join("Pipfile").exists() {
            if let Some(path) = try_pipenv(root) {
                debug!("Found Python via pipenv: {}", path.display());
                return Some(path);
            }
        }

        if root.join("uv.lock").exists() || root.join(".venv").exists() {
            if let Some(path) = try_uv(root) {
                debug!("Found Python via uv: {}", path.display());
                return Some(path);
            }
        }
    }

    if let Some(path) = try_system_python() {
        debug!("Found system Python: {}", path.display());
        Some(path)
    } else {
        warn!("No Python interpreter found");
        None
    }
}

/// Try to get Python from Poetry virtual environment
fn try_poetry(workspace_root: &Path) -> Option<PathBuf> {
    let output = Command::new("poetry")
        .arg("env")
        .arg("info")
        .arg("-p")
        .current_dir(workspace_root)
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    let venv_path = String::from_utf8(output.stdout).ok()?;
    let venv_path = venv_path.trim();

    if cfg!(windows) {
        Some(PathBuf::from(venv_path).join("Scripts").join("python.exe"))
    } else {
        Some(PathBuf::from(venv_path).join("bin").join("python"))
    }
}

/// Try to get Python from Pipenv virtual environment
fn try_pipenv(workspace_root: &Path) -> Option<PathBuf> {
    let output = Command::new("pipenv")
        .arg("--venv")
        .current_dir(workspace_root)
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    let venv_path = String::from_utf8(output.stdout).ok()?;
    let venv_path = venv_path.trim();

    if cfg!(windows) {
        Some(PathBuf::from(venv_path).join("Scripts").join("python.exe"))
    } else {
        Some(PathBuf::from(venv_path).join("bin").join("python"))
    }
}

/// Try to get Python from UV virtual environment
fn try_uv(_workspace_root: &Path) -> Option<PathBuf> {
    let output = Command::new("uv").arg("python").arg("find").output().ok()?;
    if output.status.success() {
        let python_path = String::from_utf8(output.stdout).ok()?;
        let python_path = python_path.trim();

        Some(PathBuf::from(python_path))
    } else {
        None
    }
}

/// Try to find system Python using `which` command
fn try_system_python() -> Option<PathBuf> {
    which_command("python")
}

/// Execute `which` command to find executable in PATH
fn which_command(name: &str) -> Option<PathBuf> {
    let which_cmd = if cfg!(windows) { "where" } else { "which" };
    let output = Command::new(which_cmd).arg(name).output().ok()?;

    if output.status.success() {
        let path = String::from_utf8(output.stdout).ok()?;
        let path = path.lines().next()?.trim();

        Some(PathBuf::from(path))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_interpreter_finds_something() {
        if let Some(path) = find_python_interpreter(None) {
            println!("Found Python at: {}", path.display());
        }
    }

    #[test]
    fn test_which_command() {
        let _ = which_command("python3");
        let _ = which_command("python");
        let _ = which_command("nonexistent_command_xyz");
    }
}
