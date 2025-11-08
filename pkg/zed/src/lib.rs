use zed_extension_api as zed;

struct Beacon;

impl zed::Extension for Beacon {
    fn new() -> Self
    where
        Self: Sized,
    {
        Self {}
    }

    fn language_server_command(
        &mut self, _: &zed::LanguageServerId, worktree: &zed::Worktree,
    ) -> zed::Result<zed::Command> {
        let command = get_path_to_language_server_executable(worktree)?;
        let args = get_args_for_language_server()?;
        let env = get_env_for_language_server()?;

        Ok(zed::Command { command, args, env })
    }
}

/// Provides environment variables for the language server.
/// Beacon LSP uses RUST_LOG for logging configuration.
fn get_env_for_language_server() -> zed::Result<Vec<(String, String)>> {
    Ok(vec![("RUST_LOG".to_string(), "info".to_string())])
}

/// Provides command-line arguments for the language server.
fn get_args_for_language_server() -> zed::Result<Vec<String>> {
    Ok(vec![])
}

/// Gets the path to the beacon-lsp executable.
/// First checks if beacon-lsp is available in the system PATH.
fn get_path_to_language_server_executable(worktree: &zed::Worktree) -> zed::Result<String> {
    worktree
        .which("beacon-lsp")
        .ok_or_else(|| "beacon-lsp not found in PATH. Please install beacon-lsp first.".to_string())
}

zed::register_extension!(Beacon);
