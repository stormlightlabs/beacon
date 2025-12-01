//! Integration tests for TCP mode of the LSP server

use beacon_lsp::run_server_tcp;
use serde_json::{Value, json};
use std::io::{BufRead, BufReader, Read, Write};
use std::net::TcpStream;
use std::time::Duration;
use tokio::time::sleep;

/// Helper to create LSP-formatted JSON-RPC messages
fn create_lsp_message(method: &str, params: Value, id: i64) -> String {
    let message_json = json!({
        "jsonrpc": "2.0",
        "id": id,
        "method": method,
        "params": params
    });

    let content = message_json.to_string();
    format!("Content-Length: {}\r\n\r\n{}", content.len(), content)
}

/// Helper to parse LSP response from TCP stream
fn read_lsp_response(stream: &mut BufReader<&TcpStream>) -> Result<Value, Box<dyn std::error::Error>> {
    let mut content_length = 0;
    for line in stream.lines() {
        let line = line?;
        if line.is_empty() {
            break;
        }
        if line.starts_with("Content-Length:") {
            content_length = line.split(':').nth(1).unwrap().trim().parse()?;
        }
    }

    let mut buffer = vec![0u8; content_length];
    stream.read_exact(&mut buffer)?;

    let response: Value = serde_json::from_slice(&buffer)?;
    Ok(response)
}

#[tokio::test(flavor = "multi_thread")]
async fn test_tcp_server_binds_to_address() {
    let port = 19350;

    let server_handle = tokio::spawn(async move {
        let _ = run_server_tcp("127.0.0.1", port).await;
    });

    sleep(Duration::from_millis(200)).await;

    let connect_result = tokio::time::timeout(
        Duration::from_secs(2),
        tokio::task::spawn_blocking(move || TcpStream::connect(format!("127.0.0.1:{}", port))),
    )
    .await;

    assert!(connect_result.is_ok(), "Connect should not timeout");
    assert!(
        connect_result.unwrap().unwrap().is_ok(),
        "Should be able to connect to TCP server"
    );

    server_handle.abort();
}

#[tokio::test(flavor = "multi_thread")]
async fn test_tcp_server_accepts_connection() {
    let port = 19351;

    let server_handle = tokio::spawn(async move {
        let _ = run_server_tcp("127.0.0.1", port).await;
    });

    sleep(Duration::from_millis(200)).await;

    let test_result = tokio::time::timeout(Duration::from_secs(5), async {
        let mut stream = TcpStream::connect(format!("127.0.0.1:{}", port)).expect("Should connect");
        stream.set_read_timeout(Some(Duration::from_secs(2))).ok();
        stream.set_write_timeout(Some(Duration::from_secs(2))).ok();

        let initialize_params = json!({
            "processId": null,
            "rootUri": null,
            "capabilities": {}
        });

        let message = create_lsp_message("initialize", initialize_params, 1);
        stream.write_all(message.as_bytes()).expect("Should write");

        let mut reader = BufReader::new(&stream);
        let response = read_lsp_response(&mut reader).expect("Should read response");

        assert!(response.get("result").is_some(), "Should receive initialize result");
    })
    .await;

    assert!(test_result.is_ok(), "Test should complete within timeout");

    server_handle.abort();
}

#[tokio::test(flavor = "multi_thread")]
async fn test_tcp_server_multiple_sequential_connections() {
    let port = 19352;

    let server_handle = tokio::spawn(async move {
        let _ = run_server_tcp("127.0.0.1", port).await;
    });

    sleep(Duration::from_millis(200)).await;

    let test_result = tokio::time::timeout(Duration::from_secs(5), async {
        {
            let stream = TcpStream::connect(format!("127.0.0.1:{}", port)).expect("First connection should succeed");
            drop(stream);
        }

        sleep(Duration::from_millis(100)).await;

        {
            let stream = TcpStream::connect(format!("127.0.0.1:{}", port)).expect("Second connection should succeed");
            drop(stream);
        }
    })
    .await;

    assert!(test_result.is_ok(), "Sequential connections should work");

    server_handle.abort();
}

#[test]
fn test_tcp_server_invalid_host() {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let result = rt.block_on(async { run_server_tcp("999.999.999.999", 9350).await });

    assert!(result.is_err(), "Should fail with invalid host address");
}

#[tokio::test(flavor = "multi_thread")]
async fn test_tcp_server_port_in_use() {
    let port = 19353;

    let server_handle = tokio::spawn(async move {
        let _ = run_server_tcp("127.0.0.1", port).await;
    });

    sleep(Duration::from_millis(200)).await;

    let result = tokio::time::timeout(Duration::from_secs(2), run_server_tcp("127.0.0.1", port)).await;

    assert!(
        result.is_err() || result.unwrap().is_err(),
        "Should fail when port is already in use"
    );

    server_handle.abort();
}
