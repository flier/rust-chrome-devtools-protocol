use serde::Deserialize;

#[derive(Clone, Debug, Deserialize)]
struct Version {
    #[serde(rename = "Browser")]
    pub browser: String,

    #[serde(rename = "Protocol-Version")]
    pub protocol_version: String,

    #[serde(rename = "User-Agent")]
    pub user_agent: String,

    #[serde(rename = "V8-Version")]
    pub v8_version: String,

    #[serde(rename = "WebKit-Version")]
    pub webkit_version: String,

    #[serde(rename = "webSocketDebuggerUrl")]
    pub websocket_debugger_url: String,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct Target {
    pub description: String,
    pub devtools_frontend_url: String,
    pub id: String,
    pub title: String,
    pub r#type: String,
    pub url: String,
    #[serde(rename = "webSocketDebuggerUrl")]
    pub websocket_debugger_url: String,
}
