#[cfg(feature = "client")]
use serde::Deserialize;
#[cfg(feature = "server")]
use serde::Serialize;

#[cfg_attr(feature = "server", derive(Serialize))]
#[cfg_attr(feature = "client", derive(Deserialize))]
#[derive(Clone, Debug)]
pub struct Version {
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

#[cfg_attr(feature = "server", derive(Serialize))]
#[cfg_attr(feature = "client", derive(Deserialize))]
#[derive(Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct Target {
    pub description: String,
    pub devtools_frontend_url: String,
    pub id: String,
    pub title: String,
    pub r#type: String,
    pub url: String,
    #[serde(rename = "webSocketDebuggerUrl")]
    pub websocket_debugger_url: String,
}
