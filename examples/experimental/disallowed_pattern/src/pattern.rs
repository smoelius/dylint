use serde::{Deserialize, Deserializer};

pub struct Pattern<T, U> {
    pub pattern: T,
    pub reason: Option<String>,
    pub predicate: Option<U>,
    pub callback: Option<U>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum PatternEnum {
    Simple(String),
    Complex {
        pattern: String,
        reason: Option<String>,
        predicate: Option<String>,
        callback: Option<String>,
    },
}

impl<'de> Deserialize<'de> for Pattern<String, String> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let enum_ = PatternEnum::deserialize(deserializer)?;
        match enum_ {
            PatternEnum::Simple(pattern) => Ok(Pattern {
                pattern,
                reason: None,
                predicate: None,
                callback: None,
            }),
            PatternEnum::Complex {
                pattern,
                reason,
                predicate,
                callback,
            } => Ok(Pattern {
                pattern,
                reason,
                predicate,
                callback,
            }),
        }
    }
}
