use serde::{Deserialize, Serialize};
use serde_json;

#[derive(Serialize, Deserialize, Debug)]
enum Version {
    Version,
}

#[derive(Serialize, Deserialize, Debug)]
struct StructV0 {
    _0: Version,
    field1: f32,
}

#[derive(Serialize, Deserialize, Debug)]
struct StructV1 {
    _1: Version,
    field1: f64,
    field2: String,
}

#[derive(Serialize, Deserialize, Debug)]
struct StructV2 {
    _2: Version,
    field1: f64,
    field2: String,
    field3: (f64, String),
}

impl From<StructV0> for StructV1 {
    fn from(v0: StructV0) -> Self {
        StructV1 {
            _1: Version::Version,
            field1: v0.field1 as f64,
            field2: v0.field1.to_string(),
        }
    }
}

impl From<StructV1> for StructV2 {
    fn from(v1: StructV1) -> Self {
        StructV2 {
            _2: Version::Version,
            field1: v1.field1,
            field2: v1.field2.clone(),
            field3: (v1.field1 + 42.0, v1.field1.to_string()),
        }
    }
}

impl From<StructV0> for StructV2 {
    fn from(v0: StructV0) -> Self {
        StructV1::from(v0).into()
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
enum StructV {
    V0(StructV0),
    V1(StructV1),
    V2(StructV2),
}

fn toV2(v: StructV) -> StructV2 {
    match v {
        StructV::V0(v0) => StructV2::from(v0),
        StructV::V1(v1) => StructV2::from(v1),
        StructV::V2(v2) => v2,
    }
}

// impl<'de> Deserialize<'de> for StructV2 {
//     fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
//     where
//         D: serde::Deserializer<'de>,
//     {
//         let v = StructV2::deserialize(deserializer)?;
//         let v1 = serde_json::to_string_pretty(&v);
//         println!("{v1:?}");

//         Ok(v)

//         // match v {
//         //     StructV::V0(v0) => Ok(StructV2::from(v0)),
//         //     StructV::V1(v1) => Ok(StructV2::from(v1)),
//         //     StructV::V2(v2) => Ok(v2),
//         // }
//     }
// }

fn main() {
    let v0_json = r#"{"_0":"Version","field1":10}"#;
    let v1_json = r#"{"_1":"Version","field1":20.0,"field2":"20"}"#;
    let v2_json = r#"{"_2":"Version","field1":30.0,"field2":"30","field3":[72.0,"30"]}"#;

    let struct_v2_from_v0: StructV0 = serde_json::from_str(v0_json).unwrap();
    println!("StructV2 from V0: {:?}", struct_v2_from_v0);
    let struct_v2_from_v0a = toV2(serde_json::from_str(v0_json).unwrap());
    println!("StructV2 from V0a: {:?}", struct_v2_from_v0a);
    let struct_v2_from_v1: StructV2 = toV2(serde_json::from_str(v1_json).unwrap());
    println!("StructV2 from V1: {:?}", struct_v2_from_v1);

    let struct_v2_from_v2: StructV2 = serde_json::from_str(v2_json).unwrap();
    println!("StructV2 from V2: {:?}", struct_v2_from_v2);
}
