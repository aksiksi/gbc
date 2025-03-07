pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum CartridgeError {
    Title,
    ManufacturerCode,
    LicenseeCode,
    Type,
    RomSize,
    RamSize,
}

impl std::fmt::Display for CartridgeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Title => write!(f, "Invalid title"),
            Self::ManufacturerCode => write!(f, "Invalid manufacturer code"),
            Self::LicenseeCode => write!(f, "Invalid licensee code"),
            Self::Type => write!(f, "Invalid type"),
            Self::RomSize => write!(f, "Invalid ROM size"),
            Self::RamSize => write!(f, "Invalid RAM size"),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    IoError(String),
    Utf8Error(String),
    InvalidValue(String),
    BincodeError(String),
    CartridgeError(CartridgeError),
}

impl std::error::Error for Error {}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::IoError(msg) => write!(f, "I/O error: {}", msg),
            Self::Utf8Error(msg) => write!(f, "UTF8 decoding error: {}", msg),
            Self::InvalidValue(msg) => write!(f, "Invalid value: {}", msg),
            Self::BincodeError(msg) => write!(f, "Bincode error: {}", msg),
            Self::CartridgeError(err) => write!(f, "Cartridge error: {}", err),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::IoError(err.to_string())
    }
}

impl From<std::str::Utf8Error> for Error {
    fn from(err: std::str::Utf8Error) -> Self {
        Self::Utf8Error(err.to_string())
    }
}

impl From<Box<bincode::ErrorKind>> for Error {
    fn from(err: Box<bincode::ErrorKind>) -> Self {
        Self::BincodeError(err.to_string())
    }
}

impl From<CartridgeError> for Error {
    fn from(err: CartridgeError) -> Self {
        Self::CartridgeError(err)
    }
}
