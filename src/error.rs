use std::path::PathBuf;

#[derive(Debug)]
pub struct Message {
    /// A quick summary of the error
    pub title : String,
    /// A longer message which supposedly provides a solution for the user
    pub description : String,
}

#[derive(Debug)]
pub enum Error {
    /// There is a parsing error inside a configuration file
    ConfigurationParsing(PathBuf, toml::de::Error),
    /// There is no configuration file neither in current directory nor in any of its parents
    ConfigurationNotFound,
    /// There is a configuration file, but cleopatra cannot read it
    ConfigurationRead(std::io::Error),
    /// Cleopatra could not set-up its working directory
    InitWorkingDirectory(std::io::Error),
    /// Cleopatra has been called with an incorrect subcommand
    UnknownSubcommand(String),
    /// Something else happened, that was not supposed to
    Anomaly(String),
}

impl Error {
    pub fn message(self) -> Message {
        match self {
            Error::ConfigurationParsing(path, err) =>
                Message {
                    title : format!("{:?} is not a valid cleopatra.toml file", path),
                    description : format!("The following error was found: {}.", err)
                },
            Error::ConfigurationNotFound =>
                Message {
                    title : "cleopatra could not file its configuration file".into(),
                    description : "You need to add a valid `cleopatra.toml' file at the root of your project.".into()
                },
            Error::ConfigurationRead(io) =>
                Message {
                    title : "cleopatra could not read its configuration file".into(),
                    description : format!("Its attempt failed with the following error: {}.", io)
                },
            Error::InitWorkingDirectory(io) =>
                Message {
                    title : "cleopatra could not set-up its working directory".into(),
                    description : format!("The filesystem reported the following error: {}", io)
                },
            Error::UnknownSubcommand(cmd) =>
                Message {
                    title : format!("cleopatra has been called with a unsupported subcommand"),
                    description : format!(r#"`{}' is not a valid subcommand of cleopatra.
             You can use `cleopatra --help' to get the list of supported subcommands."#, cmd)
                },
            Error::Anomaly(msg) =>
                Message {
                    title : String::from("An anomaly occured, you probably have found a bug."),
                    description : msg
                },
        }
    }
}

pub trait Raise {
    type In;
    fn or_raise(self, msg : &str) -> Result<Self::In, Error>;
}

impl<T> Raise for Option<T> {
    type In = T;

    fn or_raise(self, msg : &str) -> Result<T, Error> {
        self.ok_or(Error::Anomaly(String::from(msg)))
    }
}

impl<T, E> Raise for Result<T, E> {
    type In = T;

    fn or_raise(self, msg : &str) -> Result<T, Error> {
        self.map_err(|_| Error::Anomaly(String::from(msg)))
    }
}
