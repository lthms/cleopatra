use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::ffi::OsString;
use std::fs::{write, create_dir_all};
use std::os::unix::fs::PermissionsExt;
use serde::{Serialize, Deserialize};
use crate::error::{Error, Raise};

#[derive(Serialize, Deserialize)]
pub struct Config {
    /// Path to the directory where the generation processes are defined
    pub generation_processes : PathBuf,
    /// Collection of environment variable to set before calling the generation processes
    #[serde(default)]
    pub env : HashMap<String, OsString>,
}

impl Config {
  pub fn find_project_then<A, K>(k : K) -> Result<A, Error>
  where
      K : FnOnce(&Config) -> Result<A, Error> {
    let path = find_file("cleopatra.toml")?;
    let project = Config::read_from_file(&path)?;
    with_cwd(&project.root(), || with_env(&project.env, || k(&project)))
  }
  
  fn read_from_file(file : &Path) -> Result<Self, Error> {
      let content : String = std::fs::read_to_string(file)
          .map_err(Error::ConfigurationRead)?;
  
      let mut project = toml::from_str::<Config>(content.as_str())
          .map_err(|err| Error::ConfigurationParsing(file.into(), err))?;
  
      let parent = file.parent()
          .or_raise("cleopatra has found a configuration file, but could not guess the path of its directory.")?;
      project.prepare(parent)?;
  
      return Ok(project);
  }
  
  fn prepare(&mut self, root : &Path) -> Result<(), Error> {
      self.env.insert(
          String::from("ROOT"),
          OsString::from(root)
      );
  
      self.env.insert(
          String::from("CLEOPATRA_DIRECTORY"),
          OsString::from(root.join(".cleopatra"))
      );
  
      self.env.insert(
          String::from("CLEOPATRA_GENERATION_PROCESSES"),
          OsString::from(self.generation_processes.clone())
      );
  
      std::env::var_os("PATH")
          .as_ref()
          .map(|val| {
              let mut paths = std::env::split_paths(val).collect::<Vec<_>>();
              paths.push(root.join(".cleopatra/bin/"));
              self.env.insert(
                  String::from("PATH"),
                  std::env::join_paths(paths)
                      .unwrap_or(root.join(".cleopatra/bin/").into())
              )
          });
  
      Ok(())
  }
  
  pub fn root(&self) -> PathBuf {
      self.env["ROOT"].clone().into()
  }
  
  pub fn prepare_workspace(&self) -> Result<(), Error> {
      self.prepare_workspace_io().map_err(Error::InitWorkingDirectory)
  }
  
  fn prepare_workspace_io(&self) -> Result<(), std::io::Error> {
      for (path, content) in FILES.iter() {
          if !path.exists() {
              create_dir_all(path.parent().unwrap())?;
              write(path, content.0)?;
              if content.1 {
                  let mut perms = std::fs::metadata(path)?.permissions();
                  perms.set_mode(0o755);
                  std::fs::set_permissions(path, perms)?;
              }
          }
      }
  
      Ok(())
  }
}

fn find_file(filename : &str) -> Result<PathBuf, Error> {
    let mut cwd : PathBuf = std::env::current_dir()
        .or_raise("Cannot get current directory")?;

    loop {
        let candidate = cwd.join(filename);

        if candidate.exists() {
            return Ok(candidate);
        }

        if !cwd.pop() {
            return Err(Error::ConfigurationNotFound);
        }
    }
}

fn with_cwd<K, A>(target : &Path, k : K) -> Result<A, Error>
where
    K : FnOnce() -> Result<A, Error> {
    let origin : PathBuf = std::env::current_dir()
        .or_raise("Cannot get current directory")?;

    std::env::set_current_dir(target)
        .or_raise(&format!("Could not move to the directory {:?}", target))?;

    let res = k();

    std::env::set_current_dir(origin)
        .or_raise(&format!("Could not return from the directory {:?}", target))?;

    return res;
}

fn with_env<K, A>(env : &HashMap<String, OsString>, k : K) -> Result<A, Error>
where
    K : FnOnce() -> Result<A, Error> {
    let context : HashMap<&String, Option<OsString>> = env
        .iter()
        .map(|(var, val)| {
            let old = std::env::var_os(var);
            std::env::set_var(var, val);
            (var, old)
        })
        .collect();

    let res = k();

    for (var, old) in context {
        match old {
            Some(val) => std::env::set_var(var, val),
            None => std::env::remove_var(var),
        }
    }

    return res;
}

lazy_static! {
    static ref FILES : HashMap<PathBuf, (&'static[u8], bool)> = {
        let mut res = HashMap::new();

        res.insert(
            PathBuf::from(".cleopatra/boot.mk"),
            (&include_bytes!("../boot.mk")[..], false),
        );

        res.insert(
            PathBuf::from(".cleopatra/emacs.d/cleopatra.el"),
            (&include_bytes!("../emacs.d/cleopatra.el")[..], false),
        );

        res.insert(
            PathBuf::from(".cleopatra/emacs.d/cleopatra-gen-proc.el"),
            (&include_bytes!("../emacs.d/cleopatra-gen-proc.el")[..], false),
        );

        res.insert(
            PathBuf::from(".cleopatra/bin/cleopatra-emacs"),
            (&include_bytes!("../bin/cleopatra-emacs")[..], true),
        );

        res.insert(
            PathBuf::from(".cleopatra/bin/cleopatra-update-gitignore"),
            (&include_bytes!("../bin/cleopatra-update-gitignore")[..], true),
        );

        res.insert(
            PathBuf::from(".cleopatra/bin/cleopatra-gen-deps"),
            (&include_bytes!("../bin/cleopatra-gen-deps")[..], true),
        );

        res.insert(
            PathBuf::from(".cleopatra/bin/cleopatra-run-elisp"),
            (&include_bytes!("../bin/cleopatra-run-elisp")[..], true),
        );

        res
    };
}
