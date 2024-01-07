#[macro_use] extern crate clap;
#[macro_use] extern crate lazy_static;

use std::process::Command;
use colored::*;
use clap::{ArgMatches};

use crate::configuration::Config;
use crate::error::{Error, Raise};

pub mod error;
pub mod configuration;

fn main() -> () {
    match run(args()) {
        Err(err) => {
            let msg = err.message();
            eprintln!("{} {}\n{}", "Error:".red().bold(), msg.title, msg.description);
            std::process::exit(1);
        },
        _ => (),
    }
}

fn run(matches : ArgMatches<'static>) -> Result<(), Error> {
    match matches.subcommand() {
        ("", _) => Make::go(&matches),
        ("exec", Some(args)) => Exec::go(args),
        ("echo", Some(args)) => Echo::go(args),
        (cmd, _) => Err(Error::UnknownSubcommand(String::from(cmd))),
    }?;

    Ok(())
}

fn args() -> ArgMatches<'static> {
    clap_app!(cleopatra =>
            (version: "1.0.0-dev")
            (author: "Thomas Letan <lthms@soap.coffee")
            (@setting ArgsNegateSubcommands)
            (about: "An extensible toolchain with facilities for literate programming")
            
            (@arg parallel: -j "Enable parallel build")
            (@arg RECIPE: "The recipe to run (default to `postbuild')")
            
            (@subcommand exec =>
              (about: "Execute a command from the root of the current project")
              (@setting TrailingVarArg)
              (@arg CMD: +required +takes_value +multiple "The command to run"))
            
            (@subcommand echo =>
              (about: "Echo a la cargo")
              (@arg CATEGORY: +required "")
              (@arg DESCRIPTION: +required ""))
        ).get_matches()
}

pub struct Make<'a> {
    /// The recipe to execute
    pub recipe : &'a str,
    /// Set to `true` to allow for parallel build by Make
    pub parallel : bool,
}

impl<'a> CleopatraCommand<'a> for Make<'a> {
    fn of_args(args : &'a ArgMatches<'static>) -> Make<'a> {
        let recipe = args.value_of("RECIPE").unwrap_or("postbuild");
        let jobs = args.is_present("parallel");

        Make { recipe : recipe, parallel : jobs }
    }

    fn run(self) -> Result<(), Error> {
        Config::find_project_then(|project| {
            project.prepare_workspace()?;

            exec(&vec!["make", "-f", ".cleopatra/boot.mk", "init"])?;

            let mut cmd = vec!["make", "-f", ".cleopatra/boot.mk", self.recipe];

            if self.parallel {
                cmd.push("-j");
            }

            exec(&cmd)
        })
    }
}

pub struct Exec<'a> {
    /// A list of strings which together form the command to execute
    pub command : Vec<&'a str>,
}

impl<'a> CleopatraCommand<'a> for Exec<'a> {
    fn of_args(args : &'a ArgMatches<'static>) -> Exec<'a> {
        let cmd = args.values_of("CMD")
            .unwrap()
            .collect();

        Exec { command : cmd }
    }

    fn run(self) -> Result<(), Error> {
        Config::find_project_then(|_|  exec(&self.command))
    }
}

pub struct Echo<'a> {
    pub cat : &'a str,
    pub descr : &'a str,
}

impl<'a> CleopatraCommand<'a> for Echo<'a> {
    fn of_args(args : &'a ArgMatches<'static>) -> Echo<'a> {
        let cat = args.value_of("CATEGORY").unwrap();
        let descr = args.value_of("DESCRIPTION").unwrap();

        Echo { cat : cat, descr : descr }
    }

    fn run(self) -> Result<(), Error> {
        println!("{:>12} {}", self.cat.green(), self.descr);
        Ok(())
    }
}

fn exec(cmd : &[&str]) -> Result<(), Error> {
    Command::new(cmd[0])
        .args(cmd.split_at(1).1)
        .status()
        .or_raise("Could not execute submitted command")
        .and_then(|status| {
            if status.success() {
                Ok(())
            } else {
                Err(Error::Anomaly(format!("The command `{}' failed", cmd.join(" "))))
            }
        })
}

pub trait CleopatraCommand<'a>
where Self : Sized {
    fn of_args(args : &'a ArgMatches<'static>) -> Self;

    fn run(self) -> Result<(), Error>;

    fn go(args : &'a ArgMatches<'static>) -> Result<(), Error> {
        Self::of_args(args).run()
    }
}
