This README is quite lacking, but I don’t have time to write anything better
right now=)

# Overview
Cortex is a collection of software that together provides a distributed hosting
platform for web applications.

# Miranda
Miranda provides storage for other modules and acts as a communication bus
between them.

# Saffron
Saffron actually starts and kills web application servers.

# G23
G23 cleans up after Saffron instances that didn’t close properly.

# Ariel
Ariel provides a load balancer for all application servers started by Saffron.

# Vera
Client application to manage the platform.

# Installation

## Platform support
Cortex should run on any UNIX-like OS, especially any GNU based OS should be
fine.

## Required software

### Haskell
Cortex is mostly written in Haskell and requires a compiler implementing
Haskell 2010 standard and several language extensions (grep for `LANGUAGE` in
sources to get a list).  GHC 7.4 is recommended and it was the only compiler
tested, but other compilers could probably be used.  Apart from the standard
library Cortex requires following Haskell packages:

* `hinotify`
* `HsOpenSSL`
* `hstringtemplate`
* `hunit`
* `lifted-base`
* `network`
* `random`
* `sha`
* `temporary`

### Python
Parts written in Python should work with any 2.7 or 3.2+ compatible
interpreter/compiler.  Cortex needs these Python packages:
* `psutil`

### Other software
Cortex build script needs a shell that expands `**` to every file under current
directory, currently the shell is hard set to `/bin/zsh`.

By default build script uses GHC's LLVM backend, you might have to install it
separately from GHC.

Ariel requires Nginx to run.

### Saffron modules
Depending on the kind of frameworks you want to use, you might need to install
additional software:

#### Static files
* Nginx

#### Ruby on Rails
You should install RVM if you want to use this module.
* Phusion Passenger
* Bundler

# Usage

# Development
`Utils/autobuild` will rebuild Cortex when any Haskell source file changes, it
can be used similarly to autobuild functionality from IDEs like Eclipse.

## TODO
* God to monitor memory and CPU usage of servers.
* Authentication and authorisation.
