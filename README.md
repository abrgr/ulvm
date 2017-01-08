# ULVM 
Universe Level VM

# Motivation
System development is composed of two separate tasks:

1. Writing low-level, single-purpose, self-contained modules
2. Wiring those modules together into a system

Instead of conflating the two inside of a single programming environment and forcing bits and pieces out into separate configuration environments and infrastructure setup, ULVM is a single language that allows for the wiring-together of any modules in any programming languages into "flows," which are implemented within "scopes."

To evaluate a system now, you either walk around with a mental model that is outdated the second developers show up tomorrow, constantly try to update a napkin with boxes and arrows or searching through scattered configuration files and infrastructure recipes (if they exist) in addition to source code for umpteen services that all call each other in a hidden and implicit graph.  No wonder no one can design a secure or robust system.  By separating the low-level module writing from the system-level connection graph construction, ULVM provides a single source of truth that is simultaneously easy to rapidly develop with across teams, easy to search for security vulnerabilities, and easy to picture the full system architecture at any level of detail.  With its support for arbitrary logic, it is also easy to infuse a ULVM system with logical assertions important to various stakeholders: programmers can make assertions establishing pre- and post-conditions of their modules, system authors can make assertions ensuring various topological constraints, and security teams can add assertions ensuring desired properties concerning data locality, authorization checks, and validations.

Importantly, ULVM is an open system, meaning that scope and module providers are all external projects.  This means that ULVM code may target any desired level of detail.  All internals of a process may be constructed in ULVM or, as a means of transition, existing processes may just be referenced by ULVM code to attain the advantages of a single source of truth and a single build and deploy environment.

# Concepts

## Flows
A "flow" is a logical call-graph, passing the outputs of some modules into the inputs of others.  The input module may live in a separate process than the output module or on a different machine or in a different cluster.  The input module may deliver its output synchronously or asynchronously via a future or a promise or a queue.  Just like we allow the compiler to handle the mechanics of calling conventions within most modern programs, ULVM handles the mechanics of these higher-level "invocation conventions" in our flows.

## Scopes
Each "scope" is an artifact that ULVM will produce.  For example, one scope may be a nodejs program, whose artifact is a set of nodejs source files that ULVM produces based upon the modules in the flows that were declared to exist within that nodejs scope.  Scopes may be nested such that the nodejs scope may exist within a node docker image scope (producing a directory with a Dockerfile that builds the node code into a docker image), which exists within an AWS Auto Scaling cluster scope (producing a CloudFormation template that would launch the docker image), that exists within an AWS VPC scope (producing a CloudFormation template that sets up the required AWS resources and networking permissions, refering to the inner Auto Scaling template).

## Logic
ULVM is untyped in the traditional sense but...  ULVM exposes an interface for making arbitrary logical assertions, which are currently checked by the [Z3 Theorem Prover](https://github.com/Z3Prover/z3).  Types, subtypes, etc. may easily be modeled under this system, as can much higher-level concerns like "this data must never leave this machine," "user input is not to be passed into this scope or module without first having been sanitized," or "at some point prior to reaching this module, this flow must have passed through an authorization checking module."
