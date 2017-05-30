# ULVM 
Universe Level VM

# Motivation
System development is composed of two separate tasks:

1. Writing low-level, single-purpose, self-contained modules
2. Wiring those modules together into a system

Instead of conflating the two inside of a single programming environment and forcing bits and pieces out into separate configuration environments and infrastructure setup, ULVM is a single language that allows for the wiring-together of any modules in any programming languages into "flows," which are implemented within "scopes."

To evaluate a system now, you either walk around with a mental model that is outdated the second developers show up tomorrow, constantly try to update a napkin with boxes and arrows or searching through scattered configuration files and infrastructure recipes (if they exist) in addition to source code for umpteen services that all call each other in a hidden and implicit graph.  No wonder no one can design a secure or robust system.  By separating the low-level module writing from the system-level connection graph construction, ULVM provides a single source of truth that is simultaneously enables easy and rapid development across teams, simple searches for security vulnerabilities, and the maintenance of a picture of the full system architecture at any level of detail.  With its support for arbitrary logic, it is also easy to infuse a ULVM system with logical assertions important to various stakeholders: programmers can make assertions establishing pre- and post-conditions of their modules, system authors can make assertions ensuring various topological constraints, and security teams can add assertions ensuring desired properties concerning data locality, authorization checks, and validations.

Importantly, ULVM is an open system, meaning that scope and module providers are all external projects.  This means that ULVM code may target any desired level of detail.  All internals of a process may be constructed in ULVM or, as a means of transition, existing processes may just be referenced by ULVM code to attain the advantages of a single source of truth and a single build and deploy environment.

## The problem of implied dependencies

* Which files does this code read to get configuration data?
* I see an HTTPS connection in the code to localhost:5766.  What does my code assume is running there?
* I know that my service is stateful and clients must be routed to the right instance based on some partition key.  Are all clients doing that properly?

Our code doesn't actually encode much information about our systems.  The system information is scattered between code, configs, and deployment scripts.  Especially in a serverless world, our code only contains dangling pointers to some unspecified other systems and, for the developer or debugger trying to understand an actual business flow, chasing those dangling pointers eats up an increasingly absurd amount of time until sufficient tribal knowledge has been absorbed.  ULVM *is* the encoding of our system.  It is *always* up to date because it actually *generates* our system.

The whole system.

In one place.

Always up to date.

Inspectable.  Debuggable.  Provable.

## Call depth vs reusability

We all claim that we like to structure our code as small, reusable modules.  However, modern languages make it all too easy to conflate control flow and business processing flow with the primary purpose of a module.  The main symptom of this is deep call graphs.  Module A shouldn't call B, which calls C, which calls D, primarily because module A can now only be used in flows that require a call to D.  Using nodejs as an example, we often avoid deep logical call graphs by passing callbacks, which allow the result of module A to be passed to module B without A knowing anything about B.  Unfortunately, that just removes the problem by one step.  We now need to make a decision about where we determine which function we pass as A's callback and this is a decision that is made haphazardly and by each developer; I have yet to find a project with a completely consistent separation between the module layer and the flow layer.  ULVM crisply defines that layer.  Modules are written in any target language and ULVM is the layer at which those modules are arranged into scopes and the layer at which flows are defined by wiring those modules together.  ULVM provides a clean separation between context-free reusable modules and context-dependent flows.

# Concepts

## Flows
A "flow" is a logical call-graph, passing the outputs of some modules into the inputs of others.  The input module may live in a separate process than the output module or on a different machine or in a different cluster.  The input module may deliver its output synchronously or asynchronously via a future or a promise or a queue.  Just like we allow the compiler to handle the mechanics of calling conventions within most modern programs, ULVM handles the mechanics of these higher-level "invocation conventions" in our flows.

## Scopes
Each "scope" is an artifact that ULVM will produce.  For example, one scope may be a nodejs program, whose artifact is a set of nodejs source files that ULVM produces based upon the modules in the flows that were declared to exist within that nodejs scope.  Scopes may be nested such that the nodejs scope may exist within a node docker image scope (producing a directory with a Dockerfile that builds the node code into a docker image), which exists within an AWS Auto Scaling cluster scope (producing a CloudFormation template that would launch the docker image), that exists within an AWS VPC scope (producing a CloudFormation template that sets up the required AWS resources and networking permissions, refering to the inner Auto Scaling template).  Scopes may be seen as templates from which one or more scope instances will be created at runtime, just as source code is a template from which one or more processes will be created at runtime.

## Logic
ULVM is untyped in the traditional sense but...  ULVM exposes an interface for making arbitrary logical assertions, which are currently checked by the [Z3 Theorem Prover](https://github.com/Z3Prover/z3).  Types, subtypes, etc. may easily be modeled under this system, as can much higher-level concerns like "this data must never leave this machine," "user input is not to be passed into this scope or module without first having been sanitized," or "at some point prior to reaching this module, this flow must have passed through an authorization checking module."

# What about state?

Scopes are templates for scope instances, which may contain state.  A scope instance could be a process, a machine, a docker container, or even a datacenter.  All state must be declared in the scope's initializer.

Flows are stateless.  A flow is analogous to a function (WITHOUT any static variables).

# The open compiler

ULVM is designed as an open system.  It does not ship with support for all imaginable scopes, all languages, or even all methods of gathering dependencies.  It enables developers to craft components of the compiler in isolation to suit their needs and refer to them from ulvm files.

## Loaders

Loaders are responsible for gathering dependencies at compile time.

## Scope handlers

### Scope generators

Scope generators are responsible for creating a source artifact from a scope definition and its related flow definitions.  Scope generators transform a representation of a scope and relevant flows with their respective ASTs into a source artifact (e.g. a directory with sourcecode or a cloudformation template).  Because scopes may need to refer to configuration values, including references to the compiled artiacts of other scopes, scope generators may generate source artifacts that contain template files, whose values will be filled in at a later stage.

### Scope templaters

Scope templaters are responsible for filling in the templates remaining in a source scope generated by a scope generator.  For example, this may require filling in the maven version of a subscope or the location in s3 at which to refer a subscope (e.g. for cloudformation templates that refer to each other).

### Scope compilers

Scope compilers can compile the source artifacts generated by scope generators into compiled artifacts.

### Scope versioners 

Scope versioners are responsible for assigning a version to a source artifact.

### Scope stagers

Scope stagers are responsible for staging a compiled artifact.  For example, they may store the compiled artifact in an s3 bucket with a key based on its version.

### Scope instance creators

Scope instance creators create instances of scopes (e.g. launch a process, create a cloudformation environment, run a docker container).
