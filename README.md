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

## Modules
A module is a logical function, which is ideally developed in isolation, in the target language.  All ULVM knows about modules is some location information (where can the scope get the code for this thing?), what inputs it has (all parameters are named), and what logical outputs it has (an exception is an output, as is a pointer parameter, or the value passed to a callback).  ULVM also supports attaching configuration information to modules, which allows scopes to consume that metadata and enables our logic system (described below).  Modules also declare a module-combinator, which is responsible for generating an AST (specific to a given type of scope) that represents their invocation.

## Macros
We support two types of macros: inline module macros, which render a logical function that is invoked with a combinator like any other--they have dynamic function *definitions*--, and module combinator macros, which modify the invocation site of a module--they have dynamic function invocations.  A module that is capable of extracting properties from a JSON map and passing them as arguments to another function would be an inline module macro whereas a module that represents a pattern match or if statement would be a module combinator macro.

## Logic
ULVM is untyped in the traditional sense but...  ULVM exposes an interface for making arbitrary logical assertions, which are currently checked by the [Z3 Theorem Prover](https://github.com/Z3Prover/z3).  Types, subtypes, etc. may easily be modeled under this system, as can much higher-level concerns like "this data must never leave this machine," "user input is not to be passed into this scope or module without first having been sanitized," "at some point prior to reaching this module, this flow must have passed through an authorization checking module," or "this module will only add money values in the same currency."

# What about state?

Scopes are templates for scope instances, which may contain state.  A scope instance could be a process, a machine, a docker container, or even a datacenter.  All state must be declared in the scope's initializer.

Flows are stateless.  A flow is analogous to a function (WITHOUT any static variables).

# The open compiler

ULVM is designed as an open system.  It does not ship with support for all imaginable scopes, all languages, or even all methods of gathering dependencies.  It enables developers to craft components of the compiler in isolation to suit their needs and refer to them from ulvm files.

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

# Writing ulvm systems

## Environment

The environment is a write-once value hierarchy that is built up as part of building a ulvm environment.  All elements of the ulvm compiler take their configuration from the environment and users may supply values to configure the compilation.

### Well-known keys and values

#### Environment keys
 - `:ulvm.core/project-root` - root directory for the ulvm project.  Defaults to the directory passed to to `--dir` 
 - `:ulvm.core/gen-src-root` - root directory for the generated source.  If relative, resolved relative to `:ulvm.core/project-root`.  Defaults to `"src"`
 - `:ulvm.core/build-root` - root directory for the built artifacts.  If relative, resolved relative to `:ulvm.core/project-root`.  Defaults to `"build"`

#### Module combinator attributes
Stored in a map under the `:attr` key in a module combinator's config.
 - `:ulvm.core/result-in-invocation-block` - If mapped to a truthy value, indicates that the result of any invocation for which this module combinator is responsible is available in the invocation block.  That is, no nested block is created.

#### Scope configs
 - `:ulvm.scopes/gen-src-dir` - Added to a scope config to indicate the directory to write the scope's src to.
 - `:ulvm.scopes/build-dir` - Added to a scope config to indicate the directory to write the compiled artifact to.
 
# TODO
[ ] Handle transformers.
[ ] Remove the built-in de-nesting.  This should be handled by the module combinator.
[ ] Module combinators must be passed multiple bodies, one for each possible disjoint set of results.
[ ] How does chaining work?  (e.g. promise chains or clojure threading macros)
[ ] How do we specify or determine the module-combinator of a flow?  That is, how do we return values from the flow?
[ ] Do we support un-named sub-flows?  Right now, every invocation in a flow "returns" a value to the originating scope.  We should support some computation on scope A, which then hands off to some computation on scope B, without making another named flow.
[ ] How do we avoid naming every result?
[ ] Rationalize namespaces.
[ ] Move all user-visible constants (e.g. `ulvm.core/project-root` to a constants file)
