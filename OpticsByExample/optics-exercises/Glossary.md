# Glossary

### Structure

Data we want to apply to the lense. It could be a record or tree etc with well-typed sections. These well-typed sections become the path pieces we use to "read into" the structure.

### Path

A path is a description of how to "read into" a structure. That description could be simply by index eg. "get the first thing" or "get the second thing" or it could be a series of types such that we get the Address, then the Street in sequence.

### Focus

The focus is the result of applying a Path to a Structure, possibly nothing.

### Action

Given a structure and a path an action goes to the point of focus and performs a unique operation relative to other actions.

## Lens

A type of optic, with an associated set of actions.

### Lens Guarantees

* *focuses* (i.e. *selects*) a *single* piece of data within a larger *structure*
* must *never fail* to *get* or *modify* that focus

### Associated actions

* We can use a lens to *view* the *focus* within a structure
* We can use a lens to *set* the *focus* within a structure
* We can use a lens to *modify* the *focus* within a structure

### Anatomy

[Action] [Path] [The Structure (with focus)]

eg. `view (_2 . _1) (42, ("hello", False))`
