# Unbound

TODO: Badges

A [Minecraft](https://www.minecraft.net/en-us/about-minecraft) inspired game/-engine featuring non-blocky, near-infinite [smooth](https://en.wikipedia.org/wiki/Marching_cubes) terrain built on top of the [bevy](https://bevyengine.org/) game engine.

See the [Foam](https://foambubble.github.io/foam/) based [Project Overview](foam/project-overview.md) for more details on what I want Unbound to be.

## Project Structure

Split into four different crates:

- [unbound-client](./unbound-client) - The executable that runs the game itself (can also host servers)
- [unbound-server](./unbound-server) - A separate (smaller) executable without a GUI for dedicated servers
- [unbound-shared](./unbound-shared) - A library that contains code that is used by both client and server
- [unbound-lib](./unbound-lib) - Contains generic utilities that (unlike `unbound-shared`) don't depend on [bevy](https://bevyengine.org/)
