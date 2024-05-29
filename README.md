# Unbound

A [Minecraft](https://www.minecraft.net/en-us/about-minecraft) inspired game/-engine featuring non-blocky, infinite terrain based on [marching cubes](https://en.wikipedia.org/wiki/Marching_cubes) built using [bevy](https://bevyengine.org/).

## Project Structure

Split into three different crates:

- [unbound-client](./unbound-client) - The executable that runs the game itself (can also host servers)
- [unbound-server](./unbound-server) - A separate (smaller) executable without a GUI for dedicated servers
- [unbound-shared](./unbound-shared) - A library that contains code that is used by both client and server
