# Project Overview

While Unbound can be seen as a standalone game, it is also an easy to use yet opinionated game engine/framework to create all kinds of things.

## Just Another Minecraft Clone

Yes, Unbound is, very much so, _highly_ inspired by Minecraft.

Minecraft is great and I love the game (and the insane number of absolutely incredible mods) to death.

But at the same time, as amazing as it is, there are also a lot of gripes I have with the game, which are the main reason as to why Unbound exists.

### Programming Language

I'll keep it brief; I don't like Java. Don't get me wrong, I'm sure it improved a lot over the years, but if I had to pick a programming language (which luckily I am able to, if I create my own project) I'll choose something that I personally find much more enjoyable to work with: Rust.

### Game Customization

Minecraft features various ways to customize your game. From resource packs to evergrowing but still fairly limiting datapacks all the way to proper but non-official modding using some modloader like Forge or Fabric.

Unbound on the other hand will have a much stronger focus on modding: Pretty much everything is a mod. The game itself is essentially just the framework with various very generic features that these mods can make use of to create cool experiences for players.

Unbound will be mostly very declaratively data driven, but still allow for scripting. Said scripting will be fully sandboxed and start out with Lua or Teal (Lua with types). Eventually Lua might be getting deprecated in favor of using my own, from the ground up typed, scripting language called FluxFlow.

### Artstyle

The artstyle of Unbound will still feature pixelart just like Minecraft's default look, however I do want to try and add a variable sized and colored toon-outline on top to hopefully give it a unique look, apart from (obviously) using different textures. I don't know if there are a lot of other popular games that have tried pixelart with a toon outline, but I really wanna at least give that a try.

I do want to slightly increase the pixelart resolution when compared to Minecraft. E.g. I want items to be more around 24x24 to 32x32 pixels to have a bit more room to work with.

### Innovation

While there are a lot of things that I just want to do "_better_" than Minecraft, there are also a few areas that I want to not necessarily improve, but just do a bit different and give it somewhat of a twist, so it doesn't feel like _just another Minecraft Clone_.

#### Terrain

The biggest part here is terrain.

Minecraft stores its terrain in so called "chunks" and displays it in a classic cubic voxel fashion with somewhat limited render distance where one block is roughly one meter in size.

Unbounds on the other hand makes use of Octrees to store large areas of identical blocks such as air or stone in a very compact fashion, which should (at least in theory) result in worlds requiring much less memory while also giving a solid foundation for having insanely high render distances by just reducing the level of detail.

This also makes it possible for terrain to have a higher resolution, twice as high (2x2x2 equals one Minecraft block), without being too taxing on memory. While I probably could go even finer with resolution, I feel like it starts getting hard/tedious to work. That of course also depends on the tools you have to edit the terrain, but I feel like twice the resolution of Minecraft just feels right to me.

Additionally, terrain is not displayed using classic cubic voxels, but instead uses marching cubes, resulting in much smoother looking terrain.

##### Non-Terrain Blocks

While Minecraft stores everything (apart from entities) as blocks in chunks, Unbound instead stores blocks with custom models such as doors, chests and whatnot separately. I also want it to have much better support for "multiblocks" that cover multiple blockspaces. To differentiate these non-terrain blocks from terrain, they are referred to as "Design" in Unbound (I'm not yet sure if I like that name, but I couldn't come up with something better yet).
