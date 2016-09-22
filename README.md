These are the steps to building a realtime chart using haskell.

If you want a performance, realtime chart for >10,000 points, go for OpenGL/Vulkan.

For a few hundred points, d3.js should do fine.
For less than 1,000 points, diagrams does well.
For less than 10,000 points, gloss or cairo does well. Gloss is super easy to use.
For anything more, go with OpenGL/Vulkan.
