# Project Title: Random Game Map Generator

Adam Update: The map generator now has the ability to generate random pools of water. I have also implemented a slider that can be used to choose the insensity of the water. The slider basically just increases/decreases the chance that a water pool will be spawned, and how big the pool will be. The same option will be available for lava once it's implemented, as well as for other tiles that can be selected for use. Nothing special has to be done to run the code. The images just need to be in the same directory as the racket file.

Adam Update: As of right now, for the first milestone, the map generator is able to generate maps manually as well as generate basic maps with random paths. The path-making algorithm has been working 100% of the time. The next thing to do is to implement the hash table and generation algorithms for different tile types.

### Statement
Adam: Our project is a random game map generator. It will generate 2D game maps using various square tiles holding different graphics. Each graphic will be attached to rules that will determine the nature of how it can be randomly generated. For example, some tiles may only be able to appear a certain number of times, or some may not be able to be placed adjacent to each other. Our project is interesting because, depending on the complexity of our tile placement rules and the number of graphics we have, we can generate a large number of unique maps that appear to be designed by a human, but are actually generated randomly.

John: As Adam stated, our project is a random map generated with a more focus on a maze or dungeon-like design. We hope to include different tile types that would have different terrains such as Water, Pits, Walls, and Normal Floors which would have it's own variables such as passable or not. The idea is that after this random dungeon is created, it could be used as a 2d grid for a game in the future. This project is interesting to me as I’ve implemented somewhat similar ideas in other languages such as C++, but never in full. It will also be a good experience for me as I’ve not done any projects myself that involve directly collaborating with team members such as this one.

### Analysis
Adam: Our plan is to implement the design using a hash table and an array. The hash table will hold the textures and their placement rules, and the array will be used to represent the grid that the tiles will be placed on. We plan to use the picturing-programs library to manipulate the graphics and possibly the gui library for creating a window and random generating options. Recursion will be used for randomly generating the maps and for printing the contents of the array to a window. Filter may be used for finding graphics that are eligable for placement on a certain tile. There is also a strong possibility that me might use object-orientation for each graphic. By doing this we can give each graphic its own properties that will be useful when determining how to place them.

John: Adam wrote the basis as far as what we’ve concluded to use in our project with the use of hash tables and arrays. I think the only case where we’ll need state modification might be with the randomization options to create these maps unless it is implemented when the base procedure is called. As far as data abstraction goes we’d obviously be using hash tables to represent our different versions of times which would have their own variables such as “passable terrain” so that there will always be at least one path to the end.

### Data set or other source materials
Adam: We may decide to use graphics found on free use graphics websites for our tiles. The graphics will be resized and edited if needed.

John: Source materials would be graphics from a site or some simple ones created from some image editing program.

### Deliverable and Demonstration
Adam: In the end we will have a program capable of generating valid random tile maps using various graphics. The program will display a window containing a randomize button and randomization options. It will also have an area to display the generated map.

John: As far as the live demo is involved, we should be able to demonstrate this random map generator by showing off the different types of tiles included at the base level, and the ability to create new types on the fly by inserting one into the hash table and re-running it. Another aspect at the live demo would be demonstrating some of the randomization options such as the ability to change the size, in tiles, of the map that is produced. The hope is that at the end of the project, we would have made a successful random map generator that could be used for a game in the future. To show that the project is working we would interact with the randomization options and add different terrain types to it on the fly.

### Evaluation of Results
Adam: We will know that we were successful if we can randomly generate valid maps. Meaning that the path intended to be taken by a player is not obstructed by walls, water, or any tile that is considered to be an obstruction. If the end of a map is unreachable, we'll know that our tile placement algorithms aren't working properly.

John: It will be successful in my mind if in addition to a path from the start to the end as mentioned by Adam, there is the ability to insert different terrain types and possibly filter which we want active for a given map. If it does not have these type of randomization options then I our interface and implementation didn’t work correctly. Also if we have rooms or paths which don’t connect to the start themselves, then our algorithms are working incorrectly.

## Architecture Diagram
![](https://raw.githubusercontent.com/oplS16projects/Adam-John-Map-Generator/master/Diagram.png)

The hash table will be used to store the graphics and their properties. The hash table will be accessed by a randomization procedure that will output into the grid array. The randomize procedure will use the properties stored in the hash table for each graphic to determine where a tile is able to be placed. From there, the array will be processed by the picturing programs library and displayed in a window. The window will contain a randomize button as well as randomization options. We plan to created these buttons and input fields using racket/gui.

## Schedule

### First Milestone (Fri Apr 15)
Adam: By the first milestone we want to have a working manual map generator. The graphics will have to be placed in the array by the user, but the map should be able to be displayed in a window by this point. We also plan to have a fair number of different graphics available for use.

John: First Milestone should include a basic test map that displays our different types of tiles and the first pass of randomization of maps. More importantly at this point I think is the implementation and display of different tile graphics.

### Second Milestone (Fri Apr 22)
Adam: By the second milestone we'd like to have a random generate button that is capable of generating random maps, that for the most part, are obstruction free. We may still need to refine our placement algorithms to guarentee a valid map. This will the most challenging part of the project and will take a lot of testing to perfect.

John: Second Milestone should more inplace to guarantee valid maps and more rules for our tiles themselves, as well as a button or command to generate a new map on the fly.

### Final Presentation (last week of semester)
Adam: Assuming that we don't run into any unforseen problems, we'd like to use this time to perfect the tile placement rules and algorithms so that a valid random map is generated each time. We may also add in more graphics for the tiles.

John: Hopefully by this point we can clear up any issues that we run across and implement various UI elements such as more options for  map generation like choosing which implemented terrain types to be used and the size of the maps, as well as the ability to  print/save(?) the map.

## Group Responsibilities

### Adam Melle @adam-melle
First Milestone: Will be responsible for creating the tile array and printing to the window.
Second Milestone: We will both be working on randomly generating the map.

### John Perkins @raghnall6402
First Milestone: Will be responsible for creating the hash table for the graphics.
Second Milestone: We will both be working on randomly generating the map.
