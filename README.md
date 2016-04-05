# Project Title: Random Game Map Generator

### Statement
Adam: Our project is a random game map generator. It will generate 2D game maps using various square tiles holding different graphics. Each graphic will be attached to rules that will determine the nature of how it can be randomly generated. For example, some tiles may only be able to appear a certain number of times, or some may not be able to be placed adjacent to each other. Our project is interesting because, depending on the complexity of our tile placement rules and the number of graphics we have, we can generate a large number of unique maps that appear to be designed by a human, but are actually generated randomly.

John:

### Analysis
Adam: Our plan is to implement the design using a hash table and an array. The hash table will hold the textures and their placement rules, and the array will be used to represent the grid that the tiles will be placed on. We plan to use the picturing-programs library to manipulate the graphics and possibly the gui library for creating a window and random generating options. Recursion will be used for randomly generating the maps and for printing the contents of the array to a window. Filter may be used for finding graphics that are eligable for placement on a certain tile. There is also a strong possibility that me might use object-orientation for each graphic. By doing this we can give each graphic its own properties that will be useful when determining how to place them.

John:

### Data set or other source materials
Adam: We may decide to use graphics found on free use graphics websites for our tiles. The graphics will be resized and edited if needed.

John:

### Deliverable and Demonstration
Adam: In the end we will have a program capable of generating valid random tile maps using various graphics. The program will display a window containing a randomize button and randomization options. It will also have an area to display the generated map.

John:


### Evaluation of Results
Adam: We will know that we were successful if we can randomly generate valid maps. Meaning that the path intended to be taken by a player is not obstructed by walls, water, or any tile that is considered to be an obstruction. If the end of a map is unreachable, we'll know that our tile placement algorithms aren't working properly.

John:

## Architecture Diagram
Upload the architecture diagram you made for your slide presentation to your repository, and include it in-line here.

Create several paragraphs of narrative to explain the pieces and how they interoperate.

## Schedule
Explain how you will go from proposal to finished product. 

There are three deliverable milestones to explicitly define, below.

The nature of deliverables depend on your project, but may include things like processed data ready for import, core algorithms implemented, interface design prototyped, etc. 

You will be expected to turn in code, documentation, and data (as appropriate) at each of these stages.

Write concrete steps for your schedule to move from concept to working system. 

### First Milestone (Fri Apr 15)
Adam: By the first milestone we want to have a working manual map generator. The graphics will have to be placed in the array by the user, but the map should be able to be displayed in a window by this point. We also plan to have a fair number of different graphics available for use.

John:

### Second Milestone (Fri Apr 22)
Adam: By the second milestone we'd like to have a random generate button that is capable of generating random maps, that for the most part, are obstruction free. We may still need to refine our placement algorithms to guarentee a valid map. This will the most challenging part of the project and will take a lot of testing to perfect.

John:

### Final Presentation (last week of semester)
Adam: Assuming that we don't run into any unforseen problems, we'd like to use this time to perfect the tile placement rules and algorithms so that a valid random map is generated each time. We may also add in more graphics for the tiles.

John:

## Group Responsibilities
Here each group member gets a section where they, as an individual, detail what they are responsible for in this project. Each group member writes their own Responsibility section. Include the milestones and final deliverable.

**Additional instructions for teams of three:** 
* Remember that you must have prior written permission to work in groups of three (specifically, an approved `FP3` team declaration submission).
* The team must nominate a lead. This person is primarily responsible for code integration. This work may be shared, but the team lead has default responsibility.
* The team lead has full partner implementation responsibilities also.
* Identify who is team lead.

In the headings below, replace the silly names and GitHub handles with your actual ones.

### Adam Melle @adam-melle
will write the....

### John Perkins @raghnall6402
will work on...

### Frank Functions @frankiefunk 
Frank is team lead. Additionally, Frank will work on...   
