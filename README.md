# Project Title: Multimedia Game

### Statement
* Describe your project. 
	* A Procedurally generated 2D Sidescroller/ Platformer Game
* Why is it interesting? 
	* Many elements of the game including both graphics and audio will be psuedo-randomly procedurally generated.
* Why is it interesting to you personally? 
	* (Wesley) I've tried my hand at game design in the past, but have yet to make a full game. This is an opportunity for me to create a complete game while also learning & experimenting with a new design technique.
	* (David) I have a background in music/audio composition, and I am also an hobbyist gamer.  I've been interested in procedural generation of music for a while.  This project will allow me to explore all of these areas.  
* What do you hope to learn? 
	* Real-world applications of functional programming concepts.
	* How some of Racket's sound, graphics, and gaming libraries work.
	* How procedural generation works in videogames.

### Analysis

Procedural generation requires recursion/iteration as content is generally created one piece at a time, with the content of future pieces partially dependent on that of previous pieces.
An object-oriented approach will be useful for representing information about individual characters and physical objects in the scene, among other things.

During the audio generation process, audio will be represented as lists of notes.  Map and Reduce will be used to convert lists of notes into rsounds, and consolidate multiple rsounds into a single rsound.

Digital Audio is both resource-heavy and unintuitive to think about musically.  As such, it will be abstracted as music notes and sound effects.
<!--
Explain what approaches from class you will bring to bear on the project. 
* Be explicit: e.g., will you use recursion? How? 
* Will you use map/filter/reduce? How? 
* Will you use data abstraction? 
* Will you use object-orientation? Will you use functional approaches to processing your data? Will you use state-modification approaches? A combination?

The idea here is to identify what ideas from the class you will use in carrying out your project. 
-->

### Data set or other source materials

The visuals will require external files containing the relevent images. These can be created in GIMP or using one of Racket's image libraries.
The audio may require audio samples of some sort.
<!--
If you will be working with existing data, where will you get those data from? (Dowload it from a website? access it in a database? create it in a simulation you will build....)

How will you convert that data into a form usable for your project?  

Do your homework here: if you are pulling data from somewhere, actually go download it and look at it. Explain in some detail what your plan is for accomplishing the necessary processing.

If you are using some other starting materails, explain what they are. Basically: anything you plan to use that isn't code.
-->

### Deliverable and Demonstration
Explain exactly what you'll have at the end. 
* What will it be able to do at the live demo?
	* We will have a playable 2D Game to demonstrate at the 
* What exactly will you produce at the end of the project? 
* What will it do?

Eg.
* Will it run on some data, like batch mode? 
* Will you present some analytical results of the processing? 
* How can it be re-run on different source data?
	* It can be given a specific seed to use to generate a 

* Will it be interactive? 
* Can you show it working? This project involves a live demo, so interactivity is good.

### Evaluation of Results
* How will you know if you are successful? 
	* If you include some kind of _quantitative analysis,_ that would be good.

## Architecture Diagram
* Upload the architecture diagram you made for your slide presentation to your repository, and include it in-line here.

* Create several paragraphs of narrative to explain the pieces and how they interoperate.

## Schedule
Explain how you will go from proposal to finished product. 

There are three deliverable milestones to explicitly define, below.

 The nature of deliverables depend on your project, but may include things like processed data ready for import, core algorithms implemented, interface design prototyped, etc. 

You will be expected to turn in code, documentation, and data (as appropriate) at each of these stages.

Write concrete steps for your schedule to move from concept to working system. 

### First Milestone (Fri Apr 15)
* Basic Assets and Gameplay
	* Interactive sprite animation
	* Audio Assets
* Basic procedural generation tests.

### Second Milestone (Fri Apr 22)
* What exactly will be turned in on this day? 

### Final Presentation (last week of semester)
* What additionally will be done in the last chunk of time?

## Group Responsibilities
* Here each group member gets a section where they, as an individual, detail what they are responsible for in this project. Each group member writes their own Responsibility section. Include the milestones and final deliverable.

### Wesley Nuzzo
* Character sprite designs
* Implementation of sprite animations
* Level design, i.e.
	* Background and foreground art
	* Procedural generation code for level layout
* Dialogue

### David Benoit
* Design audio elements
	* Sound Effects
	* Music
* Game physics (gravity, collision)
* Psuedo-random procedural generation of audio 

<!-- Irrevelant as we're not a team of three...

**Additional instructions for teams of three:** 
* Remember that you must have prior written permission to work in groups of three (specifically, an approved `FP3` team declaration submission).
* The team must nominate a lead. This person is primarily responsible for code integration. This work may be shared, but the team lead has default responsibility.
* The team lead has full partner implementation responsibilities also.
* Identify who is team lead.

In the headings below, replace the silly names and GitHub handles with your actual ones.

### Susan Scheme @susanscheme
will write the....

### Leonard Lambda @lennylambda
will work on...

### Frank Functions @frankiefunk 
Frank is team lead. Additionally, Frank will work on...   
-->
