# mp-warps-rando-checker
Checks logs generated by Claris's Metroid Prime Randomizer to see if they are completable. Works for warp and/or elevator randomization. Also works for vanilla randomization.

# Difficulty levels

* Easy - Does not require any tricks. It is unlikely that any seed will be completable with this difficulty unless several items are non-randomized.
* Medium - Requires some easy tricks, but no dashing. It is unlikely that any seed will be completable with this difficulty unless several items are non-randomized.
* Hard - Intended to be completable by Any%/100% speedrunners. Some easier secret worlds are required. No difficult bomb jumps (HBJs, UBJs) are required.
* Very Hard - Intended to be completable by Low% speedrunners. Requires some difficult bomb jumps and generally does not require Space Jump.
* Expert - Adds some lengthy out-of-bounds traversal as well as incredibly tedious tricks (notably Tallon Floaty in Landing Site)

# Limitations
In some cases, the solver won't be able to find a solution even though a solution exists. This is usually due to it "using up" a warp that it needs later. However, the solver should (hopefully) never say a seed is completable when it is not.

Currently, some removable barriers in the game are treated as being one-way (e.g. the barrier in Metroid Quarantine A).

Boss difficulty is not considered at all.

Floaty jump is not considered, unless it is able to be acquired repeatedly, since a one-time usage trick makes the logic too complex. Tallon Floaty is the only example of floaty that can be required.

Magmoor Traversals without heat protection are allowed, but the e-tank requirements are only approximate. Also, most e-tank requirements assume warps are enabled, so they may not be accurate for non-warps.

# Compiling from source
First, install GHC on your machine, and make sure the GHC executable is added to your PATH variable if needed. Running the following GHC command from the root of the project will compile the code and place all of the compiled files into a folder called "build":

ghc app/Main.hs --make -O2 -i"./src" -hidir "./build" -odir "./build" -o ./build/mp-warp-rando-checker