# metroid-prime-randomizer-solver
This application checks logs generated by Claris's Metroid Prime Randomizer to see if the seeds are completable. It works for warp and/or elevator randomization. Also works for vanilla randomization. Works for Randomizer version 3.0 and later.

Claris's Randomizer is available [here](https://www.dropbox.com/s/s9nomgf5few8kyv/Randomizer.zip).

If you have not used the Randomizer before, Rekameohs's Script can help with the ISO extraction and removing the Frigate from the start of the game. Note that an older version of the Randomizer is included with the script, and the script may not work for newer versions of the Randomizer. The script and its included tools are available [here](https://www.dropbox.com/sh/66gciez2ql2enjw/AAAsH3tnA6FZiHs_8zeZpfKHa).

You should be able to randomize using Rekameohs's script and its included version of the randomizer to skip the frigate. From then on, run the newer version of the randomizer (without using the script) to create a new seed. You can still use Rekameohs's script to create the ISO after randomization.

## Usage
```
metroid-prime-randomizer-solver -d [Difficulty] -l [Log Directory]
```

Difficulty can either be the full difficulty name (easy, medium, hard, veryhard, expert, all) or the single-letter abbreviation (e, m, h, v, x, a).

If you provide the "all" option for difficulty, the checker will find the easiest difficulty level for which the seed is completable (or print "False" if not completable). Note that this is considerably slower, since each seed may be checked multiple times.

The log directory can either be a folder containing all of the logs, or a single log file. Windows-style filepaths (i.e. filepaths with backslash instead of slash) should work. When providing a single log file, the file must end with the .txt file extension. When providing a directory containing log files, the solver will attempt to parse everything in the directory, so the directory must contain log files only.

The application can also be run without providing any arguments, and it will prompt for any arguments that are not provided.

## Difficulty levels

* Easy - Does not require any tricks. It is unlikely that any seed will be completable with this difficulty unless several items are non-randomized.
* Medium - Requires some easy tricks, but no dashing. It is unlikely that any seed will be completable with this difficulty unless several items are non-randomized.
* Hard - Intended to be completable by Any%/100% speedrunners. Some easier secret worlds are required. No difficult bomb jumps (HBJs, UBJs) are required.
* Very Hard - Intended to be completable by Low% speedrunners. Requires some difficult bomb jumps and generally does not require Space Jump.
* Expert - Adds some lengthy out-of-bounds traversal as well as incredibly tedious tricks (notably Tallon Floaty in Landing Site)

## Limitations
In some cases, the solver won't be able to find a solution even though a solution exists. This is usually due to it "using up" a warp that it needs later. However, the solver should (hopefully) never say a seed is completable when it is not.

Some seeds are techically completable, but can become impossible if the warps are used in the wrong order. The solver will still say such seeds are completable.

Boss difficulty is not considered at all.

Floaty jump is not considered, unless it is able to be acquired repeatedly, since a one-time usage trick makes the logic too complex. Tallon Floaty is the only example of floaty that can be required (and only on Expert difficulty).

Magmoor traversals without heat protection are allowed, but the e-tank requirements are only approximate. Also, most e-tank requirements assume warps are enabled, so they may not be accurate for non-warps seeds.

## Compiling from source
### Installing Required Tools
All of the resources needed to compile from source can be found [here](https://www.haskell.org/downloads/). On Windows, the easiest way to install everything is using Chocolatety as described on that page. At minimum, you need GHC to compile the code, but you may want to install Haskell Cabal and Haskell Stack as well.

### Compilation
First, install GHC on your machine, and make sure the GHC executable is added to your PATH variable if needed. Running the following GHC command from the root of the project will compile the code and place all of the compiled files into a folder called "build":

```
ghc app/Main.hs --make -O2 -i"./src" -hidir "./build" -odir "./build" -o ./build/metroid-prime-randomizer-solver
```

With the above approach, the "build" folder needs to be created ahead of time. As an alternative, you can use the "build-run.bat" batchfile to compile and run the application with GHC. This batchfile will delete the "build" folder if it exists, and then make a new one prior to compilation. 

If you have Haskell Cabal installed, you can compile and run the application using:

```
cabal run main
```

If you encounter linking issues when compiling with Cabal, it may be due to filepaths that are too long. For this reason, using the "build-run.bat" batchfile is probably the easiest option.

## Customizing Tricks / Room Requirements
If you want to modify this code to change room / item requirements, you'll mostly need to edit two files: Predicates.hs, and Graph.hs. Have fun.

## Required Tricks

* Easy
    * No Tricks are required
* Medium
    * Climb Root Cave without X-Ray
    * Climb Frigate Crash Site with Space Jump
    * Cross Frigate Crash Site water with just Morph Ball
    * Collect Frigate Crash Site item with Bombs and Gravity Suit but without Space Jump
    * Climb Cargo Freight Lift to Deck Gamma with Bombs and no Gravity Suit
    * Climb Biohazard Containment with Space Jump and no Gravity Suit or with Bombs and Gravity Suit
    * Traverse Biotech with Space Jump and no Gravity Suit or with Bombs and Gravity Suit
    * Climb Great Tree Hall spider track with Spider Ball and Bombs
    * Get Life Grove Underwater item with Bombs instead of Space Jump
    * Get Main Plaza Grapple Ledge by jumping to it with Space Jump
    * Get Main Plaza Locked Door item with Space Jump
    * Get to Tower Chamber without Gravity but with Space Jump
    * Climb Ruined Shrine with Space Jump
    * Climb the small spider track in Furnace without Spider Ball 
    * Traverse Crossway with Space Jump
    * Activate the bomb slot in Hall of the Elders with Space Jump and no Spider Ball
    * Climb Reflecting Pool with Space Jump
    * Cross Twin Fires Tunnel going toward the Tallon Elevator with Space Jump or Bombs
    * Escape Quarantine Cave using Grapple
    * Collect the Observatory item from the top of Observatory using Space Jump
    * Get to Storage Cave and Security Cave without Grapple
    * Get out of various water in Phendrana without Gravity Suit
    * Reach Main Quarry save station using Space Jump (to unlock it if needed)
    * Reach Waste Disposal without Space Jump (but with Grapple)
    * Cross Transport Access without Grapple
    * Cross Metroid Quarantine A without X-Ray
    * Get to Flamethrower Room without killing the Wave Pirates
    * Cross Fungal Hall A using either Grapple or Space Jump
    * Cross Fungal Hall B using either Grapple or Space Jump
    * Cross Quarantine Access B without Space Jump
    * Cross Metroid Quarantine B without Grapple
* Hard
    * Some wallcrawling is allowed at this difficulty
    * Space Jump First
    * Get Root Cave item without Grapple Beam
    * Bypass the Arbor Chamber plasma door using Space Jump to get out of bounds
    * Cross Frigate Crash Site water without Morph
    * Get Frigate Crash Site item without any items
    * Climb Frigate Crash Site with Space Jump
    * Climb Main Ventillation Shaft Section C using a series of DBJs
    * Climb Reactor Core using Bombs if you have Gravity Suit
    * Climb Biohazard Containment with Bombs only
    * Cross Biotech with only Wave. Cross the other way with Space Jump or Bombs
    * Great Tree Hall bars skip both ways
    * Climb Great Tree Hall spider track using Space Jump
    * Go out of bounds in Life Grove using Space Jump and wallcrawl to Great Tree Hall
    * Go out of bounds in Main Plaza using Space Jump and wallcrawl to Training Chamber, Training Chamber Access, or Tower of Light
    * Collect Ruined Fountain without killing Flaahgra
    * Collect Tower Chamber without Gravity Suit or Space Jump
    * Climb Tower of Light without Missiles using Space Jump
    * Dash across Magma Pool with Space Jump
    * Collect Magma Pool with Infinite Speed if you have heat resistance
    * Go out of bounds at Gathering Hall or Watery Hall using Space Jump and wallcrawl to Hall of the Elders item, or Reflecting Pool
    * Collect Watery Hall Water item without any items
    * Collect Furnace Spider Track item without Power Bombs or Boost Ball if you have Space Jump
    * Collect Crossway item using Space Jump and Morph Ball only
    * 5-tank Varialess Magmoor with Space Jump
    * Various sections of Magmoor without Varia if you have enough E-Tanks
    * Cross Twin Fires Tunnel with only Space Jump
    * Geothermal Core without Grapple or Spider Ball
    * Get Phendrana Shorelines item and Chozo Item Temple item using Infinite Speed
    * Climb Chozo Ice Temple without Space Jump
    * Get Ice Ruins West Item without Space Jump
    * Climb Ruined Courtyard with only Space Jump
    * Go out of bounds in Ruined Courtyard using Space Jump and wallcrawl to Thardus
    * Leave Quarantine Cave in either direction with only Space Jump (and Wave)
    * Climb Transport to Magmoor Caverns South using Space Jump only
    * Climb Observatory using Space Jump only
    * Collect Control Tower without Plasma
    * Collect Gravity Chamber ledge item with Space Jump only
    * Cross Hunter Cave without Grapple using Space Jump
    * Collect Frost Cave without Grapple using Space Jump
    * Go out of bounds in Frost Cave using Space Jump and wallcrawl to Gravity Chamber, Transport Access, Security Cave or Storage Cave
    * Collect Main Quarry item without Spider Ball if you have Space Jump
    * Dash to Waste Disposal door
    * Climb Ore Processing with Space Jump and without Spider Ball
    * Climb both spider shafts in Phazon Mines with Space Jump and Bombs
    * Climb Phazon Processing Center with Space Jump and withuout Spider Ball
    * Get Metroid Quarantine A item with only Space jump
    * Cross Metroid Quarantine A with Space Jump and without Spider Ball
    * Get Mining Tunnel with standard Early Newborn E-Tank requirements
    * Traverse Metroid Quarantine B with only Space Jump
    * Go out of bounds in Metroid Quarantine B with Space Jump and wallcrawl to Phazon Processing Center or Fungal Hall B
* Very Hard
    * Go out of bounds in Tallon Canyon using Boost Ball and Bombs only, and then wallcrawl to Root Cave or Arbor Chamber
    * Climb Frigate Crash Site without Space Jump using Bombs
    * Go around the non-powered door in crashed frigate by going out of bounds using a UBJ
    * Climb Cargo Freight Lift to Deck Gamma with Gravity Suit and Bombs
    * Collect Life Grove Underwater item without Boost Ball, either using Bombs inbounds or going out of bounds with Space Jump
    * Cross Life Grove Tunnel and collect item without Boost using Bombs
    * Reach Great Tree Chamber from the top of Great Tree Hall without Space Jump
    * Go out of bounds in Great Tree Chamber using Bombs and wallcrawl to Life Grove or to Connection Elevator to Deck Beta to reach crashed frigate.
    * Reach Main Plaze Half Pipe Item using Bombs
    * Reach Ruined Shrine upper item with only Morph Ball
    * Climb to Ruined Shrine upper door with no items
    * Climb Tower of Light without Space Jump if you have enough missiles
    * Collect Magma Pool with Infinite Speed without heat resistance if you have enough E-Tanks
    * Go out of bounds at Gathering Hall or Watery Hall using bombs and wallcrawl to Hall of the Elders item, or Reflecting Pool
    * Bomb jump up the Crossway half-pipe
    * Obtain Crossway item without spider or space jump, using Supers, Boost Ball and Bombs
    * Reach the bomb slot in Hall of the Elders using Bombs and without Spider Ball or Space Jump
    * Climb Reflecting Pool using Bombs
    * Stricter Varialess requirements in Magmoor
    * Use Magmoor to dash across Twin Fires Tunnel without Space Jump
    * Several Magmoor rooms no longer require Space Jump to cross
    * Go out of bounds in Magmoor Workstation using Space Jump and wallcrawl to Plasma Processing or Mines elevator
    * Bomb jump to the area with the tower item in Phendrana Shorelines
    * Collect Ice Ruins East spider track item using Bombs
    * Reach the Ice Ruins West wave door using Bombs
    * Go out of bounds in Ice Ruins West using Bombs and wallcrawl to Ruined Courtyard or Quarantine Cave or Quarantine Monitor
    * Climb Ruined Courtyard with Bombs only
    * Climb Transport to Magmoor Caverns South using Bombs
    * Cross the top of Observatory without Space Jump
    * Collect the item at the top of Observatory, from top door, using Bombs
    * Climb from the bottom of Phendrana's Edge to Storage Cave without Space Jump
    * Reach Security Cave without Space Jump if you have Grapple
    * Climb to the top of Frozen Pike without Space Jump
    * Climb out of Gravity Chamber water without Gravity Suit, but with Space Jump
    * Collect Frost Cave item without Space Jump
    * Go out of bounds in Frost Cave without Space Jump and wallcrawl to Gravity Chamber, Transport Access, Security Cave or Storage Cave. The Transport Access item can be collected from out of bounds using Boost Ball
    * Go out of bounds in Security Access B using Bombs and wallcrawl to Storage Depot A, Security Access A, or Main Quarry
    * Climb Phazon Processing Center without Space Jump
    * Dash across the gap next to the power bomb rocks in Ore Processing without Space Jump, if needed so that you can use Grapple to get to the top.
    * Climb Central Dynamo without Space Jump
    * Climb Metroid Quarantine A without Space Jump
    * Reach Elite Control Access item using Bombs
    * Get Elite Research Laser item without Boost Ball
    * Climb Eliter Research without Space Jump
    * Climb Fungal Hall Access without Space Jump
    * Traverse Fungal Hall A without Space Jump
    * Get Mining Tunnel item with fewer E-Tanks (you may need to farm energy from the adjacent rooms)
    * Traverse Fungal Hall B using only Bombs
    * Traverse Metroid Quarantine B using only Bombs
    * Go out of bounds in Metroid Quarantine B without Space Jump and wallcrawl to Phazon Processing Center or Fungal Hall B
* Expert
    * Obtain Tallon Floaty using Infinite Speed in Landing Site. This may require you to float for an hour with lightshow. Then use Tallon Floaty to reach the inside of crashed frigate without ice beam from out of bounds
    * Go out of bounds in Tallon Canyon using Bombs only, and then wallcrawl to Root Cave or Arbor Chamber
    * Climb Great Tree Hall without Space Jump
    * Bypass the gate in Sun Tower using bombs
    * Go out of bounds in Burning Trail using Bombs only, and wallcrawl across all of Magmoor (including to the mines elevator) if necessary
    * Go out of bounds in Magmoor Workstation using Ice Beam and Bombs, and wallcrawl across all of Magmoor if necessary
    * May require inbounds Geothermal Core without boost if you get warped there without Wave Beam, but you have Ice Beam
    * Go out of bounds in Ice Ruins West using Bombs and wallcrawl to anywhere in Phendrana if necessary
    * Go out of bounds in Metroid Quarantine B without Space Jump and wallcrawl to Phazon Processing Center or Fungal Hall B or Fungal Hall Access
    * E-Tank requirements for Mining Tunnel are even stricter
