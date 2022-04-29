rd /s /q "build"
mkdir build
ghc app/Main.hs --make -Wall -O2 -hide-all-packages -package base -package containers -package directory -i"./src" -hidir "./build" -odir "./build" -o ./build/metroid-prime-randomizer-solver
.\build\metroid-prime-randomizer-solver.exe