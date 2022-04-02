rd /s /q "build"
mkdir build
ghc app/Main.hs --make -i"./src":"./app" -hidir "./build" -odir "./build" -o ./build/mp-warp-rando-checker
.\build\mp-warp-rando-checker.exe