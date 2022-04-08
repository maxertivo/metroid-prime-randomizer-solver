module Parser where 
    import Node
    import Text.Read
    
    parse :: String ->  [Node]
    parse input =  createItemNodes $ createTuples $ handleExpansions $ shortenAreas $ removePunc $ removePrefixes $ removeEmpty $ split $ dropLines $ addDash $ lines input

    createItemNodes :: [(String,String,String)] -> [Node]
    createItemNodes ((a,b,c):rest) = Item (readS a::ItemId) (readS b::ItemName) (if c == "" then getDefaultWarp (readS a::ItemId) defaultWarps else readS c::RoomId) : createItemNodes rest
    createItemNodes [] = []

    getDefaultWarp :: ItemId -> [(RoomId,ItemId)] -> RoomId
    getDefaultWarp itemId ((room,item):rest) = if itemId == item then room else getDefaultWarp itemId rest
    getDefaultWarp itemId [] = error ("Couldn't find default warp for itemId:" ++ show itemId)

    readS :: Read a => String -> a
    readS str = case readMaybe str of
        Just result -> result
        Nothing -> error ("Cannot read string:" ++ str)

    handleExpansions :: [String] -> [String]
    handleExpansions (a:b:c:d:rest) = case removeNum c of
        "MissileExpansion" -> a:b:"Missile":d : handleExpansions rest
        "MissileLauncher" -> a:b:"Missile":d : handleExpansions rest
        "PowerBombExpansion" -> a:b:"PowerBomb":d : handleExpansions rest
        "EnergyTank" -> a:b:"EnergyTank":d : handleExpansions rest
        'A':'r':'t':'i':'f':'a':'c':'t':rest2 -> a:b:"Artifact":d : handleExpansions rest
        _ -> a:b:c:d : handleExpansions rest
    handleExpansions [] = []
    handleExpansions x = error "Expecting a list with a number of elements divisible by 4"

    removePrefixes :: [String] -> [String]
    removePrefixes = map (replacePrefix "Warps to:" "")

    shortenAreas :: [String] -> [String]
    shortenAreas = map
      (replacePrefix "PhendranaDrifts" "D"
         . replacePrefix "ChozoRuins" "R"
             . replacePrefix "PhazonMines" "M"
                 . replacePrefix "MagmoorCaverns" "C"
                     . replacePrefix "TallonOverworld" "O")

    replacePrefix :: String -> String -> String -> String
    replacePrefix prefix replacement string = case stripPrefix prefix string of
        Just b -> replacement ++ b
        Nothing -> string

    addDash :: [String] -> [String]
    addDash (a:rest) = if length a < 77 then (a ++ " - Warps to:") : addDash rest else a : addDash rest
    addDash [] = []

    createTuples :: [String] -> [(String,String,String)]
    createTuples (a:b:c:d:rest) = (b,c,d): createTuples rest
    createTuples x = [] 

    removeEmpty :: [String] -> [String]
    removeEmpty ("":rest) = removeEmpty rest
    removeEmpty (x:rest) = x : removeEmpty rest
    removeEmpty [] = []

    removePunc :: [String] -> [String]
    removePunc = map (\ xs -> [x | x <- xs, x `notElem` " ()\"-.|"])

    removeNum :: String -> String
    removeNum xs = [x | x <- xs, x `notElem` "1234567890"]

    split :: [String] -> [String]
    split = concatMap (splitOn "- ")

    splitOn :: String -> String -> [String]
    splitOn delim str = splitOnHelper delim str []

    splitOnHelper :: [Char] -> [Char] -> [Char] -> [[Char]]
    splitOnHelper delim [] accum = [accum]
    splitOnHelper delim (x:rest) accum = case stripPrefix delim (x:rest) of
        Just a ->  accum : splitOnHelper delim a []
        Nothing -> splitOnHelper delim rest (accum ++ [x])
    
    stripPrefix :: String -> String -> Maybe String
    stripPrefix [] str = Just str
    stripPrefix prefix [] = Nothing
    stripPrefix (x:prefix) (y:str) = if x == y then stripPrefix prefix str else Nothing

    startsWith :: String -> String -> Bool
    startsWith [] str = True
    startsWith prefix [] = False
    startsWith (x:prefix) (y:str) = x == y && startsWith prefix str

    dropLines :: [String] -> [String]
    dropLines [] = []
    dropLines (x:rest) = if startsWith "Chozo" x then dropElevators (x:rest) else dropLines rest

    dropElevators :: [String] -> [String]
    dropElevators [] = []
    dropElevators (x:rest) = if startsWith "Elevators:" x then [] else x : dropElevators rest

    defaultWarps :: [(RoomId,ItemId)]
    defaultWarps = [(RMainPlaza,MainPlazaHalfPipe)
                    ,(RMainPlaza,MainPlazaGrappleLedge)
                    ,(RMainPlaza,MainPlazaTree)
                    ,(RMainPlazaLedge,MainPlazaLockedDoor)
                    ,(RRuinedFountainNonWarp,RuinedFountain)
                    ,(RRuinedShrine,RuinedShrineBeetleBattle)
                    ,(RRuinedShrine,RuinedShrineHalfPipe)
                    ,(RRuinedShrine,RuinedShrineLowerTunnel)
                    ,(RVault,Vault)
                    ,(RTrainingChamber,TrainingChamber)
                    ,(RRuinedNursery,RuinedNursery)
                    ,(RTrainingChamberAccess,TrainingChamberAccess)
                    ,(RMagmaPool,MagmaPool)
                    ,(RTowerofLight,TowerofLight)
                    ,(RTowerChamber,TowerChamber)
                    ,(RRuinedGallery,RuinedGalleryMissileWall)
                    ,(RRuinedGallery,RuinedGalleryTunnel)
                    ,(RTransportAccessNorth,TransportAccessNorth)
                    ,(RGatheringHall,GatheringHall)
                    ,(RHiveTotem,HiveTotem)
                    ,(RSunchamber,SunchamberFlaahgra)
                    ,(RSunchamber,SunchamberGhosts)
                    ,(RWateryHallAccess,WateryHallAccess)
                    ,(RWateryHall,WateryHallScanPuzzle)
                    ,(RWateryHall,WateryHallUnderwater)
                    ,(RDynamo,DynamoLower)
                    ,(RDynamo,DynamoSpiderTrack)
                    ,(RBurnDome,BurnDomeMissile)
                    ,(RBurnDome,BurnDomeIDrone)
                    ,(RFurnace,FurnaceSpiderTracks)
                    ,(RFurnace,FurnaceInsideFurnace)
                    ,(RHalloftheElders,HalloftheElders)
                    ,(RCrossway,Crossway)
                    ,(RElderChamber,ElderChamber)
                    ,(RAntechamber,Antechamber)
                    ,(DPhendranaShorelines,PhendranaShorelinesBehindIce)
                    ,(DPhendranaShorelines,PhendranaShorelinesSpiderTrack)
                    ,(DChozoIceTemple,ChozoIceTemple)
                    ,(DIceRuinsWest,IceRuinsWest)
                    ,(DIceRuinsEast,IceRuinsEastBehindIce)
                    ,(DIceRuinsEast,IceRuinsEastSpiderTrack)
                    ,(DChapeloftheElders,ChapeloftheElders)
                    ,(DRuinedCourtyard,RuinedCourtyard)
                    ,(DPhendranaCanyon,PhendranaCanyon)
                    ,(DQuarantineCave,QuarantineCave)
                    ,(DResearchLabHydra,ResearchLabHydra)
                    ,(DQuarantineMonitor,QuarantineMonitor)
                    ,(DObservatory,Observatory)
                    ,(DTransportAccess,TransportAccess)
                    ,(DControlTower,ControlTower)
                    ,(DResearchCore,ResearchCore)
                    ,(DFrostCave,FrostCave)
                    ,(DResearchLabAether,ResearchLabAetherTank)
                    ,(DResearchLabAether,ResearchLabAetherMorphTrack)
                    ,(DGravityChamber,GravityChamberUnderwater)
                    ,(DGravityChamber,GravityChamberGrappleLedge)
                    ,(DStorageCave,StorageCave)
                    ,(DSecurityCave,SecurityCave)
                    ,(OLandingSite,LandingSite)
                    ,(OAlcove,Alcove)
                    ,(OFrigateCrashSite,FrigateCrashSite)
                    ,(OOvergrownCavern,OvergrownCavern)
                    ,(ORootCave,RootCave)
                    ,(OArtifactTemple,ArtifactTemple)
                    ,(OTransportTunnelB,TransportTunnelB)
                    ,(OArborChamber,ArborChamber)
                    ,(OCargoFreightLifttoDeckGamma,CargoFreightLifttoDeckGamma)
                    ,(OBiohazardContainment,BiohazardContainment)
                    ,(OHydroAccessTunnel,HydroAccessTunnel)
                    ,(OGreatTreeChamber,GreatTreeChamber)
                    ,(OLifeGroveTunnel,LifeGroveTunnel)
                    ,(OLifeGrove,LifeGroveStart)
                    ,(OLifeGrove,LifeGroveUnderwaterSpinner)
                    ,(MMainQuarry,MainQuarry)
                    ,(MSecurityAccessA,SecurityAccessA)
                    ,(MStorageDepotB,StorageDepotB)
                    ,(MStorageDepotA,StorageDepotA)
                    ,(MEliteResearch,EliteResearchPhazonElite)
                    ,(MEliteResearch,EliteResearchLaser)
                    ,(MEliteControlAccess,EliteControlAccess)
                    ,(MVentilationShaft,VentilationShaft)
                    ,(MPhazonProcessingCenter,PhazonProcessingCenter)
                    ,(MProcessingCenterAccess,ProcessingCenterAccess)
                    ,(MEliteQuarters,EliteQuarters)
                    ,(MCentralDynamo,CentralDynamo)
                    ,(MMetroidQuarantineB,MetroidQuarantineB)
                    ,(MMetroidQuarantineA,MetroidQuarantineA)
                    ,(MFungalHallB,FungalHallB)
                    ,(MPhazonMiningTunnel,PhazonMiningTunnel)
                    ,(MFungalHallAccess,FungalHallAccess)
                    ,(CLavaLake,LavaLake)
                    ,(CTriclopsPit,TriclopsPit)
                    ,(CStorageCavern,StorageCavern)
                    ,(CTransportTunnelA,TransportTunnelA)
                    ,(CWarriorShrine,WarriorShrine)
                    ,(CShoreTunnel,ShoreTunnel)
                    ,(CFieryShores,FieryShoresMorphTrack)
                    ,(CFieryShores,FieryShoresWarriorShrineTunnel)
                    ,(CPlasmaProcessing,PlasmaProcessing)
                    ,(CMagmoorWorkstation,MagmoorWorkstation)]