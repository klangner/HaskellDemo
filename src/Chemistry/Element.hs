module Chemistry.Element where



data Element = Element { atomicNumber :: Int
                        , symbol :: String
                        , name :: String
                        , weight :: Double } 
            
elements :: [Element]
elements = [ Element 1 "H" "Hydrogen" 1.008 
           , Element 2 "He" "Helium" 4.002602 
           , Element 3 "Li" "Lithium" 6.941 
           , Element 4 "Be" "Beryllium" 9.012182 
           , Element 5 "B" "Boron" 10.811 
           , Element 6 "C" "Carbon" 12.011 
           , Element 7 "N" "Nitrogen" 14.007 
           , Element 8 "O" "Oxygen" 15.999
           , Element 9 "F" "Fluorine" 18.9984032 
           , Element 10 "Ne" "Neon" 20.1797 
           , Element 11 "Na" "Sodium" 22.989768 
           , Element 12 "Mg" "Magnesium" 24.305 
           , Element 13 "Al" "Alluminium" 26.981
           , Element 14 "Si" "Silicon" 28.085
           , Element 15 "P" "Phosphous" 30.973
            -- S   Sulphur         16  32,066 
            -- Cl  Chlorine        17  35,4527 
            -- Ar  Argon       18  39,948 
            -- K   Potassium   German Kalium   19  39,0983 
            -- Ca  Calcium         20  40,078 
            -- Sc  Scandium        21  44,95591 
            -- Ti  Titanium        22  47,88 
            -- V   Vanadium        23  50,9415 
            -- Cr  Chromium        24  51,9961 
            -- Mn  Manganese       25  54,93805 
           , Element 26 "Fe" "Iron" 55.845
           , Element 27 "Co" "Cobalt" 58.933
           , Element 28 "Ni" "Nickel" 58.6934
           , Element 29 "Cu" "Copper" 63.546
            -- Zn  Zinc        30  65,39 
            -- Ga  Gallium         31  69,723 
            -- Ge  Germanium       32  72,61 
            -- As  Arsenic         33  74,92159 
            -- Se  Selenium        34  78,96 
            -- Br  Bromine     35  79,904 
            -- Kr  Krypton         36  83,8 
            -- Rb  Rubidium        37  85,4678 
            -- Sr  Strontium       38  87,62 
            -- Y   Yttrium         39  88,90585 
            -- Zr  Zirconium       40  91,224 
            -- Nb  Niobium         41  92,90638 
            -- Mo  Molybdenum      42  95,94 
            , Element  43 "Tc" "Technetium" 98.9063 
            -- Ru  Ruthenium       44  101,07 
            -- Rh  Rhodium         45  102,9055 
            -- Pd  Palladium       46  106,42 
            -- Ag  Silver  Latin Argentum  47  107,8682 
            -- Cd  Cadmium         48  112,411 
            -- In  Indium      49  114,82 
            -- Sn  Tin     Latin Stannum   50  118,71 
            -- Sb  Antimony    Latin Stibium   51  121,75 
            -- Te  Tellurium       52  127,6 
            -- I   Iodine      53  126,90447 
            -- Xe  Xenon       54  131,29 
            -- Cs  Caesium         55  132,90543 
            -- Ba  Barium      56  137,327 
            -- La  Lanthanum       57  138,9055 
            -- Ce  Cerium      58  140,115 
            -- Pr  Praseodymium        59  140,90765 
            -- Nd  Neodymium       60  144,24 
            , Element 61 "Pm" "Promethium" 146.9151 
            -- Sm  Samarium        62  150,36 
            -- Eu  Europium        63  151,965 
            -- Gd  Gadolinium      64  157,25 
            -- Tb  Terbium         65  158,92534 
            -- Dy  Dysprosium      66  162,5 
            -- Ho  Holmium         67  164,93032 
            -- Er  Erbium      68  167,26 
            -- Tm  Thulium         69  168,93421 
            -- Yb  Ytterbium       70  173,04 
            -- Lu  Lutetium        71  174,967 
            -- Hf  Hafnium         72  178,49 
            -- Ta  Tantalum        73  180,9479 
            -- W   Tungsten    German Wolfram  74  183,85 
            -- Re  Rhenium         75  186,207 
            -- Os  Osmium      76  190,2 
            -- Ir  Iridium         77  192,22 
            -- Pt  Platinum        78  195,08 
            -- Au  Gold    Latin Aurum     79  196,96654  
           , Element 80 "Hg" "Mercury" 200.59  
            -- Tl  Thallium        81  204,3833 
            -- Pb  Lead    Latin Plumbum   82  207,2 
            -- Bi  Bismuth         83  208,98037 
            -- Po  Polonium        84  208,9824 
            -- At  Astatine        85  209,9871 
            -- Rn  Radon       86  222,0176 
            -- Fr  Francium        87  223,0197 
            -- Ra  Radium      88  226,0254 
            -- Ac  Actinium        89  227,0278 
            , Element 90 "Th" "Thorium" 232.0381 
            -- Pa  Protactinium        91  231,0359 
            -- U   Uranium         92  238,0289 
            -- Np  Neptunium       93  237,0482 
            -- Pu  Plutonium       94  244,0642 
            -- Am  Americium       95  243,0614 
            -- Cm  Curium      96  247,0703 
            -- Bk  Berkelium       97  247,0703 
            -- Cf  Californium         98  251,0796 
            -- Es  Einsteinium         99  252,0829 
            -- Fm  Fermium         100 257,0951 
            -- Md  Mendelevium         101 258,0986 
            -- No  Nobelium        102 259,1009 
            -- Lr  Lawrencium      103 260,1053 
            , Element  104 "Rf" "Rutherfordium" 261.1087 
            -- Db  Dubnium         105 262,1138 
            -- Sg  Seaborgium      106 263,1182 
            -- Bh  Bohrium         107 262,1229 
            -- Hs  Hassium         108 265 
            -- Mt  Meitnerium      109 266 
            -- Ds  Darmstadtium        110 269 
            -- Rg  Roentgenium         111 272 
            -- Uub     Ununbium        112 277 
            -- Uut     Ununtrium       113 
            -- Uug     Ununquadium         114 
            -- Uup     Ununpentium         115 
            -- Uuh     Ununhexium      116 
            -- Uus     Ununseptium         117     
           , Element 118 "Uuo" "Ununoctium" 294 ]
           
