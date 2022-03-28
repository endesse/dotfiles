  -- Base
import XMonad
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (copyToAll, kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
-- import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docks, docksEventHook, ToggleStruts(..))
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

   -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn)
import XMonad.Util.SpawnOnce

------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------
-- It's nice to assign values to stuff that you will use more than once
-- in the config. Setting values for things like font, terminal and editor
-- means you only have to change the value here to make changes globally.

myFont :: String
myFont = "xft:Mononoki Nerd Font:bold:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask       -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "urxvtc"   -- Sets default terminal

myBrowser :: String
myBrowser = "qutebrowser "               -- Sets qutebrowser as browser for tree select
-- myBrowser = myTerminal ++ " -e lynx " -- Sets lynx as browser for tree select

myEditor :: String
-- myEditor = "emacsclient -c -a emacs "  -- Sets emacs as editor for tree select
myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor for tree select

myBorderWidth :: Dimension
myBorderWidth = 2          -- Sets border width for windows

myNormColor :: String
myNormColor   = "#282c34"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#bbc5ff"  -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts

-- Colors for polybar
color1, color2, color3, color4 :: String
--color1 = "#7F7F7F"
--color2 = "#c792ea"
--color3 = "#900000"
--color4 = "#2E9AFE"
color1 = "#FF5722" -- Inactive
color2 = "#FF8A65" -- Window Status
color3 = "#FF7043"
color4 = "#FFAB91" -- Active


------------------------------------------------------------------------
-- AUTOSTART
------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
	  spawnOnce "wal -n -R -q &"
	  spawnOnce "urxvtd -q -o -f &"
	  spawnOnce "numlockx &"
          spawnOnce "nitrogen --restore &"
          spawnOnce "picom &"
	  spawnOnce "lxsession &"
	  spawnOnce "pcmanfm -d &"
          spawnOnce "nm-applet --no-agent &"
          spawnOnce "volumeicon &"
          spawnOnce "xscreensaver -nosplash &"
	  spawnOnce "birdtray &"
	  spawnOnce "imwheel &"
	  spawnOnce "~/.config/polybar/launch.sh &"
	  -- TT Office
	  --spawnOnce "anydesk &"
	  --spawnOnce "skypeforlinux &"
	  --spawnOnce "firefox &"
	  --
	  --setWMName "compiz"
	  setWMName "LG3D"


------------------------------------------------------------------------
-- GRID SELECT
------------------------------------------------------------------------
-- GridSelect displays items (programs, open windows, etc.) in a 2D grid
-- and lets the user select from it with the cursor/hjkl keys or the mouse.

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x28,0x2c,0x34) -- lowest inactive bg
                  (0x28,0x2c,0x34) -- highest inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x28,0x2c,0x34) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

myAppGrid = [ ("Qutebrowser", "qutebrowser")
                 , ("Firefox", "firefox")
                 , ("Gimp", "gimp")
		 , ("Krita", "krita")
		 , ("Inkscape", "inkscape")
                 , ("LibreOffice Impress", "loimpress")
		 , ("LibreOffice Calc", "localc")
                 , ("LibreOffice Writer", "lowriter")
		 , ("LibreOffice Draw", "lodraw")
                 , ("OBS", "obs")
		 , ("Discord", "discord")
		 , ("Steam", "steam")
		 , ("Visual Studio Code", "code")
		 , ("Blender", "blender")
		 , ("Natron", "Natron")
		 , ("PCmanFM", "pcmanfm")
		 , ("Joplin", "joplin-desktop")
		 , ("Davinci Resolve", "/opt/resolve/bin/resolve")
		 , ("DigiKam", "digikam")
		 , ("Fluent Reader", "fluent-reader")
                 ]

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
   [ Node (TS.TSNode "+ 3D" "3D Manipulation Software" (return ()))
       [ Node (TS.TSNode "ArmorPaint" "ArmorPaint is a software for 3D PBR texture painting" (spawn "armorpaint")) []
       , Node (TS.TSNode "Blender" "Open 3D Creation Suite" (spawn "blender")) []
       , Node (TS.TSNode "Dust3D" "3D watertight modeling software" (spawn "dust3d")) []
       , Node (TS.TSNode "FreeCAD" "General purpose 3D CAD modeler" (spawn "freecad")) []
       , Node (TS.TSNode "FSpy" "cross platform app for still image camera matching" (spawn "fspy")) []
       , Node (TS.TSNode "Goxel" "3D program that lets you create voxel volumes" (spawn "goxel")) []
       , Node (TS.TSNode "Make Human" "Parametrical modeling program for creating human bodies" (spawn "makehuman")) []
       , Node (TS.TSNode "Quixel Bridge" "A tool for browsing, searching, downloading, importing and exporting Megascans assets" (spawn "quixel-bridge")) []
       , Node (TS.TSNode "TextureLab" "Procedural texture generation (Substance Designer Alternative)" (spawn "/opt/texturelab_030/texturelab")) []
       ]
   , Node (TS.TSNode "+ Accessories" "Accessory applications" (return ()))
       [ Node (TS.TSNode "Archive Manager" "Tool for archived packages" (spawn "file-roller")) []
       , Node (TS.TSNode "Bottles" "Easily manage wine and proton prefix" (spawn "bottles")) []
       , Node (TS.TSNode "Calculator" "Gui version of qalc" (spawn "qalculate-gtk")) []
       , Node (TS.TSNode "Carla" "Audio Plugin Host" (spawn "carla")) []
       , Node (TS.TSNode "CopyQ" "Clipboard manager with searchable and editable history" (spawn "copyq")) []
       , Node (TS.TSNode "Deckboard" "Streamdeck alternative - Create macros and launch them on mobile devices" (spawn "deckboard")) []
       , Node (TS.TSNode "Goverlay" "A GUI to help manage Vulkan/OpenGL overlays" (spawn "goverlay")) []
       , Node (TS.TSNode "ImWheel" "Mouse wheel configuration tool for XFree86/Xorg" (spawn (myTerminal ++ " -e sh -c 'imwheel -h; bash'"))) []
       , Node (TS.TSNode "InklingReader" "A GNU/Linux-friendly version of the Wacom Inkling SketchManager." (spawn "inklingreader")) []
       , Node (TS.TSNode "k3b" "Feature-rich and easy to handle CD burning application" (spawn "k3b")) []
       , Node (TS.TSNode "OpenRGB" "Configuration utility for RGB lights supporting motherboards, RAM, & peripherals" (spawn "openrgb")) []
       , Node (TS.TSNode "Pensela" "Screen annotation tool" (spawn "pensela")) []
       , Node (TS.TSNode "Picom Toggle on/off" "Compositor for window managers" (spawn "killall picom; picom")) []
       , Node (TS.TSNode "PureRef" "Reference Image Viewer" (spawn "PureRef")) []
       , Node (TS.TSNode "RivalCFG" "CLI tool and Python library to configure SteelSeries gaming mice" (spawn (myTerminal ++ " -e sh -c 'rivalcfg --help; bash'"))) []
       , Node (TS.TSNode "Virtualbox" "Oracle's virtualization program" (spawn "virtualbox")) []
       ]
   , Node (TS.TSNode "+ Games" "fun and games" (return ()))
       [ Node (TS.TSNode "0 A.D." "Cross-platform, 3D and historically-based real-time strategy game" (spawn "0ad")) []
       , Node (TS.TSNode "Dungeon Revealer" "Web app for tabletop gaming to allow the DM to reveal areas of the game map to players" (spawn (myTerminal ++ " -e sh -c 'dungeon-revealer; bash'"))) []
       , Node (TS.TSNode "Endless Sky" "A sandbox-style space exploration and combat game" (spawn "endless-sky")) []
       , Node (TS.TSNode "Hedgewars" "Turn-based strategy artillery game similiar to Worms" (spawn "hedgewars")) []
       , Node (TS.TSNode "Heroic" "HGL, a Native alternative Linux Launcher for Epic Games" (spawn "heroic")) []
       , Node (TS.TSNode "Lutris" "Open gaming platform" (spawn "lutris")) []
       , Node (TS.TSNode "Openra Command & Conquer" "An open-source implementation of the Command & Conquer" (spawn "openra-cnc")) []
       , Node (TS.TSNode "Openra Dune 2000" "An open-source implementation of the Dune 2000" (spawn "openra-d2k")) []
       , Node (TS.TSNode "Openra Red Alert" "An open-source implementation of the Red Alert" (spawn "openra-ra")) []
       , Node (TS.TSNode "OpenTTD" "Open source simulation game based upon Transport Tycoon Deluxe" (spawn "openttd")) []
       , Node (TS.TSNode "Pingus" "A Lemmings clone, i.e. a level-based puzzle game." (spawn "pingus")) []
       , Node (TS.TSNode "Rare" "A GUI for legendary, an open source replacement for Epic Games Launcher" (spawn "rare")) []
       , Node (TS.TSNode "Simutrans" "Transportation simulation game" (spawn "simutrans")) []
       , Node (TS.TSNode "Steam" "Steam gaming platform" (spawn "steam")) []
       , Node (TS.TSNode "SuperTux Kart" "Kart racing game featuring Tux and his friends" (spawn "supertuxkart")) []
       , Node (TS.TSNode "The Dark Mod" "Thief style universe using idTech4" (spawn "thedarkmod")) []
       , Node (TS.TSNode "The Lost Vikings" "Blizzard's The Lost Vikings" (spawn "dosbox /opt/VIKINGS/VIKINGS/VIKINGS.EXE")) []
       , Node (TS.TSNode "Xonotic" "A free, fast-paced crossplatform first-person shooter" (spawn "xonotic-sdl")) []
       ]
   , Node (TS.TSNode "+ Graphics" "graphics programs" (return ()))
       [ Node (TS.TSNode "Akira" "UI/UX Design" (spawn "akira")) []
       , Node (TS.TSNode "Birdfont" "Font editor which can generate fonts in TTF, EOT and SVG formats" (spawn "birdfont")) []
       , Node (TS.TSNode "Darktable" "Digital Darkroom - Lightwave alternative" (spawn "darktable")) []
       , Node (TS.TSNode "DigiKam" "An advanced digital photo management application" (spawn "digikam")) []
       , Node (TS.TSNode "Enve" "2D animation software" (spawn "enve")) []
       , Node (TS.TSNode "Gimp" "GNU image manipulation program" (spawn "gimp")) []
       , Node (TS.TSNode "Gthumb" "Image browser and viewer | Batch resizer" (spawn "gthumb")) []
       , Node (TS.TSNode "Inkscape" "An SVG editing program" (spawn "inkscape")) []
       , Node (TS.TSNode "Krita" "Digital painting program" (spawn "krita")) []
       , Node (TS.TSNode "Scribus" "Desktop publishing software" (spawn "scribus")) []
       , Node (TS.TSNode "Simple Scan" "A simple scanning program" (spawn "simple-scan")) []
       , Node (TS.TSNode "Valentina" "Sewing pattern drafting tool aiming to remake the garment industry" (spawn "valentina")) []
       ]
   , Node (TS.TSNode "+ Internet" "internet and web programs" (return ()))
       [ Node (TS.TSNode "Discord" "Chat and video chat platform" (spawn "discord")) []
       , Node (TS.TSNode "Exodus" "Bitcoin wallet" (spawn "exodus")) []
       , Node (TS.TSNode "FileZilla" "An FTP client" (spawn "filezilla")) []
       , Node (TS.TSNode "Firefox" "Open source web browser" (spawn "firefox")) []
       , Node (TS.TSNode "Hexchat" "A popular and easy to use graphical IRC (chat) client" (spawn "hexchat")) []
       , Node (TS.TSNode "Jitsi Meet Desktop" "Open source video chat" (spawn "jitsi-meet-desktop")) []
       , Node (TS.TSNode "LBRY" "LBRY is a blockchain-based file-sharing and payment network that powers decentralized platforms" (spawn "/opt/lbry-desktop/dist/electron/LBRY_0.51.2.AppImage")) []
       , Node (TS.TSNode "Quassel" "Next-generation distributed IRC client" (spawn "quassel")) []
       , Node (TS.TSNode "Qutebrowser" "Minimal web browser" (spawn "qutebrowser")) []
       , Node (TS.TSNode "Skype" "Skype for Linux" (spawn "skypeforlinux")) []
       , Node (TS.TSNode "Tangram" "Run web apps on your desktop" (spawn "tangram")) []
       , Node (TS.TSNode "Microsoft Teams" "Microsoft Teams for Linux" (spawn "teams")) []
       , Node (TS.TSNode "Transmission" "Bittorrent client" (spawn "transmission-gtk")) []
       , Node (TS.TSNode "Youtube-DL" "Download youtube videos and more" (spawn (myTerminal ++ " -e sh -c 'youtube-dl --help; bash'"))) []
       , Node (TS.TSNode "Xfreerdp" "Remote desktop application" (spawn (myTerminal ++ " -e sh -c 'xfreerdp --help; bash'"))) []
       ]
   , Node (TS.TSNode "+ Multimedia" "sound and video applications" (return ()))
       [ Node (TS.TSNode "Alsa Mixer" "Alsa volume control utility" (spawn (myTerminal ++ " -e alsamixer"))) []
       , Node (TS.TSNode "Ardour" "Professional-grade digital audio workstation" (spawn "ardour6")) []
       , Node (TS.TSNode "Audacity" "Graphical audio editing program" (spawn "audacity")) []
       , Node (TS.TSNode "Blanket" "Improve focus and increase your productivity by listening to different sounds." (spawn "blanket")) []
       , Node (TS.TSNode "Davinci Resolve" "Professional A/V post-production software suite from Blackmagic Design" (spawn "/opt/resolve/bin/resolve")) []
       , Node (TS.TSNode "Deskreen" "Turns any device with a web browser to a second screen for your computer" (spawn "deskreen")) []
       , Node (TS.TSNode "Handreak" "Multithreaded video transcoder" (spawn "ghb")) []
       , Node (TS.TSNode "Kdenlive" "Open source non-linear video editor" (spawn "kdenlive")) []
       , Node (TS.TSNode "LMMS" "Digital audio Workstation DAW" (spawn "lmms")) []
       , Node (TS.TSNode "MPV" "Video Player" (spawn "mpv --player-operation-mode=pseudo-gui")) []
       , Node (TS.TSNode "Natron" "Compositing for VFX and Mograph" (spawn "Natron")) []
       , Node (TS.TSNode "OBS Studio" "Open Broadcaster Software" (spawn "obs")) []
       , Node (TS.TSNode "Photofilmstrip" "Create video clips from photos" (spawn "photofilmstrip")) []
       , Node (TS.TSNode "QtQR" "Creating and decoding QR codes" (spawn "qtqr")) []
       , Node (TS.TSNode "Rhythmbox" "Music playback and management application" (spawn "rhythmbox")) []
       , Node (TS.TSNode "Shutter Encoder" "Converter for all formats video|audio|image professionnals codecs and standards - swiss knife tool for Linux" (spawn "shutter-encoder")) []
       , Node (TS.TSNode "Sonic Visualiser" "Waveform visualiser" (spawn "sonic-visualiser")) []
       , Node (TS.TSNode "Subtitld" "An open source software to create, edit and transcribe subtitles." (spawn "subtitld")) []
       , Node (TS.TSNode "Synfig Studio" "Professional vector animation program" (spawn "synfigstudio")) []
       , Node (TS.TSNode "Termv" "A terminal iptv player written in bash" (spawn (myTerminal ++ " -e sh -c 'termv; bash'"))) []
       , Node (TS.TSNode "Vimix" "Live Video Mixing" (spawn "vimix")) []
       , Node (TS.TSNode "VLC" "Multimedia player and server" (spawn "vlc")) []
       ]
   , Node (TS.TSNode "+ Office" "office applications" (return ()))
       [ Node (TS.TSNode "Calibre" "Ebook management application" (spawn "calibre")) []
       , Node (TS.TSNode "Fluent Reader" "Modern desktop RSS reader built with Electron, React, and Fluent UI" (spawn "fluent-reader")) []
       , Node (TS.TSNode "GhostWriter" "Aesthetic, distraction-free Markdown editor" (spawn "ghostwriter")) []
       , Node (TS.TSNode "GimageReader" "Qt front-end to tesseract-ocr (OCR from images or PDF)" (spawn "gimagereader-qt5")) []
       , Node (TS.TSNode "Joplin" "Cross-platform note taking and to-do app" (spawn "joplin-desktop")) []
       , Node (TS.TSNode "LanguageTool" "An open source language checker" (spawn "languagetool")) []
       , Node (TS.TSNode "LibreOffice" "Open source office suite" (spawn "libreoffice")) []
       , Node (TS.TSNode "LibreOffice Base" "Desktop database front end" (spawn "lobase")) []
       , Node (TS.TSNode "LibreOffice Calc" "Spreadsheet program" (spawn "localc")) []
       , Node (TS.TSNode "LibreOffice Draw" "Diagrams and sketches" (spawn "lodraw")) []
       , Node (TS.TSNode "LibreOffice Impress" "Presentation program" (spawn "loimpress")) []
       , Node (TS.TSNode "LibreOffice Math" "Formula editor" (spawn "lomath")) []
       , Node (TS.TSNode "LibreOffice Writer" "Word processor" (spawn "lowriter")) []
       , Node (TS.TSNode "Manuskript" "Novel writing software" (spawn "manuskript")) []
       , Node (TS.TSNode "Thunderbird" "Standalone mail and news reader from mozilla.org" (spawn "thunderbird")) []
       , Node (TS.TSNode "Zathura" "PDF Viewer" (spawn "zathura")) []
       , Node (TS.TSNode "Zotero" "Collect, organize, cite, and share your research" (spawn "zotero")) []
       ]
   , Node (TS.TSNode "+ Pentest" "Pentesting tools" (return()))
       [ Node (TS.TSNode "+ Cracking" "Set of Cracking tools" (return()))
       		[ Node (TS.TSNode "Hashcat" "Multithreaded advanced password recovery utility" (spawn (myTerminal ++ " -e sh -c 'hashcat --help; bash'"))) []
		, Node (TS.TSNode "Hydra" "Very fast network logon cracker which support many different services" (spawn (myTerminal ++ " -e sh -c 'hydra; bash'"))) []
       		, Node (TS.TSNode "John" "John the Ripper password cracker" (spawn (myTerminal ++ "  -e sh -c 'john; bash'"))) []
		]
       , Node (TS.TSNode "+ Enumeration" "Tools for system and vulnerabilities enumerations" (return()))
       		[ Node (TS.TSNode "Dirbuster" "An application designed to brute force directories and files names on web/application servers" (spawn "dirbuster")) []
		, Node (TS.TSNode "Enum4Linux" "Tool for enumerating information from Windows and Samba systems." (spawn (myTerminal ++ " -e sh -c 'enum4linux; bash'"))) []
       		, Node (TS.TSNode "Gobuster" "A directory/file & DNS busting tool." (spawn (myTerminal ++ " -e sh -c 'gobuster; bash'"))) []
       		, Node (TS.TSNode "Nmap" "Utility for network discovery and security auditing" (spawn (myTerminal ++ " -e sh -c 'nmap; bash'"))) []
		, Node (TS.TSNode "MyCli" "MySQL Enumeration tool" (spawn (myTerminal ++ " -e sh -c 'mycli --help; bash'"))) []
		, Node (TS.TSNode "SQLMap" "Automatic SQL injection and database takeover tool" (spawn (myTerminal ++ " -e sh -c 'sqlmap; bash'"))) []
		, Node (TS.TSNode "Wfuzz" "Utility to bruteforce web applications" (spawn (myTerminal ++ " -e sh -c 'wfuzz --help; bash'"))) []
		, Node (TS.TSNode "OWASP ZAP" "Integrated penetration testing tool for finding vulnerabilities in web applications" (spawn "zaproxy")) []
		]
       , Node (TS.TSNode "+ Exploit" "Tools for post exploitation" (return()))
       		[ Node (TS.TSNode "Exploit DB" "Offensive Security’s Exploit Database Archive" (spawn (myTerminal ++ " -e sh -c 'searchsploit; bash'"))) []
       		, Node (TS.TSNode "Metasploit" "Advanced open-source platform for developing, testing, and using exploit code" (spawn (myTerminal ++ " -e msfconsole"))) []
		, Node (TS.TSNode "Netcat" "TCP/IP swiss army knife." (spawn (myTerminal ++ " -e sh -c 'nc -h; bash'"))) []
		]
       , Node (TS.TSNode "+ Wordlists" "Various wordlists for pentesting" (return()))
       		[ Node (TS.TSNode "Cewl" "Generate wordlists from URL" (spawn (myTerminal ++ " -e sh -c 'cewl --help; bash'"))) []
		, Node (TS.TSNode "Curl" "Generate Wordlists" (spawn (myTerminal ++ " -e sh -c 'curl --help; bash'"))) []
		, Node (TS.TSNode "Rockyou" "The famous Rockyou wordlist for users/passwords" (spawn (myTerminal ++ " -e vifmrun /usr/share/dict/"))) []
		, Node (TS.TSNode "Seclists" "A compilation of various seclists" (spawn (myTerminal ++ " -e vifmrun /usr/share/seclists/"))) []
		]
       , Node (TS.TSNode "Burp Suite" "An integrated platform for performing security testing of web applications (free edition)" (spawn "burpsuite")) []
       , Node (TS.TSNode "StegHide" "Embeds a message in a file by replacing some of the least significant bits" (spawn (myTerminal ++ " -e sh -c 'steghide --help; bash'"))) []
       , Node (TS.TSNode "StegSeek" "Lightning fast steghide cracker" (spawn (myTerminal ++ " -e sh -c 'stegseek --help; bash'"))) []
       , Node (TS.TSNode "Wireshark" "Network traffic and protocol analyzer/sniffer" (spawn "wireshark")) []
       ]
   , Node (TS.TSNode "+ Programming" "programming and scripting tools" (return ()))
        [Node (TS.TSNode "Arduino" "Arduino prototyping platform SDK" (spawn "arduino")) []
	, Node (TS.TSNode "Fritzing" "PCB layout prototyping application" (spawn "Fritzing")) []
	, Node (TS.TSNode "Google WebDesigner" "Create engaging, interactive HTML5-based designs and motion graphics that can run on any device." (spawn "google-webdesigner")) []
	, Node (TS.TSNode "Glade" "User Interface Builder for GTK+ applications" (spawn "glade")) []
	, Node (TS.TSNode "Godot" "Advanced cross-platform 2D and 3D game engine" (spawn "godot")) []
	, Node (TS.TSNode "Python" "Python interactive prompt" (spawn (myTerminal ++ " -e python"))) []
	, Node (TS.TSNode "Visual Studio Code" "Coding IDE from Microsoft" (spawn "code")) []
	, Node (TS.TSNode "Vysor" "Mirror and control your Android device" (spawn "vysor")) []
	, Node (TS.TSNode "XOD" "XOD is a visual programming language for microcontrollers." (spawn "/opt/'XOD IDE'/xod-client-electron")) []
       ]
   , Node (TS.TSNode "+ System" "system tools and utilities" (return ()))
       [ Node (TS.TSNode "Arandr" "Provide a simple visual front end for XRandR" (spawn "arandr")) []
       , Node (TS.TSNode "Cartão de Cidadão" "Cartão Cidadão - Gov PT" (spawn "eidguiV2")) []
       , Node (TS.TSNode "Glances" "Terminal system monitor" (spawn (myTerminal ++ " -e glances"))) []
       , Node (TS.TSNode "Gufw" "GUI uncomplicated firewall" (spawn "gufw")) []
       , Node (TS.TSNode "Htop" "Terminal process viewer" (spawn (myTerminal ++ " -e htop"))) []
       , Node (TS.TSNode "KDE Connect" "Adds communication between KDE and your smartphone" (spawn "kdeconnect-app")) []
       , Node (TS.TSNode "KcolorChooser" "System color picker" (spawn "kcolorchooser")) []
       , Node (TS.TSNode "LXAppearance" "Customize look and feel" (spawn "lxappearance")) []
       , Node (TS.TSNode "Nitrogen" "Wallpaper viewer and setter" (spawn "nitrogen")) []
       , Node (TS.TSNode "Nmon" "Network monitor" (spawn (myTerminal ++ " -e nmon"))) []
       , Node (TS.TSNode "Nvidia Settings" "Tool for configuring the NVIDIA graphics driver" (spawn "nvidia-settings")) []
       , Node (TS.TSNode "PCmanFM" "GUI File manager" (spawn "pcmanfm")) []
       , Node (TS.TSNode "Stacer" "Linux System Optimizer" (spawn "stacer")) []
       , Node (TS.TSNode "Vifm" "Vim-like file manager" (spawn (myTerminal ++ " -e vifmrun"))) []
       ]
   , Node (TS.TSNode "------------------------" "" (spawn "xdotool key Escape")) []
   , Node (TS.TSNode "+ Bookmarks" "a list of web bookmarks" (return ()))
       [ Node (TS.TSNode "+ Linux" "a list of web bookmarks" (return ()))
           [ Node (TS.TSNode "+ Arch Linux" "btw, i use arch!" (return ()))
               [ Node (TS.TSNode "Arch Linux" "Arch Linux homepage" (spawn (myBrowser ++ "https://www.archlinux.org/"))) []
               , Node (TS.TSNode "Arch Wiki" "The best Linux wiki" (spawn (myBrowser ++ "https://wiki.archlinux.org/"))) []
               , Node (TS.TSNode "AUR" "Arch User Repository" (spawn (myBrowser ++ "https://aur.archlinux.org/"))) []
               , Node (TS.TSNode "Arch Forums" "Arch Linux web forum" (spawn (myBrowser ++ "https://bbs.archlinux.org/"))) []
               ]
           , Node (TS.TSNode "+ Linux News" "linux news and blogs" (return ()))
               [ Node (TS.TSNode "DistroWatch" "Linux distro release announcments" (spawn (myBrowser ++ "https://distrowatch.com/"))) []
               , Node (TS.TSNode "LXer" "LXer linux news aggregation" (spawn (myBrowser ++ "http://lxer.com"))) []
               , Node (TS.TSNode "OMG Ubuntu" "Ubuntu news, apps and reviews" (spawn (myBrowser ++ "https://www.omgubuntu.co.uk"))) []
               ]
               , Node (TS.TSNode "+ XMonad" "xmonad documentation" (return ()))
                   [ Node (TS.TSNode "XMonad" "Homepage for XMonad" (spawn (myBrowser ++ "http://xmonad.org"))) []
                   , Node (TS.TSNode "XMonad GitHub" "The GitHub page for XMonad" (spawn (myBrowser ++ "https://github.com/xmonad/xmonad"))) []
                   , Node (TS.TSNode "xmonad-contrib" "Third party extensions for XMonad" (spawn (myBrowser ++ "https://hackage.haskell.org/package/xmonad-contrib"))) []
                   , Node (TS.TSNode "xmonad-ontrib GitHub" "The GitHub page for xmonad-contrib" (spawn (myBrowser ++ "https://github.com/xmonad/xmonad-contrib"))) []
                   , Node (TS.TSNode "Xmobar" "Minimal text-based status bar"  (spawn (myBrowser ++ "https://hackage.haskell.org/package/xmobar"))) []
                   ]
               ]
	   , Node (TS.TSNode "PhotoPea" "Online Photoshop Alternative" (spawn (myBrowser ++ "https://www.photopea.com"))) []
	   , Node (TS.TSNode "Vectr" "Online Vector Application" (spawn (myBrowser ++ "https://vectr.com"))) []
	   , Node (TS.TSNode "Daz3D" "3D Content Library" (spawn (myBrowser ++ "https://www.daz3d.com"))) []
           ]
       , Node (TS.TSNode "+ Search and Reference" "Search engines, indices and wikis" (return ()))
           [ Node (TS.TSNode "DuckDuckGo" "Privacy-oriented search engine" (spawn (myBrowser ++ "https://duckduckgo.com/"))) []
           , Node (TS.TSNode "Google" "The evil search engine" (spawn (myBrowser ++ "http://www.google.com"))) []
           , Node (TS.TSNode "Thesaurus" "Lookup synonyms and antonyms" (spawn (myBrowser ++ "https://www.thesaurus.com/"))) []
           , Node (TS.TSNode "Wikipedia" "The free encyclopedia" (spawn (myBrowser ++ "https://www.wikipedia.org/"))) []
           ]
       , Node (TS.TSNode "+ Programming" "programming and scripting" (return ()))
           [ Node (TS.TSNode "+ Bash and Shell Scripting" "shell scripting documentation" (return ()))
               [ Node (TS.TSNode "GNU Bash" "Documentation for bash" (spawn (myBrowser ++ "https://www.gnu.org/software/bash/manual/"))) []
               , Node (TS.TSNode "r/bash" "Subreddit for bash" (spawn (myBrowser ++ "https://www.reddit.com/r/bash/"))) []
               , Node (TS.TSNode "r/commandline" "Subreddit for the command line" (spawn (myBrowser ++ "https://www.reddit.com/r/commandline/"))) []
               , Node (TS.TSNode "Learn Shell" "Interactive shell tutorial" (spawn (myBrowser ++ "https://www.learnshell.org/"))) []
               ]
         , Node (TS.TSNode "+ Haskell" "haskell documentation" (return ()))
             [ Node (TS.TSNode "Haskell.org" "Homepage for haskell" (spawn (myBrowser ++ "http://www.haskell.org"))) []
             , Node (TS.TSNode "Hoogle" "Haskell API search engine" (spawn "https://hoogle.haskell.org/")) []
             , Node (TS.TSNode "r/haskell" "Subreddit for haskell" (spawn (myBrowser ++ "https://www.reddit.com/r/Python/"))) []
             , Node (TS.TSNode "Haskell on StackExchange" "Newest haskell topics on StackExchange" (spawn (myBrowser ++ "https://stackoverflow.com/questions/tagged/haskell"))) []
             ]
         , Node (TS.TSNode "+ Python" "python documentation" (return ()))
             [ Node (TS.TSNode "Python.org" "Homepage for python" (spawn (myBrowser ++ "https://www.python.org/"))) []
             , Node (TS.TSNode "r/Python" "Subreddit for python" (spawn (myBrowser ++ "https://www.reddit.com/r/Python/"))) []
             , Node (TS.TSNode "Python on StackExchange" "Newest python topics on StackExchange" (spawn (myBrowser ++ "https://stackoverflow.com/questions/tagged/python"))) []
             ]
         ]
       , Node (TS.TSNode "+ Vim" "vim and neovim documentation" (return ()))
           [ Node (TS.TSNode "Vim.org" "Vim, the ubiquitous text editor" (spawn (myBrowser ++ "https://www.vim.org/"))) []
           , Node (TS.TSNode "r/Vim" "Subreddit for vim" (spawn (myBrowser ++ "https://www.reddit.com/r/vim/"))) []
           , Node (TS.TSNode "Vi/m StackExchange" "Vi/m related questions" (spawn (myBrowser ++ "https://vi.stackexchange.com/"))) []
           ]
   , Node (TS.TSNode "+ Config Files" "config files that edit often" (return ()))
       [ Node (TS.TSNode "+ xmobar configs" "My xmobar config files" (return ()))
           [ Node (TS.TSNode "xmobar mon1" "status bar on monitor 1" (spawn (myEditor ++ " -e vim /home/endesse/.config/xmobar/xmobarrc0"))) []
           ]
       , Node (TS.TSNode "+ xmonad configs" "My xmonad config files" (return ()))
           [ Node (TS.TSNode "xmonad.hs" "My XMonad Main" (spawn (myEditor ++ " -e vim /home/endesse/.xmonad/xmonad.hs"))) []
           ]
       , Node (TS.TSNode "bashrc" "the bourne again shell" (spawn (myEditor ++ " -e vim /home/endesse/.bashrc"))) []
       , Node (TS.TSNode "bspwmrc" "binary space partitioning window manager" (spawn (myEditor ++ " -e vim /home/endesse/.config/bspwm/bspwmrc"))) []
       , Node (TS.TSNode "dmenu config.h" "dynamic menu program" (spawn (myEditor ++ " -e vim /home/endesse/dmenu-distrotube/config.h"))) []
       , Node (TS.TSNode "qutebrowser config.py" "qutebrowser web browser" (spawn (myEditor ++ " -e /home/endesse/.config/qutebrowser/config.py"))) []
       , Node (TS.TSNode "xresources" "xresources file" (spawn (myEditor ++ " -e vim /home/endesse/.Xresources"))) []
       ]
   , Node (TS.TSNode "+ Screenshots" "take a screenshot" (return ()))
       [ Node (TS.TSNode "Quick fullscreen" "take screenshot immediately" (spawn "scrot -d 1 ~/scrot/%Y-%m-%d-@%H-%M-%S-scrot.png")) []
       , Node (TS.TSNode "Delayed fullscreen" "take screenshot in 5 secs" (spawn "scrot -d 5 ~/scrot/%Y-%m-%d-@%H-%M-%S-scrot.png")) []
       , Node (TS.TSNode "Section screenshot" "take screenshot of section" (spawn "scrot -s ~/scrot/%Y-%m-%d-@%H-%M-%S-scrot.png")) []
       ]
   , Node (TS.TSNode "------------------------" "" (spawn "xdotool key Escape")) []
   , Node (TS.TSNode "+ XMonad" "window manager commands" (return ()))
       [ Node (TS.TSNode "+ View Workspaces" "View a specific workspace" (return ()))
         [ Node (TS.TSNode "View 1" "View workspace 1" (spawn "~/.xmonad/xmonadctl 1")) []
         , Node (TS.TSNode "View 2" "View workspace 2" (spawn "~/.xmonad/xmonadctl 3")) []
         , Node (TS.TSNode "View 3" "View workspace 3" (spawn "~/.xmonad/xmonadctl 5")) []
         , Node (TS.TSNode "View 4" "View workspace 4" (spawn "~/.xmonad/xmonadctl 7")) []
         , Node (TS.TSNode "View 5" "View workspace 5" (spawn "~/.xmonad/xmonadctl 9")) []
         , Node (TS.TSNode "View 6" "View workspace 6" (spawn "~/.xmonad/xmonadctl 11")) []
         , Node (TS.TSNode "View 7" "View workspace 7" (spawn "~/.xmonad/xmonadctl 13")) []
         , Node (TS.TSNode "View 8" "View workspace 8" (spawn "~/.xmonad/xmonadctl 15")) []
         , Node (TS.TSNode "View 9" "View workspace 9" (spawn "~/.xmonad/xmonadctl 17")) []
         ]
       , Node (TS.TSNode "+ Shift Workspaces" "Send focused window to specific workspace" (return ()))
         [ Node (TS.TSNode "View 1" "View workspace 1" (spawn "~/.xmonad/xmonadctl 2")) []
         , Node (TS.TSNode "View 2" "View workspace 2" (spawn "~/.xmonad/xmonadctl 4")) []
         , Node (TS.TSNode "View 3" "View workspace 3" (spawn "~/.xmonad/xmonadctl 6")) []
         , Node (TS.TSNode "View 4" "View workspace 4" (spawn "~/.xmonad/xmonadctl 8")) []
         , Node (TS.TSNode "View 5" "View workspace 5" (spawn "~/.xmonad/xmonadctl 10")) []
         , Node (TS.TSNode "View 6" "View workspace 6" (spawn "~/.xmonad/xmonadctl 12")) []
         , Node (TS.TSNode "View 7" "View workspace 7" (spawn "~/.xmonad/xmonadctl 14")) []
         , Node (TS.TSNode "View 8" "View workspace 8" (spawn "~/.xmonad/xmonadctl 16")) []
         , Node (TS.TSNode "View 9" "View workspace 9" (spawn "~/.xmonad/xmonadctl 18")) []
         ]
       , Node (TS.TSNode "Next layout" "Switch to next layout" (spawn "~/.xmonad/xmonadctl next-layout")) []
       , Node (TS.TSNode "Recompile" "Recompile XMonad" (spawn "xmonad --recompile")) []
       , Node (TS.TSNode "Restart" "Restart XMonad" (spawn "xmonad --restart")) []
       , Node (TS.TSNode "Quit" "Restart XMonad" (io exitSuccess)) []
       ]
   ]

tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0xdd282c34
                              , TS.ts_font         = myFont
                              , TS.ts_node         = (0xffd0d0d0, 0xff1c1f24)
                              , TS.ts_nodealt      = (0xffd0d0d0, 0xff282c34)
                              , TS.ts_highlight    = (0xffffffff, 0xff755999)
                              , TS.ts_extra        = 0xffd0d0d0
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 20
                              , TS.ts_originX      = 100
                              , TS.ts_originY      = 100
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = myTreeNavigation
                              }

myTreeNavigation = M.fromList
    [ ((0, xK_Escape),   TS.cancel)
    , ((0, xK_Return),   TS.select)
    , ((0, xK_space),    TS.select)
    , ((0, xK_Up),       TS.movePrev)
    , ((0, xK_Down),     TS.moveNext)
    , ((0, xK_Left),     TS.moveParent)
    , ((0, xK_Right),    TS.moveChild)
    , ((0, xK_k),        TS.movePrev)
    , ((0, xK_j),        TS.moveNext)
    , ((0, xK_h),        TS.moveParent)
    , ((0, xK_l),        TS.moveChild)
    , ((0, xK_o),        TS.moveHistBack)
    , ((0, xK_i),        TS.moveHistForward)
    , ((0, xK_a),        TS.moveTo ["+ Accessories"])
    , ((0, xK_e),        TS.moveTo ["+ Games"])
    , ((0, xK_g),        TS.moveTo ["+ Graphics"])
    , ((0, xK_h),	 TS.moveTo ["+ Pentest", "+ Cracking", "+ Enumeration", "+ Exploit"])
    , ((0, xK_i),        TS.moveTo ["+ Internet"])
    , ((0, xK_m),        TS.moveTo ["+ Multimedia"])
    , ((0, xK_o),        TS.moveTo ["+ Office"])
    , ((0, xK_p),        TS.moveTo ["+ Programming"])
    , ((0, xK_s),        TS.moveTo ["+ System"])
    , ((0, xK_b),        TS.moveTo ["+ Bookmarks"])
    , ((0, xK_c),        TS.moveTo ["+ Config Files"])
    , ((0, xK_r),        TS.moveTo ["+ Screenshots"])
    , ((mod4Mask, xK_l), TS.moveTo ["+ Bookmarks", "+ Linux"])
    , ((mod4Mask, xK_e), TS.moveTo ["+ Bookmarks", "+ Emacs"])
    , ((mod4Mask, xK_s), TS.moveTo ["+ Bookmarks", "+ Search and Reference"])
    , ((mod4Mask, xK_p), TS.moveTo ["+ Bookmarks", "+ Programming"])
    , ((mod4Mask, xK_v), TS.moveTo ["+ Bookmarks", "+ Vim"])
    , ((mod4Mask .|. altMask, xK_a), TS.moveTo ["+ Bookmarks", "+ Linux", "+ Arch Linux"])
    , ((mod4Mask .|. altMask, xK_n), TS.moveTo ["+ Bookmarks", "+ Linux", "+ Linux News"])
    , ((mod4Mask .|. altMask, xK_w), TS.moveTo ["+ Bookmarks", "+ Linux", "+ Window Managers"])
    ]

------------------------------------------------------------------------
-- XPROMPT SETTINGS
------------------------------------------------------------------------

dtXPConfig :: XPConfig
dtXPConfig = def
      { font                = myFont
      , bgColor             = "#282c34"
      , fgColor             = "#bbc2cf"
      , bgHLight            = "#c792ea"
      , fgHLight            = "#000000"
      , borderColor         = "#535974"
      , promptBorderWidth   = 0
      , promptKeymap        = dtXPKeymap
      , position            = Top
--    , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      -- , searchPredicate     = isPrefixOf
      , searchPredicate     = fuzzyMatch
      , alwaysHighlight     = True
      , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
      }

-- The same config above minus the autocomplete feature which is annoying
-- on certain Xprompts, like the search engine prompts.
dtXPConfig' :: XPConfig
dtXPConfig' = dtXPConfig
      { autoComplete        = Nothing
      }

-- A list of all of the standard Xmonad prompts and a key press assigned to them.
-- These are used in conjunction with keybinding I set later in the config. SUPER+p <key>
promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt)          -- manpages prompt
             , ("p", passPrompt)         -- get passwords (requires 'pass')
             , ("g", passGeneratePrompt) -- generate passwords (requires 'pass')
             , ("r", passRemovePrompt)   -- remove passwords (requires 'pass')
             , ("s", sshPrompt)          -- ssh prompt
             , ("x", xmonadPrompt)       -- xmonad prompt
             ]

-- Same as the above list except this is for my custom prompts. SUPER+p c
promptList' :: [(String, XPConfig -> String -> X (), String)]
promptList' = [ ("c", calcPrompt, "qalc")         -- requires qalculate-gtk
              ]

calcPrompt c ans =
    inputPrompt c (trim ans) ?+ \input ->
        liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c
    where
        trim  = f . f
            where f = reverse . dropWhile isSpace

dtXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
dtXPKeymap = M.fromList $
     map (first $ (,) controlMask)   -- control + <key>
     [ (xK_z, killBefore)            -- kill line backwards
     , (xK_k, killAfter)             -- kill line forwards
     , (xK_a, startOfLine)           -- move to the beginning of the line
     , (xK_e, endOfLine)             -- move to the end of the line
     , (xK_m, deleteString Next)     -- delete a character foward
     , (xK_b, moveCursor Prev)       -- move cursor forward
     , (xK_f, moveCursor Next)       -- move cursor backward
     , (xK_BackSpace, killWord Prev) -- kill the previous word
     , (xK_y, pasteString)           -- paste a string
     , (xK_g, quit)                  -- quit out of prompt
     , (xK_bracketleft, quit)
     ]
     ++
     map (first $ (,) altMask)       -- meta key + <key>
     [ (xK_BackSpace, killWord Prev) -- kill the prev word
     , (xK_f, moveWord Next)         -- move a word forward
     , (xK_b, moveWord Prev)         -- move a word backward
     , (xK_d, killWord Next)         -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++
     map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]

archwiki, ebay, news, reddit, urban :: S.SearchEngine

archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
ebay     = S.searchEngine "ebay" "https://www.ebay.com/sch/i.html?_nkw="
news     = S.searchEngine "news" "https://news.google.com/search?q="
reddit   = S.searchEngine "reddit" "https://www.reddit.com/search/?q="
urban    = S.searchEngine "urban" "https://www.urbandictionary.com/define.php?term="

-- This is the list of search engines that I want to use. Some are from
-- XMonad.Actions.Search, and some are the ones that I added above. SUPER+s <key>
searchList :: [(String, S.SearchEngine)]
searchList = [ ("a", archwiki)
             , ("d", S.duckduckgo)
             , ("e", ebay)
             , ("g", S.google)
             , ("h", S.hoogle)
             , ("i", S.images)
             , ("n", news)
             , ("r", reddit)
             , ("s", S.stackage)
             , ("t", S.thesaurus)
             , ("v", S.vocabulary)
             , ("b", S.wayback)
             , ("u", urban)
             , ("w", S.wikipedia)
             , ("y", S.youtube)
             , ("z", S.amazon)
             ]

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "mocp" spawnMocp findMocp manageMocp
                ]
  where
    spawnTerm  = myTerminal ++ " -n scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnMocp  = myTerminal ++ " -n mocp 'mocp'"
    findMocp   = resource =? "mocp"
    manageMocp = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
mirror	 = renamed [Replace "mirror_tall"]
	   $ limitWindows 12
	   $ mySpacing 8
	   $ Mirror
	   $ ResizableTall 1 (3/100) (1/2) []
magnify  = renamed [Replace "magnify"]
           $ magnifier
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ mySpacing' 8
           $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           $ limitWindows 7
           $ mySpacing' 4
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ limitWindows 7
           $ mySpacing' 4
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
           $ tabbed shrinkText myTabConfig
  where
    myTabConfig = def { fontName            = "xft:Mononoki Nerd Font:regular:pixelsize=11"
                      , activeColor         = "#282c34"
                      , inactiveColor       = "#3e445e"
                      , activeBorderColor   = "#282c34"
                      , inactiveBorderColor = "#282c34"
                      , activeTextColor     = "#ffffff"
                      , inactiveTextColor   = "#d0d0d0"
                      }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Sans:bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#000000"
    , swn_color             = "#FFFFFF"
    }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               -- I've commented out the layouts I don't use.
               myDefaultLayout =     tall
	       			 ||| mirror
                                 ||| magnify
                                 ||| noBorders monocle
                                 ||| floats
                                 ||| grid
                                 ||| noBorders tabs
                                 ||| spirals
                                 -- ||| threeCol
                                 -- ||| threeRow

------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------
-- Sets some rules for certain programs. Examples include forcing certain
-- programs to always float, or to always appear on a certain workspace.
-- Forcing programs to a certain workspace with a doShift requires xdotool
-- if you are using clickable workspaces. You need the className or title
-- of the program. Use xprop to get this info.

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out
     -- the full name of my workspaces.
     [ className =? "firefox"	--> doShift ( myWorkspaces !! 1 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     , className =? "qutebrowser"	--> doShift ( myWorkspaces !! 1 )
     , (className =? "Pcmanfm" <&&> title =? "Removable medium is inserted") --> doFloat
     , className =? "Zotero"	--> doShift ( myWorkspaces !! 3 )
     , (className =? "Zotero" <&&> title =? "Zotero Preferences") --> doFloat
     , (className =? "Zotero" <&&> title =? "Import") --> doFloat
     , (className =? "Zotero" <&&> title =? "Advanced Search") --> doFloat
     , (className =? "Zotero" <&&> title =? "My Library - Zotero Timeline") --> doFloat
     , (className =? "Zotero" <&&> title =? "RTF Scan") --> doFloat
     , (className =? "Zotero" <&&> title =? "Add-ons Manager") --> doFloat
     , (className =? "Zotero" <&&> title =? "Error Console") --> doFloat
     , (className =? "Zotero" <&&> title =? "Run JavaScript") --> doFloat
     , (className =? "Zotero" <&&> title =? "Zotero Style Editor") --> doFloat
     , (className =? "Zotero" <&&> title =? "Zotero Error Report") --> doFloat
     , (className =? "Zotero" <&&> title =? "Software Update") --> doFloat
     , className =? "VirtualBox Manager"	--> doShift ( myWorkspaces !! 4 )
     , className =? "Jitsi Meet"	--> doShift ( myWorkspaces !! 5 )
     , className =? "Audacity"	--> doShift ( myWorkspaces !! 6 )
     , className =? "lmms"	--> doShift ( myWorkspaces !! 6 )
     , className =? "Sonic Visualiser"	--> doShift ( myWorkspaces !! 6 )
     , className =? "kdenlive"	--> doShift ( myWorkspaces !! 7 )
     , className =? "mpv"     --> doShift ( myWorkspaces !! 7 )
     , className =? "vlc"     --> doShift ( myWorkspaces !! 7 )
     , className =? "Gimp"    --> doShift ( myWorkspaces !! 8 )
     , className =? "Inkscape"    --> doShift ( myWorkspaces !! 8 )
     , className =? "pentablet" --> doShift ( myWorkspaces !! 2)
     , className =? "pentablet"	--> doFloat
     , title =? "Krita"    --> doShift ( myWorkspaces !! 8 )
     -- TT Ofice
     , title =? "Anydesk"	--> doShift ( myWorkspaces !! 0 )
     , title =? "Skype"		--> doShift ( myWorkspaces !! 1 )
     --
     , title =? "Oracle VM VirtualBox Manager"     --> doFloat
     ] <+> namedScratchpadManageHook myScratchPads

myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")      -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")        -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                  -- Quits xmonad

    -- Open my preferred terminal
        --, ("M-<Return>", spawn (myTerminal ++ " -e fish"))
	, ("M-<Return>", spawn (myTerminal))

    -- Run Prompt
        , ("M-S-<Return>", shellPrompt dtXPConfig)   -- Shell Prompt

    -- Windows
        , ("M-S-c", kill1)                           -- Kill the currently focused client
        , ("M-S-a", killAll)                         -- Kill all windows on current workspace

    -- Floating windows
        , ("M-f", sendMessage (T.Toggle "floats"))       -- Toggles my 'floats' layout
        , ("M-<Delete>", withFocused $ windows . W.sink) -- Push floating window back to tile
        , ("M-S-<Delete>", sinkAll)                      -- Push ALL floating windows to tile

    -- Grid Select (CTRL-g followed by a key)
        , ("C-g g", spawnSelected' myAppGrid)                 -- grid select favorite apps
        , ("C-M1-g", spawnSelected' myAppGrid)                -- grid select favorite apps
        , ("C-g t", goToSelected $ mygridConfig myColorizer)  -- goto selected window
        , ("C-g b", bringSelected $ mygridConfig myColorizer) -- bring selected window

    -- Tree Select/
        , ("C-t t", treeselectAction tsDefaultConfig)

    -- Windows navigation
        , ("M-m", windows W.focusMaster)     -- Move focus to the master window
        , ("M-j", windows W.focusDown)       -- Move focus to the next window
        , ("M-k", windows W.focusUp)         -- Move focus to the prev window
        --, ("M-S-m", windows W.swapMaster)    -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)      -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)        -- Swap focused window with prev window
        , ("M-<Backspace>", promote)         -- Moves focused window to master, others maintain order
        , ("M1-S-<Tab>", rotSlavesDown)      -- Rotate all windows except master and keep focus in place
        , ("M1-C-<Tab>", rotAllDown)         -- Rotate all the windows in the current stack
        , ("M-S-s", windows copyToAll)
        , ("M-C-s", killAllOtherCopies)

        -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)                -- Switch to next layout
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
	, ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-S-<Space>", sendMessage ToggleStruts)         -- Toggles struts
        , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)      -- Toggles noborder
        , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))   -- Increase number of clients in master pane
        , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))  -- Decrease number of clients in master pane
        , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows
        , ("M-S-<KP_Divide>", decreaseLimit)                -- Decrease number of windows

        , ("M-h", sendMessage Shrink)                       -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                       -- Expand horiz window width
        , ("M-C-j", sendMessage MirrorShrink)               -- Shrink vert window width
        , ("M-C-k", sendMessage MirrorExpand)               -- Exoand vert window width

    -- Workspaces
        , ("M-.", nextScreen)  -- Switch focus to next monitor
        , ("M-,", prevScreen)  -- Switch focus to prev monitor
        , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
        , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws

    -- Scratchpads
        , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")
        , ("M-C-c", namedScratchpadAction myScratchPads "mocp")

    -- Controls for mocp music player.
        , ("M-u p", spawn "mocp --play")
        , ("M-u l", spawn "mocp --next")
        , ("M-u h", spawn "mocp --previous")
        , ("M-u <Space>", spawn "mocp --toggle-pause")

    --- My Applications (Super+Alt+Key)
        , ("M-M1-a", spawn (myTerminal ++ " -e ncpamixer"))
        , ("M-M1-b", spawn "surf www.youtube.com/c/DistroTube/")
        , ("M-M1-e", spawn (myTerminal ++ " -e neomutt"))
        , ("M-M1-f", spawn (myTerminal ++ " -e sh -c 'vifmrun; bash'"))
        , ("M-M1-i", spawn (myTerminal ++ " -e irssi"))
        , ("M-M1-j", spawn (myTerminal ++ " -e joplin"))
        , ("M-M1-l", spawn (myTerminal ++ " -e lynx https://distrotube.com"))
        , ("M-M1-m", spawn (myTerminal ++ " -e mocp"))
        , ("M-M1-n", spawn (myTerminal ++ " -e newsboat"))
        , ("M-M1-p", spawn (myTerminal ++ " -e pianobar"))
	, ("M-M1-q", spawn ("qutebrowser"))
        , ("M-M1-r", spawn (myTerminal ++ " -e rtv"))
        , ("M-M1-t", spawn (myTerminal ++ " -e toot curses"))
        , ("M-M1-w", spawn (myTerminal ++ " -e wopr report.xml"))
        , ("M-M1-y", spawn (myTerminal ++ " -e youtube-viewer"))

    -- Multimedia Keys
        , ("<XF86AudioPlay>", spawn "cmus toggle")
        , ("<XF86AudioPrev>", spawn "cmus prev")
        , ("<XF86AudioNext>", spawn "cmus next")
        -- , ("<XF86AudioMute>",   spawn "amixer set Master toggle")  -- Bug prevents it from toggling correctly in 12.04.
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86HomePage>", spawn "qutebrowser")
        , ("<XF86Search>", safeSpawn "qutebrowser" ["https://www.google.com/"])
        , ("<XF86Mail>", runOrRaise "geary" (resource =? "thunderbird"))
        , ("<XF86Calculator>", runOrRaise "gcalctool" (resource =? "gcalctool"))
        , ("<XF86Eject>", spawn "toggleeject")
	, ("<Scroll_lock>", spawn "xscreensaver-command -lock;")
        , ("<Print>", spawn "sleep 0.2; scrot -s ~/scrot/%Y-%m-%d-@%H-%M-%S-scrot.png")
        ]
        -- Appending search engine prompts to keybindings list.
        -- Look at "search engines" section of this config for values for "k".
        ++ [("M-s " ++ k, S.promptSearch dtXPConfig' f) | (k,f) <- searchList ]
        ++ [("M-S-s " ++ k, S.selectSearch f) | (k,f) <- searchList ]
        -- Appending some extra xprompts to keybindings list.
        -- Look at "xprompt settings" section this of config for values for "k".
        ++ [("M-p " ++ k, f dtXPConfig') | (k,f) <- promptList ]
        ++ [("M-p " ++ k, f dtXPConfig' g) | (k,f,g) <- promptList' ]
        -- The following lines are needed for named scratchpads.
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

main :: IO ()
main = do
    xmonad $ ewmh $ docks $ defaults

defaults = def
    { handleEventHook     = serverModeEventHookCmd
                            <+> serverModeEventHook
                            <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                            <+> docksEventHook
    , modMask             = myModMask
    , terminal            = myTerminal
    , workspaces          = myWorkspaces
    , layoutHook          = showWName' myShowWNameTheme $ smartBorders $ myLayoutHook
    , normalBorderColor   = myNormColor
    , focusedBorderColor  = myFocusColor
    , manageHook          = myManageHook <+> manageHook def
    , borderWidth         = myBorderWidth
    , startupHook         = myStartupHook
    } `additionalKeysP` myKeys
