WELCOME TO KONOPKA SIGNATURE VCL CONTROLS VERSION 6

CONTENTS

- Completing the Installation
- What's New
- Moving the Component Palettes Pages to Front
- Uninstalling
- Minimum system requirements


COMPLETING THE INSTALLATION

Once the installation program is complete you may begin using the components in RAD Studio 2009 and later using Delphi or C++Builder. Restart RAD Studio and you will see the following pages on the component palette:

Raize Panels
Raize Edits
Raize Lists
Raize Buttons
Raize Display
Raize Shell
Raize Widgets


WHAT'S NEW

For a complete description of new components, enhancements, and features please read the WhatsNew.pdf document in the main installation directory along with this Readme.txt file. The information in the document is also available in the Raize Components help file, which is integrated into the RAD Studio Help System.

    
MOVING THE COMPONENT PALETTE PAGES TO FRONT

During the installation, the appropriate design packages are loaded into the selected RAD Studio IDEs.  Unfortunately, when you restart the IDE, the component palette pages for the newly installed components will appear at the end of the component palette.

If you would like to move the Raize pages to the front of the palette, there is an a easier way to do this than by manually dragging them.  Located in the RC6\Bin directory is a program called MoveRCPagesToFront.exe, which will move all of the "Raize" pages to the front of the palette automatically.

Simply run the MoveRCPageToFront.exe program and follow the instructions.
  

UNINSTALLING

>> Removing the components from the RAD Studio component palette

Close all files and projects, and select Component|Install Packages... to display the Packages page in the Project Options dialog.

Select the "Raize Components 6.x" package from the Design Packages list and click the Remove button. A message box will be displayed to confirm your request--press OK.  Next, depending on the IDE, you may be asked if a runtime package should be from the Runtime Packages list.  If so, click OK to remove the runtime package.  

Next, select the "Raize Components 6.x (Data-Aware)" package from the Design Packages list and click the Remove button. Again, a message box will be displayed to confirm your request--press OK.  Next, depending on the IDE, you may be asked if a runtime package should be removed from the Runtime Packages list.  If so, click OK to remove the runtime package.

Close the Project Options dialog box by clicking the OK button.
                 
Repeat the above steps for each IDE that is using Raize Components.

>> Removing the component files from your hard disk

At this point, all IDEs are no longer using Raize Components.  To remove the Raize Components files from your hard disk the Add/Remove Programs icon from the Control Panel. Next, select the "Raize Components 6.0" entry from the list of installed programs, and then click the Remove button.


MINIMUM SYSTEM REQUIREMENTS

At least one of the following compilers:
Embarcadero RAD Studio 10.3 Rio		-- Delphi 10.3 Rio, C++Builder 10.3 Rio                 
Embarcadero RAD Studio 10.2 Tokyo	-- Delphi 10.2 Tokyo, C++Builder 10.2 Tokyo
Embarcadero RAD Studio 10.1 Berlin	-- Delphi 10.1 Berlin, C++Builder 10.1 Berlin
Embarcadero RAD Studio 10 Seattle	-- Delphi 10 Seattle, C++Builder 10 Seattle
Embarcadero RAD Studio XE8		-- Delphi XE8, C++Builder XE8
Embarcadero RAD Studio XE7		-- Delphi XE7, C++Builder XE7
Embarcadero RAD Studio XE6 (Update 1)	-- Delphi XE6, C++Builder XE6
Embarcadero RAD Studio XE5		-- Delphi XE5, C++Builder XE5
Embarcadero RAD Studio XE4		-- Delphi XE4, C++Builder XE4
Embarcadero RAD Studio XE3		-- Delphi XE3, C++Builder XE3
Embarcadero RAD Studio XE2 (Update 1)	-- Delphi XE2, C++Builder XE2
Embarcadero RAD Studio XE (Update 1)	-- Delphi XE, C++Builder XE
Embarcadero RAD Studio 2010		-- Delphi 2010, C++Builder 2010   
CodeGear RAD Studio 2009		-- Delphi 2009, C++Builder 2009

Hard Disk Space Requirements:  
20 MB + 10 MB for each compiler to be supported
  
