RealThinClient SDK
http://www.realthinclient.com

Copyright 2004-2014 (c) RealThinClient.com
All rights reserved.

--------------------------------
********************************

1.) License Agreement

2.) Install RTC SDK components in Delphi

3.) Make the RTC SDK accessible from XCode (for iOS development)

4.) Update RTC SDK components in Delphi

5.) Help

6.) Demos

7.) Support

********************************
--------------------------------

---------------------
1.) License Agreement
---------------------

Please read the RTC SDK License Agreement before using RTC SDK components.

You will find the RTC SDK License Agreement in the "License.txt" file.

--------------------------------
2.) INSTALL RTC SDK components in Delphi
--------------------------------

After you have unpacked the files in a folder of your choice and started Delphi, 
open the "Package_Main" Project Group where you will find 3 packages:
 
  rtcSDK.dpk      -> The main Package. Includes all Client and Server HTTP/S components.

  rtcSDK_DBA.dpk  -> Optional, includes only "TRtcMemDataSet" and "TRtcDataSetMonitor" components.

  rtcSDK_RAW.dpk  -> Optional, includes only raw TCP/IP and UDP communication components.

Install the components in Delphi by using the "Install" button, or the "Install" menu option.
In older Delphi versions, you will see the "Install" button in the Project Manager window.
In newer Delphi versions, you will find the "Install" option if you right-click the package
file in the Project Manager accessed in the "View" drop-down menu.

When compiled and installed, you will see a message listing all components installed.

After that, you should add the path to the RTC SDK's "Lib" folder to "Library paths" in Delphi.

In older Delphi versions, the Library Path is located in the "Tools / Environment Options" menu.
Select the "Library" tab and add the full path to the RTC SDK's "Lib" folder to "Library path".

In newer Delphi versions, Library Paths are located in the "Tools / Options" menu. Select the 
"Environment Options / Delphi Options / Library" tree branch, where you will find the "Library Path" field.
There, you should click the "..." button next to "Library path" and add the path to the RTC SDK's "Lib" folder.

In Delphi XE2 and later, you will also see a "Selected Platform" drop-down menu. There, all the settings are separated 
by platforms, so you  will need to repeat the process for every platform you want to use the "RTC SDK" with. 

-------------------------------
3.) Make the RTC SDK accessible from XCode (for iOS development) - Delphi XE2
-------------------------------

For the FPC compiler to find RTC SDK files, you can either copy the complete "Lib" folder (with sub-folders)
from the RTC SDK package into the "/Developer/Embarcadero/fmi" folder (quick and dirty solution), or ... 

You can add the path to the RTC SDK “Lib” folder (located on your Windows PC, made accessible to Mac over LAN) 
to the FPC search path. Unfortunatelly, there  is no “parameter” for adding FPC search paths in XCode directly, 
so you will need to do this manually for every XCode Project. And not only once, but every time you recreate 
XCode Project files by using the “dpr2xcode” tool, because all your changes will be overwritten by "dpr2xcode". 

To avoid having to make these changes too often, use "dpr2xcode" ONLY if you have made changes to the Project 
file itself (changed the Project icon, for example). There is no need to recreate XCode Project files if you 
have only changed forms or units inside the Project.

To add the RTC SDK paths to FPC, you will need to modify the file "xcode/<ProjectName>.xcodeproj/project.pbxproj". 
The path to the RTC SDK “Lib” folder needs to be added as two new “-Fu” parameters. Once for iOS-Simulator and 
once for iOS-Device compilation, both of are configured through the “shellScript” parameter.

The best place to add the RTC SDK Lib path is after the FireMonkey path, so you should search for 
“-Fu/Developer/Embarcadero/fmi” in the above mentioned XCode Project file. You will find 2 such 
instances in the "ShellScript" line and you should add the path to the RTC SDK Lib folder directly 
after each "-Fu/Developer/Embarcadero/fmi" instance.

For example, if you have made the complete RTC SDK folder on your Windows PC available to your Mac OSX 
through a network share named “RTC_SDK” (read/write access rights to that folder will be required for FPC 
compilation to work), you should add “-Fu/Volumes/RTC_SDK/Lib” after both “-Fu/Developer/Embarcaedro/fmi” locations. 
One is for the iOS-Simulator, the other one for the iOS device. 

That will be enough to let FPC know where to look for RTC SDK files.

Should you still get "File not found" errors when trying to compile a Project using RTC files, 
make sure the path you have used is correct and that Mac OSX has read and write access to that folder.

PS. Before configuring access to the RTC SDK, you will need to have OSX 10.6 or 10.7, the latest XCode 4.x 
version and both packages provided by Embarcadero (included with RAD Studio XE2) installed on your Mac. 

To make sure your Mac OSX configuration is correct, create an empty "FireMonkey iOS HD" Project, use "dpr2xcode" 
to create XCode Project files and try to run that Project from XCode,
either inside the iOS-Simulator or directly on your iOS device (iPhone or iPad).

-------------------------------
4.) UPDATE RTC SDK components in Delphi
-------------------------------

Download the latest version of the RTC SDK from the RTC Support Forum:
http://sf.realthinclient.com

Information about recent RTC SDK updates is in the "Updates*.txt" file(s).

To update RTC SDK components, it's adviseable to uninstall the old packages and 
delete the old BPL and DCP files (rtcSDK.bpl, rtcSDK.dcp, rtcSDK_DBA.bpl, 
rtcSDK_DBA.dcp, rtcSDK_RAW.bpl and rtcSDK_RAW.dcp) before installing new packages.

To uninstall RTC SDK components, after you start Delphi, 
open the menu "Component / Install Packages ..." where you 
will see a list of all packages currently installed in your Delphi. 

Scroll down to find "RealThinClient SDK" and click on it (single click). 
When you select it, click the button "Remove" and Delphi will ask you 
if you want to remove this package. Clicking "Yes" will uninstall the RTC SDK.

After that, *close* Delphi and follow step (2) to install the new RTC SDK package.

NOTE: Uninstalling the RTC SDK package will also uninstall all packages which are 
using the RTC SDK (for example, rtcSDK_DBA, rtcSDK_RAW and "Nexus Portal" packages). 
So ... if you are using "Nexus Portal" or any other product using the RTC SDK, you will 
need to Build and Install all related packages again, after you reinstall the RTC SDK.

-------------
5.) Help
-------------

The best place to start learning about RTC SDK is the QuickStart guide. After going through the 
online lessons, you should also go through the QuickStart examples included in the RTC SDK package. 

When you are done examining QuickStart examples, I suggest browsing through the FAQ. Even if you won't
be reading all the articles, you should at least get the feeling about the information included there.

RTC SDK Demos are another good source of information, including a lot of examples and best practices 
for using the RealThinClient SDK. And the most extensive source of information on the RealThinClient SDK 
are Help files. Some of the information is spread across the files, but if you know which class you need, 
you will most likely be able to find what you are looking for.

When you start working on your project, the FAQ will come in handy when you have to do something 
specific (use Sessions, accept form post data, write and call remote functions, etc). The FAQ is 
continually being extended, as more questions come in.

If you have a question for which you were unable to find the answer in the QuickStart guide, QuickStart 
examples or the FAQ … and searching through the Help files didn't give you the answers you need, don't 
hesitate to post your question(s) on Developer Support groups.

The latest Help file for Off-line viewing is in the "Help" folder:
- "Help\RTCSDK_Help.chm"

-------------------
6.) Demos
-------------------

You can find Demos using RTC SDK components in the "Demos" folder.
Simple Quick Start Examples are available in the "QuickStart" folder.

There are also 5 Project Groups which include all the Demos and Quick Start examples:

  * Demos_VCL - Demos using the VCL and the rtcSDK.dpk
  * Demos_VCL_DBA - Demos using the VCL with the rtcSDK.dpk and rtcSDK_DBA.dpk

  * Demos_FMX - Demos using FMX (Win,OSX,iOS) with the rtcSDK.dpk 
  * Demos_FMX_DBA - Demos using FMX (Win,OSX,iOS) with the rtcSDK.dpk and rtcSDK_DBA.dpk

  * Examples_QuickStart_VCL - Short "QuickStart" examples using the VCL with the rtcSDK.dpk

For short descriptions of available demos and quick start examples, please find the 
"Readme_Demos.txt" file in the "Demos" folder, and "Readme_QuickStart.txt" file in the QuickStart folder.

-------------
7.) Support
-------------

More information on RTC SDK:
> http://www.realthinclient.com/about.htm

Download the latest version of RTC components and get Support through the RTC Forums:
> http://sf.realthinclient.com