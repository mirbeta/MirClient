ICS - Internet Component Suite - V8 - Delphi 7 to RAD Studio 10.3 Rio
=====================================================================
(Aka FPIETTE's Components)


Revised: November 2, 2018
Release: V8.58
http://www.overbyte.be/
http://wiki.overbyte.be/
http://www.overbyte.eu/
http://wiki.overbyte.eu/

Table of content:
-----------------

- Legal issues
- Donate
- Register
- Contributions
- Latest Versions
- Version Control repository
- Installation
- Available VCL Components
- Sample applications
- About SSL
- Support
- Release notes
- Midware
- Known problems
- Special thanks


Legal issues:
-------------
              Copyright (C) 1997-2018 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium
              <francois.piette@overbyte.be>

              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

              ICS is freeware.

              This software is provided 'as-is', without any express or
              implied warranty. In no event will the author be held liable
              for any damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

              5. As this code make use of OpenSSL, your rights are restricted
                 by OpenSSL license as soon as you use any SSL feature.
                 See http://www.openssl.org for details.



Donate
------

ICS is freeware. You can use it without paying anything except the registration
postcard (see "register" below). But of course donations are welcome. You can
send cash (Euro currency or US Dollars) in an envelop to my street address or
buy a gift certificate at Amazon in the UK. I will then use it to buy books.
Here is the direct URL at Amazon UK (nearest to my home, please don't use another):
http://www.amazon.co.uk/exec/obidos/gc-email-order1/ref=g_gc_email/202-6198323-6681414
For more generous amount, contact me by email.


Register
--------

ICS is freeware. If you use the components, you must register by sending a
picture postcard showing the area you live in and some beautiful stamps for
my kids who are stamp collectors. Do not use an envelop, I collect USED
postcards sent to me. Write on the postcard that it is your ICS registration.

Address your card to: Francois PIETTE, rue de Grady 24, 4053 Embourg, Belgium.
Don't forget to mention your name, street address, EMail and web site.


Contributions:
--------------

ICS has been designed by François PIETTE but many other peoples are working on the
components and sample programs. The history of changes in each source file list
all developers having contributed (When no name is given, the change is by F. Piette).
I can't list all contributors here but I want to specially thanks two specially active
contributors:
    - Arno Garrels
    - Angus Robertson <angus@magsys.co.uk>


Latest versions:
---------------

The latest versions of ICS can be downloaded from the ICS Wiki web site:

http://wiki.overbyte.eu/wiki/index.php/ICS_Download

ICS V5 and V6 are archive releases no longer updated, last supported release was 2007.

ICS V7 is a stable release that may still be updated for major bugs, but not for new
releases of Delphi, latest it supported was XE3.

ICS V8 is the current development release which is held in a public Version Control
repository that is zipped each night for easy download.  The download page above
also includes the OpenSSL binaries needed to support SSL. ICS V8 supports Delphi
64-bit and Mac OS-X projects.  Note that C++ Builder versions supported are up to
XE3, 10.2 Tokyo and 10.3 Rio.  There are currently no packages for XE4 to 10.1. but
older or newer ones will often work.

The latest version is V8.58, which will be reported by the CopyRight constant in
OverbyteIcsWSocket.pas and the integer WSocketVersion as 858.

ICS V9 is in early development and is planned to support Android and Linux Server. There
are no current plans for ICS for iOS.


Version Control repository:
---------------------------

svn://svn.overbyte.be/ics or http://svn.overbyte.be:8443/svn/ics
(Usercode = ics, password = ics)


Installation:
-------------

ICS V8 has been designed for Embarcadero Delphi 2009 and up, and C++ Builder
2009 and up, but is fully compatible with Borland Delphi 7 and CodeGear 2006 and
2007. Embarcadero RAD Studio includes Delphi and C++ Builder.

http://www.embarcadero.com/

With Delphi XE2 and later, VCL 64-bit Windows targets are supported for Delphi only.
Currently FireMonkey is partly supported for Delphi only (there are still a few
non-ported components). ICS for Mac OSX is currently experimental.

The zip file has sub-directories in it. You must use the WinZip "Use folder names"
option to restore this directory tree or you will have problems because the files
would not be in their proper subdirectories.

Please note most of these directories are differently named to ICS V7 and earlier,
to ease support of multiple versions of Delphi and platforms, and to ease location
of similar sample projects.  Please don't install V8 over an existing V7
installation, it will be a mess of old and new.

This is the new V8 sub-directory layout:

.\                                    Info directory
.\Install                             Component packages project groups for all versions
.\Packages        (was Delphi\Vc32)   Delphi (7 and up) and C++Builder (2006 and up) packages projects
.\Source          (was Delphi\Vc32)   ICS Delphi source code built into packages
.\Source\Include  (was Delphi\Vc32)   .inc files (including OverbyteIcsDefs.inc)
.\Source\Extras   (was Delphi\Vc32)   Extra source code not built into packages
.\Source\zobj125   (was Delphi\Vc32)   ZLIB C OBJ include files

.\Lib                                 Unit output directories for all package builds, subdirectories
    |                                 for 2007+ will be created on building the packages
  \$(Config)                          Release / Debug
      |
    \$(Platform)                      Win32 / Win64 / OSX32
        |
      \<delphi_version>               D7..XE8, 10 Seattle includes .dcu and .dfm files for Delphi
                                      and .obj and .hpp files for C++ Builder

.\Samples                             Delphi Win32/Win64 common source for all demos
.\Samples\delphi\BroswerDemo          Delphi Win32/Win64 Web Browser sample application (all Delphi versions)
.\Samples\delphi\BroswerDemo\Resources Resource file, web pages and movie linked into browser demo
.\Samples\delphi\FtpDemos             Delphi Win32/Win64 FTP sample applications (all Delphi versions)
.\Samples\delphi\MailNewsDemos        Delphi Win32/Win64 SMTP, POP3, NNTP sample applications (all Delphi versions)
.\Samples\delphi\MiscDemos            Delphi Win32/Win64 Miscellaneous applications (all Delphi versions)
.\Samples\delphi\OtherDemos           Delphi Win32/Win64 DNS, Ping, SNMP, Syslog sample applications (all Delphi versions)
.\Samples\delphi\PlatformDemos        Delphi FireMonkey and cross-platform samples (Delphi XE2+)
.\Samples\delphi\SocketDemos          Delphi Win32/Win64 Socket sample applications (all Delphi versions)
.\Samples\delphi\sslinternet          Delphi Win32/Win64 SSL-enabled sample applications (all Delphi versions)
.\Samples\delphi\WebDemos             Delphi Win32/Win64 HTTP sample applications (all Delphi versions)
.\Samples\delphi\WebDemos\WebAppServerData  Directory for WebAppServ demo data files
.\Samples\delphi\WebDemos\WebServData Directory for WebServ demo data files
.\Samples\cpp\internet                C++Builder sample applications
.\Samples\cpp\internet\cb2006         C++Builder 2006 projects
.\Samples\cpp\internet\cb2007         C++Builder 2007 projects
.\Samples\cpp\internet\cb2009         C++Builder 2009 projects
.\Samples\cpp\internet\cb2010         C++Builder 2010 projects
.\Samples\cpp\internet\cbXE           C++Builder XE projects
.\Samples\cpp\internet\cbXE2          C++Builder XE2 projects


UPGRADING and REINSTALLING

Uninstall an existing ICS package (Menu | Component | Install Packages, select
the component package and click Remove).

Rename the old ICS directory and unzip to a new or empty directory, remove the
old path from the library path and add either the new .\Source directory to the library
path under Tools | Options |... or the appropriate .\Lib subdirectory according to
version, ie .\Lib\Debug\Win32\D2007 for Delphi 2007.

The latter has the advantage that the ICS source code won't be recompiled whenever
your project is build. Also under Tools | Options |... add the new .\Source directory
to the Browsing path.


All DELPHI and C++ BUILDER VERSIONS/WIN32

Always upgrade your compiler with the latest update available from Embarcadero.
Always update your system with http://windowsupdate.microsoft.com


SSL or not SSL?

By default the SSL code is compiled into the run-time package and additional SSL-
enabled components are installed. In order to not compile the SSL code into the
run-time package and to not install the SSL-Enabled components you need to remove
the conditional define USE_SSL from both the run-time and design-time package.

However if you do not build your applications with run-time packages it is
recommended to build the packages with default settings. The SSL code will the
be compiled into your applications depending on whether the conditional define
USE_SSL is set in the project options or not (this requires having the .\Source
directory in either in the library path or in projects Search path).

Actual use of SSL in your applications also requires the OpenSSL files
libcrypto-1_1.dll (or libcrypto-1_1-x64.dll) and libssl-1_1.dll (or libssl-1_1-x64).dll
being available somewhere in the path.  The ICS distribution includes the latest Win32
OpenSSL files in the .\OpenSSL-Win32 directory and the four main DLLs duplicated in
.\Samples\delphi\sslinternet for Win32 and Win64 samples.

Other OpenSSL files, including older and Win64, may be downloaded from:

http://wiki.overbyte.eu/wiki/index.php/ICS_Download


INSTALLATION USING THE INSTALL PROJECT GROUPS

For each Delphi and C++ Builder version one project group is provided in directory
.\Install:

Delphi 7         :  D7Install.bpg
Delphi 2006      :  D2006Install.bdsgroup
Delphi 2007      :  D2007Install.groupproj
Delphi 2009      :  D2009Install.groupproj
Delphi 2010      :  D2010Install.groupproj
Delphi XE        :  DXeInstall.groupproj
Delphi XE2       :  DXe2Install.groupproj // VCL only, no FireMonkey components
Delphi XE2       :  DXe2InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi XE3       :  DXe3Install.groupproj // VCL only, no FireMonkey components
Delphi XE3       :  DXe3InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi XE4       :  DXe4Install.groupproj // VCL only, no FireMonkey components
Delphi XE4       :  DXe4InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi XE5       :  DXe5Install.groupproj // VCL only, no FireMonkey components
Delphi XE5       :  DXe5InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi XE6       :  DXe6Install.groupproj // VCL only, no FireMonkey components
Delphi XE6       :  DXe6InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi XE7       :  DXe7Install.groupproj // VCL only, no FireMonkey components
Delphi XE7       :  DXe7InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi XE8       :  DXe8Install.groupproj // VCL only, no FireMonkey components
Delphi XE8       :  DXe8InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi 10 Seattle  : D10SInstall.groupproj // VCL only, no FireMonkey components
Delphi 10 Seattle  : D10SInstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi 10.1 Berlin : D101Install.groupproj // VCL only, no FireMonkey components
Delphi 10.1 Berlin : D101InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi 10.2 Tokyo  : D102Install.groupproj // VCL only, no FireMonkey components
Delphi 10.2 Tokyo  : D102InstallVclFmx.groupproj // Both VCL and FireMonkey components
Delphi 10.3 Rio  : D103Install.groupproj // VCL only, no FireMonkey components
Delphi 10.3 Rio  : D103InstallVclFmx.groupproj // Both VCL and FireMonkey components
C++ Builder 2006 :  CB2006Install.bdsgroup
C++ Builder 2007 :  CB2007Install.groupproj
C++ Builder 2009 :  CB2009Install.groupproj
C++ Builder 2010 :  CB2010Install.groupproj
C++ Builder XE   :  CBXeInstall.groupproj
C++ Builder XE2  :  CBXe2Install.groupproj // VCL only no FireMonkey components
C++ Builder XE2  :  CBXe2InstallVclFmx.groupproj // Both VCL and FireMonkey components
C++ Builder XE3  :  CBXe3InstallVclFmx.groupproj // Both VCL and FireMonkey components
C++ Builder 10.2 Tokyo  : CB102InstallVclFmx.groupproj // Both VCL and FireMonkey components
C++ Builder 10.3 Rio  : CB103InstallVclFmx.groupproj // Both VCL and FireMonkey components

1 - Do a File/Open Project, navigate to the Install directory, select the correct
file and open it. The project manager view should now display two package
projects, one run-time and one design-time package. The run-time package name
contains the "Run" suffix. The design-time package name contains the "Design"
suffix.

2 - Select and Build the run-time package (do not install).

3 - Select and Install the design-time package.

After a few seconds, you should have a dialog box telling you the package has
been installed with a bunch of new components registered in the Tool Palette
under "Overbyte ICS" and "Overbyte ICS SSL". Then do a "Save All" and a "Close All".

4 - One package is installed, called 'Overbyte ICS Design-Time Package for Delphi xxx'.

5 - Various directories under .\Samples\delphi\ include samples that illustrate use
of all the ICS components, see later.

6 - Alternatively, recent Delphi versions have a Build Groups pane in the Project
Manager, opened by clicking a a button.  This allows building multiple packages with
multiple configurations and platforms with a single click. After building, the
32-bit design package still need to be installed manually.


FIREMONKEY CROSS PLATFORM PACKAGES:

1 - For XE2 and later, DXe?Install (where ? is the version) installs VCL components
only, while DXe?InstallVclFmx also installs FireMonkey cross platform components
(three run time packages). In order to use this feature first uninstall the old
design-time package.

2 = Build all three run-time packages for all available platforms (32-bit
and 64-bit Windows and Mac OS X) in the order they are listed in project manager.

3 - Next build and install the three design-time packages in the order they are
listed in project manager.

4 - Three packages are installed, called:
    'Overbyte ICS Common Design-Time Package for Delphi xxx'
    'Overbyte ICS FMX Design-Time Package for Delphi xxx'
    'Overbyte ICS VCL Design-Time Package for Delphi xxx'

Note that the new packaging is still beta/alpha, both package names and included
units might change in a future beta drop. The old VCL packages are still there
however they do no longer support FireMonkey and of course only one set of
packages can be installed in the IDE at the same time, if you want both VCL
and FMX install DXe2InstallVclFmx.groupproj only. Currently the XE2 package
cache is buggy and should be disabled by adding the -nocache parameter.

5 - Alternatively, recent Delphi versions have a Build Groups pane in the Project
Manager, opened by clicking a a button.  This allows building multiple packages with
multiple configurations and platforms with a single click.  After building, the
32-bit design packages still need to be installed one by one.

6 - The .\Samples\delphi\PlatformDemos\ folder contains FireMonkey sample
projects that may all be built with FireMonkey for Mac OS X (and Windows).


ALTERNATE INSTALLATION USING THE PACKAGE PROJECT FILES:

For each Delphi and C++ Builder version two package project files exist in the
.\Packages directory. One run-time and one design-time package project file.
The run-time file name contains the "Run" suffix. The design-time file name
contains the "Design" suffix.

PACKAGE PROJECT FILE NAMES - VCL:
Delphi 7         :  OverbyteIcsD7Run.dpk, OverbyteIcsD7Design.dpk
Delphi 2006      :  OverbyteIcsD2006Run.bdsproj, OverbyteIcsD2006Design.bdsproj
Delphi 2007      :  OverbyteIcsD2007Run.dproj, OverbyteIcsD2007Design.dproj
Delphi 2009      :  OverbyteIcsD2009Run.dproj, OverbyteIcsD2009Design.dproj
Delphi 2010      :  OverbyteIcsD2010Run.dproj, OverbyteIcsD2010Design.dproj
Delphi XE        :  OverbyteIcsDXeRun.dproj, OverbyteIcsDXeDesign.dproj
Delphi XE2       :  OverbyteIcsDXe2Run.dproj, OverbyteIcsDXe2Design.dproj
Delphi XE3       :  OverbyteIcsDXe3Run.dproj, OverbyteIcsDXe3Design.dproj
Delphi XE4       :  OverbyteIcsDXe4Run.dproj, OverbyteIcsDXe4Design.dproj
Delphi XE5       :  OverbyteIcsDXe5Run.dproj, OverbyteIcsDXe5Design.dproj
Delphi XE6       :  OverbyteIcsDXe6Run.dproj, OverbyteIcsDXe6Design.dproj
Delphi XE7       :  OverbyteIcsDXe7Run.dproj, OverbyteIcsDXe7Design.dproj
Delphi XE8       :  OverbyteIcsDXe8Run.dproj, OverbyteIcsDXe8Design.dproj
Delphi 10 Seattle  : OverbyteIcsD10SRun.dproj, OverbyteIcsD10SDesign.dproj
Delphi 10.1 Berlin : OverbyteIcsD101Run.dproj, OverbyteIcsD101Design.dproj
Delphi 10.2 Tokyo  : OverbyteIcsD102Run.dproj, OverbyteIcsD102Design.dproj
Delphi 10.3 Rio  : OverbyteIcsD103Run.dproj, OverbyteIcsD103Design.dproj
C++ Builder 2006 :  OverbyteIcsCB2006Run.bdsproj, OverbyteIcsCB2006Design.bdsproj
C++ Builder 2007 :  OverbyteIcsCB2007Run.cbproj, OverbyteIcsCB2007Design.cbproj
C++ Builder 2009 :  OverbyteIcsCB2009Run.cbproj, OverbyteIcsCB2009Design.cbproj
C++ Builder 2010 :  OverbyteIcsCB2010Run.cbproj, OverbyteIcsCB2010Design.cbproj
C++ Builder XE   :  OverbyteIcsCBXeRun.cbproj, OverbyteIcsCBXeDesign.cbproj
C++ Builder XE2  :  OverbyteIcsCBXe2Run.cbproj, OverbyteIcsCBXe2Design.cbproj
C++ Builder XE3  :  OverbyteIcsCBXe3Run.cbproj, OverbyteIcsCBXe3Design.cbproj

PACKAGE PROJECT FILE NAMES - FireMonkey and VCL:
Delphi XE2 FMX/VCL      :  IcsCommonDXe2Run.dproj, IcsCommonDXe2Design.dproj
Delphi XE2 VCL          :  IcsVclDXe2Run.dproj, IcsVclDXe2Design.dproj
Delphi XE2 FMX          :  IcsFmxDXe2Run.dproj, IcsFmxDXe2Design.dproj
Delphi XE3 FMX/VCL      :  IcsCommonDXe3Run.dproj, IcsCommonDXe3Design.dproj
Delphi XE3 VCL          :  IcsVclDXe3Run.dproj, IcsVclDXe3Design.dproj
Delphi XE3 FMX          :  IcsFmxDXe3Run.dproj, IcsFmxDXe3Design.dproj
Delphi XE4 FMX/VCL      :  IcsCommonDXe4Run.dproj, IcsCommonDXe4Design.dproj
Delphi XE4 VCL          :  IcsVclDXe4Run.dproj, IcsVclDXe4Design.dproj
Delphi XE4 FMX          :  IcsFmxDXe4Run.dproj, IcsFmxDXe4Design.dproj
Delphi XE5 FMX/VCL      :  IcsCommonDXe5Run.dproj, IcsCommonDXe5Design.dproj
Delphi XE5 VCL          :  IcsVclDXe5Run.dproj, IcsVclDXe5Design.dproj
Delphi XE5 FMX          :  IcsFmxDXe5Run.dproj, IcsFmxDXe5Design.dproj
Delphi XE6 FMX/VCL      :  IcsCommonDXe6Run.dproj, IcsCommonDXe6Design.dproj
Delphi XE6 VCL          :  IcsVclDXe6Run.dproj, IcsVclDXe6Design.dproj
Delphi XE6 FMX          :  IcsFmxDXe6Run.dproj, IcsFmxDXe6Design.dproj
Delphi XE7 FMX/VCL      :  IcsCommonDXe7Run.dproj, IcsCommonDXe7Design.dproj
Delphi XE7 VCL          :  IcsVclDXe7Run.dproj, IcsVclDXe7Design.dproj
Delphi XE7 FMX          :  IcsFmxDXe7Run.dproj, IcsFmxDXe7Design.dproj
Delphi XE8 FMX/VCL      :  IcsCommonDXe8Run.dproj, IcsCommonDXe8Design.dproj
Delphi XE8 VCL          :  IcsVclDXe8Run.dproj, IcsVclDXe8Design.dproj
Delphi XE8 FMX          :  IcsFmxDXe8Run.dproj, IcsFmxDXe8Design.dproj
Delphi 10 Seattle FMX/VCL: IcsCommonD10SRun.dproj, IcsCommonD10SDesign.dproj
Delphi 10 Seattle VCL   :  IcsVclD10SRun.dproj, IcsVclD10SDesign.dproj
Delphi 10 Seattle FMX   :  IcsFmxD10SRun.dproj, IcsFmxD10SDesign.dproj
Delphi 10.1 Berlin FMX/VCL: IcsCommonD101Run.dproj, IcsCommonD101Design.dproj
Delphi 10.1 Berlin VCL  :  IcsVclD101Run.dproj, IcsVclD101Design.dproj
Delphi 10.1 Berlin FMX  :  IcsFmxD101Run.dproj, IcsFmxD101Design.dproj
Delphi 10.2 Tokyo FMX/VCL: IcsCommonD102Run.dproj, IcsCommonD102Design.dproj
Delphi 10.2 Tokyo VCL   :  IcsVclD102Run.dproj, IcsVclD102Design.dproj
Delphi 10.2 Tokyo FMX   :  IcsFmxD102Run.dproj, IcsFmxD102Design.dproj
Delphi 10.3 Rio FMX/VCL :  IcsCommonD103Run.dproj, IcsCommonD103Design.dproj
Delphi 10.3 Rio VCL     :  IcsVclD103Run.dproj, IcsVclD103Design.dproj
Delphi 10.3 Rio FMX     :  IcsFmxD103Run.dproj, IcsFmxD103Design.dproj
C++ Builder XE2 FMX/VCL :  IcsCommonCBXe2Run.dproj, IcsCommonDXe2Design.dproj
C++ Builder XE2 VCL     :  IcsVclCBXe2Run.dproj, IcsVclCBXe2Design.dproj
C++ Builder XE2 FMX     :  IcsFmxCBXe2Run.dproj, IcsFmxCBXe2Design.dproj
C++ Builder XE3 FMX/VCL :  IcsCommonCBXe3Run.dproj, IcsCommonDXe3Design.dproj
C++ Builder XE3 VCL     :  IcsVclCBXe3Run.dproj, IcsVclCBXe3Design.dproj
C++ Builder XE3 FMX     :  IcsFmxCBXe3Run.dproj, IcsFmxCBXe3Design.dproj
C++ Builder 10.2 Tokyo VCL : IcsVclCB102Run.dproj, IcsVclCB102Design.dproj
C++ Builder 10.2 Tokyo FMX : IcsFmxCB102Run.dproj, IcsFmxCB102Design.dproj
C++ Builder 10.3 Rio VCL: IcsVclCB103Run.dproj, IcsVclCB103Design.dproj
C++ Builder 10.3 Rio FMX: IcsFmxCB103Run.dproj, IcsFmxCB103Design.dproj


1 - Open and Build the run-time package project (do not install!).

2 - Open and Install the design-time package project.
(Do a File/Open Project, browse to the .\Packages directory. Select the correct file
and open it. Then in the project manager view, right-click on the package,
then click on either the Build or Install button.)

3 - For Delphi XE2 and later, a 64-bit run-time package can be built by changing
the package target platform to 64-bit Windows. This has the same name as the
32-bit package, so a different package output directory needs to be specified in
Tools / Options / Delphi Options for 64-bit Windows.

After a few seconds, you should have a dialog box telling you the package has
been installed with a bunch of new components registered in the Tool Palette
under "Overbyte ICS" and "Overbyte ICS SSL". Then do a "Save All" and a "Close All".


DELPHI 2006/WIN32, 2007/WIN32, 2009/WIN32, 2010/WIN32, XE/WIN32:

Having installed the package, verify that the appropriate Win32 Library Path
(Tools / Options / Delphi Options / Library - Win32 / Library Path) has been added,
.\Lib subdirectory according to version, ie .\Lib\Debug\Win32\D2007 for Delphi 2007.
If not, add it manually. It is not mandatory to add .\Lib to the global Delphi path,
but it will be much easier for you because otherwise you'll have to add it to each
project.


DELPHI XE2/WIN32, XE3/WIN32, XE4/WIN32, XE5/WIN32, XE6/WIN32, XE7/WIN32, XE8/WIN32,
10 Seattle/WIN32, 10.1 Berlin/WIN32, 10.2 Tokyo/WIN32, 10.3 Rio/WIN32, XE2/WIN64,
XE3/WIN64, XE4/WIN64,XE5/WIN64, XE6/WIN64, XE7/WIN64, XE8/WIN64, 10 Seattle/WIN64,
10.1 Berlin/WIN64, 10.2 Tokyo/WIN64, 10.3 Rio/WIN64:

Similar to above, but the Library path is specified separately for 32-bit and 64-bit
Windows Platforms. Beware Delphi seems to default to 64-bit platform, and needs to
be changed to 32-bit plaform before setting the Library path.


DELPHI 7: Add VC32 directory path to your library path (Tools menu / Environment
Options / Library / Library Path. Add .\Lib\Debug\Win32\D7 path at the end of the
existing path).


SAMPLE DELPHI PROJECTS

Once the package is installed, you may open the sample projects. There are about 95
samples are split into several directories according to protocols, with a project
group that can be opened in all versions of Delphi.

.\Samples\delphi\AllDemosProject.bpg
.\Samples\delphi\BroswerDemo
.\Samples\delphi\FtpDemos\FtpDemos.bpg
.\Samples\delphi\MailNewsDemos\MailNewsDemos.bpg
.\Samples\delphi\MiscDemos\MiscDemos.bpg
.\Samples\delphi\OtherDemos\OtherDemos.bpg
.\Samples\delphi\PlatformDemos\XSamples.groupproj
.\Samples\delphi\SocketDemos\SocketDemos.bpg
.\Samples\delphi\sslinternet\SslDemos.bpg
.\Samples\delphi\WebDemos\WebDemos.bpg

Full details of the individual sample projects are shown later in this document.

AllDemosProject.bpg contains all 94 samples except BrowserDemo (which needs a third
party component installed).  But building all the samples at the same requires a
lot of memory and was not possible until Delphi 10 Seattle and later which have improved
memory management.  Earlier versions of Delphi will compile each other project group.

You might get some dialog box telling you that resource files are missing (they may not
have been included in the zip file to save space) and are recreated by Delphi. It is OK.
Any other error message is a problem you should fix. After all resource files have
been recreated, you should see in the project manager a group of projects.

To compile all samples in the group at once, do Project / Build all projects. This may
take a few minutes.

Note Delphi has warnings which triggers a lot of messages for 100% OK code. You
can turn those warnings off in the project/ options / Compiler messages
and deselecting: "Deprecated symbol", "Platform symbol", "unsafe type", "unsafe code",
"unsafe typecast". Those are intended for .NET and Linux portability. You can
safely ignore them if you run windows. For you facility, I included a utility
SetProjectOptions (source code, you must compile it) in the internet directory.
This utility will update project options to disable the warnings.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.


C++ BUILDER 2006, 2007, 2009, 2010, XE, XE2, XE3, 10.2 Tokyo, 10.3 Rio:

Follow the installation procedure described for Delphi 2006. Just change
the project group and package name: use CB2006, CBXe, etc, see above.
You can't have Delphi 2006 and CBuilder 2006 packages installed at the
same time in the IDE. So when switching from one to the other, be sure to
remove the one you don't need.

Building the FireMonkey CBXE2InstallVclFmx C++ packages for OSX may trigger an
ILINK32 error, this is a bug in C++ Builder reported as QC #103668 the Win32
packages should build without errors.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.
Projects are located in SAMPLES\CPP\INTERNET\CB2006\ (or CB2006, CBXE, etc)
with a project group in each directory, OverbyteIcsCB2006Sam.bdsgroup,
OverbyteIcsCBXe2Sam.groupproj, etc. It is likely that for each project,
C++ Builder complains about a missing .res file. This is not a problem,
C++ Builder will recreate it as needed. They have not been included to save
space in the zip file.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.

NOTES:
- You may have an error message, using Delphi or C++ Builder complaining about
Font.Charset, OldCreateOrder and other properties. Those are new properties
in newer Delphi or C++ Builder versions, newer than the version you use.
You can safely ignore those errors because those properties are not
used by the components nor sample programs. You may encounter this
error at run time. To avoid it, you must open each form at design time
and ignore the error. Then recompile. If you don't ignore the error
at design time, you'll have it at runtime !

- If you have Delphi or C++ Builder complaining about a file not found, add
.\source directory to your library path.

- If you are using C++ Builder you may encounter an error at link time
such as "Unable to open file MWBCB30.LIB" (or other libs). This is a bug
in C++ Builder. To solve it, you can edit project option file (right click in
project manager) and remove any reference to the missing libraries.

- Don't forget that the C++Builder components are located in .\delphi\vc32
which is object pascal source code (not a problem for C++Builder, just
indicate that the *.pas files are displayed when installing). C++Builder
will create the *.hpp files. There are some on-line help files in the VC32
directory.


Available VCL Components
------------------------

- The following is a list of the files that should be installed in order to
properly add all of the available components in this collection:

> OverbyteIcsCharsetComboBox.pas Provides easy MIME charset selection
> OverbyteIcsDnsQuery          DNS lookup component - useful for getting MX records
> OverbyteIcsDprUpdFix.pas     IDE plugin for Delphi 2009 and 2010 to update old projects
> OverbyteIcsEmulVT.pas        ANSI terminal emulation in a control
> OverbyteIcsFingCli.pas       FINGER client protocol - Find information about user
> OverbyteIcsFtpCli.pas        FTP client protocol - file transfer
> OverbyteIcsFtpSrv.pas        FTP server protocol - file transfer
> OverbyteIcsFtpSrvT.pas       FTP server protocol - helpers
> OverbyteIcsHttpAppServer.pas HTTP server protocol - used to build advanced web servers
> OverbyteIcsHttpProt.pas      HTTP client protocol - used by the web
> OverbyteIcsHttpSrv.pas       HTTP server protocol - used to build web servers
> OverbyteIcsLogger.pas        A component to log information
> OverbyteIcsMimeDec.pas       MIME component - decode file attach, use with POP3
> OverbyteIcsMultiProgressBar.pas A segmented progress bar
> OverbyteIcsMultipartFtpDownloader.pas
        FTP client protocol - download one file using simultaneous connections to speedup download
> OverbyteIcsMultipartHttpDownloader.pas
        HTTP client protocol - download one file using simultaneous connections to speedup download
> OverbyteIcsNntpCli.pas       NNTP client protocol - send and receive newsgroups messages
> OverbyteIcsPing.pas          ICMP echo protocol - ping a host
> OverbyteIcsPop3Prot.pas      POP3 client protocol - get mail from mail server
> OverbyteIcsProxy.pas         Proxy server protocol - HTTP forward and reverse proxy, and others
> OverbyteIcsReg.pas           Register design components
> OverbyteIcsSmtpProt.pas      SMTP client protocol - send mail to server
> OverbyteIcsSmtpSrv.pas       SMTP server protocol - receive mail from client
> OverbyteIcsSnmpCli.pas       SNMP client protocol - network management
> OverbyteIcsSnmpMsgs.pas      SNMP client protocol - message helper
> OverbyteIcsSslHttpRest.pas   HTTPS REST functions, descends from THttpCli, includes OAuth2 authentication.
> OverbyteIcsSysLogClient.pas  Syslog Client Protocol - receive syslog messages
> OverbyteIcsSysLogDefs.pas    Syslog Protocol - helpers
> OverbyteIcsSysLogServer.pas  Syslog Server Protocol - send syslog messages
> OverbyteIcsTnCnx.pas         TELNET client protocol - terminal emulation protocol
> OverbyteIcsTnEmulVT.pas      TELNET and ANSI terminal emulation combined
> OverbyteIcsTnOptFrm.pas      TELNET Client configuration form
> OverbyteIcsTnScript.pas      TELNET client protocol - with automation
> OverbyteIcsWSocket.pas       Winsock component - TCP, UDP, DNS,...
> OverbyteIcsWSocketE.pas      Register procedure and property editor for TWSocket
> OverbyteIcsWSocketS.pas      Winsock component for building servers
> OverbyteIcsWSocketTS.pas     Winsock component for building multithreaded servers

- The following list support and utilities units:
> OverbyteIcsAsn1Utils.pas     ASN1 utilities (for TSnmpClient component)
> OverbyteIcsAvlTrees.pas      Implements a fast cache-like data storage
> OverbyteIcsCharsetUtils.pas  MIME-charset functions
> OverbyteIcsCookies.pas       Client Cookie Handling
> OverbyteIcsCRC.pas           32 bit CRC computation
> OverbyteIcsCsc.pas           character set routines
> OverbyteIcsDES.pas           Implementation of the Data Encryption Standard (DES)
> OverbyteIcsDigestAuth.pas    HTTP Digest Access Authentication
> OverbyteIcsFormDataDecoder.pas Decode a MIME data block as generated by a HTML form
> OverbyteIcsHttpCCodZLib.pas  Supports GZIP coding for HttpContCod
> OverbyteIcsHttpContCod.pas   HTTP Content Coding support, uses extra units
> OverbyteIcsIcmp.pas          ICMP protocol support, used by the PING component
> OverbyteIcsIconv.pas         Headers for iconv library (LGPL)
> OverbyteIcsLIBEAY.pas        Delphi encapsulation for libeay32.dll and libcrypto-1_1.dll (OpenSSL)
> OverbyteIcsMD4.pas           Implementation of the MD4 Message-Digest Algorithm
> OverbyteIcsMD5.pas           Implementation of the MD5 Message-Digest Algorithm
> OverbyteIcsMimeUtil.pas      Support routines for MIME standard
> OverbyteIcsMLang.pas         A few header translations from MS mlang.h
> OverbyteIcsNtlmMsgs.pas      Client NTLM authentification messages used within HTTP protocol
> OverbyteIcsNtlmSsp.pas       Server NTLM authentification of user credentials using Windows SSPI
> OverbyteIcsOneTimePw.pas     One Time Password support functions, used by FTP
> OverbyteIcsSHA1.pas          Implementation of US Secure Hash Algorithm 1 (SHA1)
> OverbyteIcsSocketUtils.pas   Cross platform socket utilities for ICS
> OverbyteIcsSSLEAY.pas        Delphi encapsulation for ssleay32.dll and libssl-1_1.dll (OpenSSL)
> OverbyteIcsSslJose.pas       JOSE - Json Object Signing and Encryption
> OverbyteIcsSslSessionCache.pas  A very fast external SSL-session-cache component
> OverbyteIcsSslThrdLock.pas   Implementation of OpenSsl thread locking (Windows);
> OverbyteIcsSslX509Certs.pas  Automatically download SSL X509 certificates from Let's Encrypt and CertCentre AG
> OverbyteIcsSslX509Utils.pas  SSL key and X509 certification creation
> OverbyteIcsSspi.pas          A few header translations from MS sspi.h and security.h
> OverbyteIcsStreams.pas       Fast streams for ICS
> OverbyteIcsThreadTimer.pas   A custom timer class using custom timer messages from one or more threads
> OverbyteIcsTicks64.pas       GetTickCount64 support for all versions of Windows
> OverbyteIcsTimeList.pas      List of items with expiry times, used for WebSessions
> OverbyteIcsTypes.pas         Common types, mainly for backward compiler compatibility
> OverbyteIcsURL.pas           Support routines for URL handling
> OverbyteIcsUtils.pas         Vast number of common utilities, many supporting Unicode for D7/2007
> OverbyteIcsWSockBuf.pas      FIFO buffers for TWSocket
> OverbyteIcsWebSession.pas    Web session support for THttpAppSrv and MidWare
> OverbyteIcsWinnls.pas        A few header translations for Unicode Normalization in winnls.h
> OverbyteIcsWinsock.pas       Some Winsock initialisations
> OverbyteIcsWndControl.pas    A class that encapsulates a windows message queue and a message map
> OverbyteIcsZLibDll.pas       Zlib support, interface to external zlib.dll functions
> OverbyteIcsZlibHigh.pas      Zlib support, high level interface for compression and decompression
> OverbyteIcsZLibObj.pas       Zlib support, interface to zlib linked C OBJ functions


FireMonkey Cross Platform Support:
----------------------------------

For Delphi and C++ Builder XE2 and later, FireMonkey Desktop applications are an alternate
to VCL Forms applications, supporting cross platforms of Windows 32-bit and 64-bit and Mac
OS X (and perhaps other platforms in future).  FireMonkey uses different visual components
to VCL, while some non-visual components can be used for both VCL and FMX projects, while
other components need special versions, such as ICS.

Earlier betas of V8 used the conditional define "FMX" which is *no longer required in
project options. Instead in your existing ICS FireMonkey app. add either "Ics.Fmx" to
the unit scope names in project options or apply the following changes in the uses clause,
rename:

OverbyteIcsWndControl              -> Ics.Fmx.OverbyteIcsWndControl
OverbyteIcsWSocket                 -> Ics.Fmx.OverbyteIcsWSocket
OverbyteIcsFtpCli                  -> Ics.Fmx.OverbyteIcsFtpCli
OverbyteIcsFtpSrv                  -> Ics.Fmx.OverbyteIcsFtpSrv
OverbyteIcsHttpProt                -> Ics.Fmx.OverbyteIcsHttpProt
OverbyteIcsWSocketS                -> Ics.Fmx.OverbyteIcsWSocketS
OverbyteIcsSmtpProt                -> Ics.Fmx.OverbyteIcsSmtpProt.pas
OverbyteIcsPop3Prot                -> Ics.Fmx.OverbyteIcsPop3Prot.pas
OverbyteIcsNntpCli                 -> Ics.Fmx.OverbyteIcsNntpCli.pas
OverbyteIcsPing                    -> Ics.Fmx.OverbyteIcsPing.pas
OverbyteIcsDnsQuery                -> Ics.Fmx.OverbyteIcsDnsQuery.pas
OverbyteIcsFingCli                 -> Ics.Fmx.OverbyteIcsFingCli.pas
OverbyteIcsSslSessionCache         -> Ics.Fmx.OverbyteIcsSslSessionCache.pas
OverbyteIcsSslThrdLock             -> Ics.Fmx.OverbyteIcsSslThrdLock.pas
OverbyteIcsHttpSrv                 -> Ics.Fmx.OverbyteIcsHttpSrv.pas
OverbyteIcsSocketUtils             -> Ics.Fmx.OverbyteIcsSocketUtils.pas
OverbyteIcsMultipartFtpDownloader  -> Ics.Fmx.OverbyteIcsMultipartFtpDownloader.pas
OverbyteIcsMultipartHttpDownloader -> Ics.Fmx.OverbyteIcsMultipartHttpDownloader.pas
OverbyteIcsHttpAppServer           -> Ics.Fmx.OverbyteIcsHttpAppServer.pas
OverbyteIcsThreadTimer             -> Ics.Fmx.OverbyteIcsThreadTimer.pas
OverbyteIcsCharsetComboBox         -> Ics.Fmx.OverbyteIcsCharsetComboBox.pas
{ Demo units }
OverbyteIcsWebAppServerCounter     -> Ics.Fmx.OverbyteIcsWebAppServerCounter
OverbyteIcsWebAppServerMailer      -> Ics.Fmx.OverbyteIcsWebAppServerMailer

The list above is also the list of units that now have different names in the FireMonkey
framework however most of them share the same source file.

Dropping a ICS component on the form will add the correct unit name for each framework
automatically (don't forget to disable the package cache as described above).

Unit OverbyteIcsLibrary.pas has been *deprecated* and ICS IPv8 doesn't use it anymore.
If you used it in your own code read the comment in OverbyteIcsLibrary.pas, search
for "deprecated".


Sample applications:
--------------------

With V8, the 96 sample applications are now grouped into directories according to
general functionality, to make it easier to compare related samples.

Many samples are similar. When searching for something, always look at the date
the demos where created. The most recent is always the best code!  In the lists
below, ACTIVE!! indicates applications that are actively maintained to test and
support new functionality in the ICS components.  These may not be simplest
samples, but are usually the first to try when learning about a component.

Delphi Win32/Win64 Web Browser sample application
-------------------------------------------------
.\Samples\delphi\BroswerDemo
> FrameBrowserIcs.dpr           Web Browser using HtmlViewer component - ACTIVE!!
Note this sample needs HtmlViewer component installed

Delphi Win32/Win64 FTP sample applications
------------------------------------------
.\Samples\delphi\FtpDemos\FtpDemos.bpg - Project group
> OverbyteIcsBasFtp.dpr         Basic FTP client program
> OverbyteIcsConFtp.dpr         Basic console mode FTP client
> OverbyteIcsFtpAsy.dpr         Example of asynchronous FTP client
> OverbyteIcsFtpMulti.dpr       Demo to do several FTP downloads in parallel to get a list of files
> OverbyteIcsFtpMultipartDownload.dpr Demo to FTP download a single large file in several parts in parallel
> OverbyteIcsFtpServ.dpr        General purpose FTP server, uses TSocketServer - ACTIVE!!
> OverbyteIcsFtpThrd.dpr        Demo of multithreaded FTP client, see also FTPASY
> OverbyteIcsFtpTst.dpr         Basic graphical FTP client - ACTIVE!!

Delphi Win32/Win64 SMTP, POP3, NNTP sample applications
-------------------------------------------------------
.\Samples\delphi\MailNewsDemos\MailNewsDemos.bpg - Project group
> OverbyteIcsBasNntp.dpr        Basic NNTP client program
> OverbyteIcsConPop3.dpr        Basic console mode demo for POP3 (mail receive)
> OverbyteIcsConSmtp.dpr        Basic console mode demo for SMTP (mail send)
> OverbyteIcsMailHtml.dpr       Example of HTML formatted EMail sending, including embedded images - ACTIVE!!
> OverbyteIcsMailRcv.dpr        Internet EMail access using POP3 protocol - ACTIVE!!
> OverbyteIcsMailSnd.dpr        Example of EMail sending using SMTP, including file attach - ACTIVE!!
> OverbyteIcsMailSndAsync.dpr   Example of parallel EMail sending with multiple connections
> OverbyteIcsMimeDemo.dpr       Example of EMail decoding (attached files are extracted) - ACTIVE!!
> OverbyteIcsNewsReader.dpr     Example of TNntpCli component (Send/receive newsgroups) - ACTIVE!!
> OverbyteIcsSmtpServer.dpr     Internet EMail server using SMTP protocol - ACTIVE!!

Delphi Win32/Win64 Miscellaneous applications
---------------------------------------------
.\Samples\delphi\MiscDemos\MiscDemos.bpg - Project group
> OverbyteIcsBufStrmTst.dpr     Test of buffered stream classes
> OverbyteIcsCacheTest.dpr      Test of TCacheTree class used in TSslAvlSessionCache
> OverbyteIcsMD4Test.dpr        Test program for MD4 unit
> OverbyteIcsMD5File.dpr        Example of MD5 unit: computer MD5 checksum for files
> OverbyteIcsMD5Test.dpr        Test program for MD5 unit
> OverbyteIcsOneTimePassword.dpr One Time Password test routines for OverByteIcsOneTimePw unit
> OverbyteIcsSHA1Test.dpr       Test program for SHA unit
> OverbyteIcsThreadTimerDemo.dpr Demo for TIcsThreadTimer
> OverbyteIcsTicks64Demo.dpr    GetTickCount64 test routines for OverbyteIcsTicks64 unit
> OverbyteIcsTimerDemo.dpr      Very simple demo for TIcsTimer
> OverByteIcsWndControlTest.dpr Test program for windows and threads

Delphi Win32/Win64 DNS, Ping, SNMP, Syslog sample applications
--------------------------------------------------------------
.\Samples\delphi\OtherDemos\OtherDemos.bpg - Project group
> OverbyteIcsBatchDnsLookup.dpr Batch async DNS lookup using DnsLookup (IPv6 and IPv4)
> OverbyteIcsConPing.dpr        Basic console mode demo for ping component
> OverbyteIcsDll1.dpr           Demo showing how to use a TWSocket component in a DLL
> OverbyteIcsDll2.dpr           Demo showing how to use a THttpCli component in a DLL
> OverbyteIcsDllTst.dpr         Test program calling ICSDLL1 and ICSDLL2
> OverbyteIcsDnsLook.dpr        Example of name resolution (IPv6 and IPv4)
> OverbyteIcsDnsResolver.dpr    Batch async DNS lookup event driven using DnsQuery
> OverbyteIcsFinger.dpr         Example of TFingerCli component
> OverbyteIcsNsLookup.dpr       Demo for the DnsQuery component
> OverbyteIcsPingTst.dpr        Demo for the ping component, includes trace route
> OverbyteIcsSnmpCliTst.dpr     Demo for SNMP (simple network management protocol) component
> OverbyteIcsSysLogClientDemo.dpr Demo for SysLog client component
> OverbyteIcsSysLogServerDemo.dpr Demo for SysLog server component

Delphi FireMonkey cross-platform samples (Delphi XE2 and later)
---------------------------------------------------------------
All these samples may be built for Mac OS X (and Windows).
.\Samples\delphi\PlatformDemos\XSamples.groupproj
> IcsCliDemo.dproj              Example of client for SRVDEMO, IPV4 only
> IcsTcpSrvIPv6.dproj           Basic server without client forms, event-driven, IPv4/IPV6
> IcsConSmtp.dproj              Basic console mode demo for SMTP (mail send)
> IcsMailSnd.dproj              Example of EMail sending using SMTP, including file attach
> IcsMailRcv.dproj              Internet EMail access using POP3 protocol
> IcsHttpsTst.dproj             Example of THttpCli component (GET), show many features
> IcsWebServ.dproj              Demo of HTTP server, uses TSocketServer
> IcsWebAppServ.dproj           Advanced HTTP server demo, uses WebServ, adds sessions
> IcsFtpTst.dproj               Basic graphical FTP client
> IcsFtpServ.dproj              General purpose FTP server, uses TSocketServer
> IcsUdpLstn.dproj              UDP listen demo
> IcsUdpSend.dproj              UDP send demo
> IcsBatchDnsLookup.dproj       Batch async DNS lookup using DnsLookup (IPv6 and IPv4)
> IcsDll1.dproj                 Demo showing how to use a TWSocket component in a DLL
> IcsDll2.dproj                 Demo showing how to use a THttpCli component in a DLL
> IcsDllTst.dproj               Test program calling ICSDLL1 and ICSDLL2
> IcsThreadTimerDemo.dproj      Very simple demo for TIcsTimer

 Delphi Win32/Win64 Socket sample applications
 ---------------------------------------------
 .\Samples\delphi\SocketDemos\SocketDemos.bpg - Project group
> OverbyteIcsBinCliDemo.dpr       Client program to receive binary and delimited text data. Works with BinTcpSrv demo.
> OverbyteIcsCliDemo.dpr          Example of client for SRVDEMO, IPV4 only  - ACTIVE!!
> OverbyteIcsClient5.dpr          Basic client GUI applications
> OverbyteIcsClient7.dpr          Simple client application demonstrating TWSocket
> OverbyteIcsConCli1.dpr          Basic client/server console applications
> OverbyteIcsConCli2.dpr          Basic client/server console applications with thread
> OverbyteIcsConSrv1.dpr          Basic server application in console mode
> OverbyteIcsConUdpLstn.dpr       Console application to listen for UDP messages
> OverbyteIcsDynCli.dpr           Demo of dynamically created TWSocket components
> OverbyteIcsMtSrv.dpr            Basic server, multi-threaded, see THRDSRV for better code
> OverbyteIcsRecv.dpr             Simple file receive (server), use with SENDER demo (client)
> OverbyteIcsSender.dpr           Simple file send (client), use with RECV demo (server)
> OverbyteIcsServer5.dpr          Basic server GUI applications
> OverbyteIcsSocksTst.dpr         How to use TWSocket with SOCKS protocol (firewall traversing)
> OverbyteIcsSrvDemo.dpr          Example of server using a TTable - ACTIVE!!
> OverbyteIcsSrvTcp.dpr           Basic server without client forms, event-driven
> OverbyteIcsSvcTcp.dpr           Same as SRVTCP but as an NT/2K/XP service
> OverbyteIcsTWSChat.dpr          Chat program (both client and server in a single program)
> OverbyteIcsTcpSrv.dpr           Basic server without client forms, event-driven, IPv4 only - ACTIVE!!
> OverbyteIcsTcpSrvIPv6.dpr       Basic server without client forms, event-driven, IPv4/IPV6 - ACTIVE!!
> OverbyteIcsTelnetClient.dpr     Telnet client using a TnEmulVT
> OverbyteIcsThrdSrv.dpr          Basic multithreaded TCP server, banner sent in main thread
> OverbyteIcsThrdSrvV2.dpr        Basic multithreaded TCP server, banner sent in worker thread
> OverbyteIcsThrdSrvV3.dpr        Basic TCP server showing how to use TWSocketThrdServer
> OverbyteIcsTnDemo.dpr           Telnet client using a TMemo
> OverbyteIcsTnSrv.dpr            Basic TCP server with client forms, event-driven
> OverbyteIcsUdpLstn.dpr          UDP listen demo
> OverbyteIcsUdpSend.dpr          UDP send demo

Delphi Win32/Win64 SSL-enabled sample applications
--------------------------------------------------
.\Samples\delphi\sslinternet\SslDemos.bpg - Project group
> OverbyteIcsHttpRestTst,dpr      ICS HTTPS REST and 0Auth functions demo - ACTIVE!!.
> OverbyteIcsHttpsTst.dpr         Example of TSslHttpCli component (GET) - ACTIVE!!
> OverbyteIcsJoseTst.dpr          ICS SSL Json Object Signing (Jose) Demos - ACTIVE!!
> OverbyteIcsPemTool.dpr          ICS Pem Certificate Tool - Create and import certificates in many formats  - ACTIVE!!
> OverbyteIcsProxySslServer.dpr   ICS Proxy server  - ACTIVE!!
> OverbyteIcsSimpleSslCli.dpr     Example of simple SSL client using TSslWSocket - ACTIVE!!
> OverbyteIcsSimpleSslServer.dpr  Example of SSL server using TSslWSocket - ACTIVE!!
> OverbyteIcsSslFtpServ.dpr       General purpose FTP SSL server, uses TSocketServer - ACTIVE!!
> OverbyteIcsSslFtpTst.dpr        Basic graphical FTP SSL client - ACTIVE!!
> OverbyteIcsSslMailRcv.dpr       Internet EMail access using POP3 protocol and SSL - ACTIVE!!
> OverbyteIcsSslMailSnd.dpr       Example of EMail sending using SMTP and SSL - ACTIVE!!
> OverbyteIcsSslMultiWebServ.dpr  Advanced multi host web server demo  - ACTIVE!!
> OverbyteIcsSslNewsRdr.dpr       Example of TSslNntpCli component (Send/receive newsgroups) - ACTIVE!!
> OverbyteIcsMsVerify.dpr         Verify and show an OpenSSL certificate or certificate chain using
                                     class TMsCertChainEngine which uses MS crypto API - ACTIVE!!
> OverbyteIcsSslSniSrv.dpr        Test of Server Name Indication (SNI) in server mode - ACTIVE!!
> OverbyteIcsSslWebServ.dpr       Demo of HTTPS server, uses TSocketServer - ACTIVE!!
> OverbyteIcsSslWebAppServer.dpr  Advanced HTTPS server demo, uses WebServ, adds sessions - ACTIVE!!
> OverbyteIcsSslSmtpServer.dpr    Internet EMail server using SMTP protocol and SSL - ACTIVE!!
> OverbyteIcsX509CertsTst.dpr     Automatically download SSL X509 certificates from Let's Encrypt and CertCentre AG - ACTIVE!!

Delphi Win32/Win64 HTTP sample applications (the SSL versions are preferred!)
-------------------------------------------
.\Samples\delphi\WebDemos\WebDemos.bpg - Project group
> OverbyteIcsConHttp.dpr          Basic console mode HTTP client
> OverbyteIcsHttpAsp.dpr          Example of THttpCli component with cookie (POST to an ASP page)
> OverbyteIcsHttpAsy.dpr          Example of THttpCli component with multiple async requests (GET)
> OverbyteIcsHttpChk.dpr          Example of THttpCli to check for valid URL using HEAD request
> OverbyteIcsHttpDmo.dpr          Simple HTTP client demo with proxy
> OverbyteIcsHttpGet.dpr          Example of THttpCli component (GET into a file)
> OverbyteIcsHttpMultipartDownload.dpr  Demo application for TMultipartHttpDownloader to download files using simultaneous connections
> OverbyteIcsHttpPg.dpr           Example of THttpCli component (POST to CGI script)
> OverbyteIcsHttpPost.dpr         Example of THttpCli component (POST), work with WebServ sample - ACTIVE!!
> OverbyteIcsHttpThrd.dpr         Example of THttpCli component (multi-threaded GET)
> OverbyteIcsHttpTst.dpr          Example of THttpCli component (GET), show many features - ACTIVE!!
> OverbyteIcsIsapi.dll            Example of FTP client component within an ISAPI extension
> OverbyteIcsWebAppServer.dpr     Advanced HTTP server demo, uses WebServ, adds sessions - ACTIVE!!
> OverbyteIcsWebServ.dpr          Demo of HTTP server, uses TSocketServer - ACTIVE!!


Sample Notes
------------
Note 1: Not all samples have been rewritten in C++ for C++ Builder. And those rewritten are
        frequently much simpler. So C++ Builder user: have a look at the Delphi sample too !
Note 2: Follow "UserMade" link on ICS web site to find more sample programs written by
        ICS users, although these are mostly for older versions of ICS.

As explained in the component installation, you may encounter an error loading
a sample application or running it. This may be because the last time I loaded the form,
I was using another Delphi or C++ Builder version which has new properties.
You can safely ignore messages related to those new properties. They are not used
in the samples. (The properties are CharSet, OldCreateOrder and others).
You can also encounter error about duplicate resources. You can ignore them
safely. If you have those errors, open each form in the IDE, ignore the error
then recompile. If you don't open the form in the IDE, you'll get the errors
at runtime and your program will abort.

When installing a new version, always delete old dcu, obj, dcpil and always
recompile everything !
Close everything before recompiling the library or packages.
When installing a new version, be sure to unzip it in the same directory
tree as the old one or you'll mess both versions.


About SSL:
----------

TSslWSocket and TSslWSocketServer component are derived from the standard
TWSocket and TWSocketServer component. The SSL code is compiled into the
component only if you define USE_SSL symbol to your packages and projects.
Just add USE_SSL to the defines in the project or package options and
recompile everything.

The components make use of libcrypto-1_1.dll (or libcrypto-1_1-x64.dll) and
libssl-1_1.dll (or libssl-1_1-x64).dll to handle SSL protocol stuff. The DLLs
are dynamically loaded at runtime. It means that the DLLs will only be required
at runtime when you first make use of a SSL function. Your applications will
run on systems without OpenSSL DLLs as long as you don't call any SSL function.
The ICS distribution includes the latest OpenSSL files or they may be downloaded
from:

http://wiki.overbyte.eu/wiki/index.php/ICS_Download

Most ICS components have their SSL enabled counter part. They work exactly
the same way as the regular component except when SSL specific stuff is needed,
for example certificates. To support SSL stuff, the SSL-enabled version use
some new properties, events and methods. Many sample programs have their
SSL-enabled counter part in a separate sources located in SslInternet folder.

SSL certificates:
To make use of SSL, you frequently need certificates. I provide some demo
certificates I built using command line OpenSSL tool. PEM certificates can
be opened by a text editor, LF as well as CRLF are allowed as line breaks.

CACERT.PEM :   A demo certificate for "Example CA"
01CERT.PEM :   A demo certificate which is signed by CACERT.PEM
01KEY.PEM :    A demo private key for 01CERT.PEM
               Passphrase is "password".
CLIENT.PEM :   A demo certificate and private key.
               Passphrase is "password".
SERVER.PEM :   A demo certificate and private key.
               Passphrase is "password".
ROOT.PEM :     A demo CA certificate.
               Passphrase is "password".
TRUSTEDCABUNDLE.PEM :
               A demo CA file in PEM format containing about 52
               well known root CA certificates to be specified in
               property CA Path of the demo applications. Read
               the comments included in this file.
ROOTCABUNDLE.PEM :
               A demo CA file in PEM format containing about 280
               well known root CA certificates to be specified in
               property CA Path of the demo applications. Read
               the comments included in this file.
6F6359FC.0 :   Located in sub directory SslInternet\TrustedCaStore,
               it's the file CACERT.PEM stored with a hashed file
               name. Directory TrustedCaStore can be specified in
               property CA Path of the demo applications.

For details about certificate, see the excellent book:
  "Network security with OpenSSL", O'Reilly, ISBN 10: 0-596-00270-X

The SSL demo project OverbyteIcsPemTool may be used to create self
signed PEM certificates, certificate requests for commercial use, to
convert existing certificates in the Windows Certificate Store
to PEM format understood by OpenSSL and to examine PEM certificates.

You will find more information in IcsSslHowTo.txt file.


Commercial SSL certificates:
To avoid browsers giving certificate warning messages, you need to purchase
a SSL certificate from one of numerous companies, such as Verisign, Thawte
GeoTrust or RapidSSL.  Prices vary dramatically and are often cheaper from
resellers such as Servertastic than from the main issuing companies.

The main purpose of an SSL certificate is to prove the identity of the owner
of a web site, ideally the company behind the web site.  That usually requires
paper work identifying the company is submitted and also proof the domain being
protected is owned by that company, it usually also involves telephone calls.
Such certificates are usually called fully validated and cost $120 or more each
year for a single domain, ie secure.website.com.  Wild card certificates cost
$350 or more, but protect multiple sub-domains, ie web.website.com as well.
Extended Validation certificates cost from $450 a year, and show the company name
in green in the address bar.  For testing and simple use, instant issued SSL
certificates cost from $15 per year and protect a single domain only with automated
checking reducing the cost (an email to admin@website.com to prove you receive
email for the domain, perhaps a telephone call as well).  Note these instant
certificates do not include a company name.

To buy and install an SSL certificate for use with ICS and OpenSSL follow these
steps:

1 - Build the SSL demo project OverbyteIcsPemTool. Take Extras, Create Certificate
Requests, fill in the various fields (check other certificates if uncertain, the
Common Name is the domain to protect, ie secure.website.com and E-Mail should be
an email address at the than domain, ideally admin or administrator, 2048 bits.
Click Create, and specify two file names, first for the private key (mykey.pem)
then the certificate request file (myreq.pem).  The request can also be done using
OpenSSL command line arguments, or you can build it into your own application.

2 - Choose you SSL supplier and certificate type, at some point during the ordering
process you will be asked for the certificate request, so open the PEM file you
saved with a text editor and copy the base64 encoded block starting
-BEGIN CERTIFICATE REQUEST- into the web form.  It should be decoded and displayed
so you check it's correct.  The private key is not needed for the certificate to
be issued.  At this point the validation process starts as mentioned above, which
might take hours or weeks to complete.

3 - Eventually the SSL certificate should be issued, either by email or made
available to download from the supplier's web site.  It should be in X.509 format
in a base64 encoded block starting -BEGIN CERTIFICATE- which should be saved as
a PEM file (mycert.pem).  There should also be an Intermediate CA certificate,
with which your new certificate was signed, which should also be saved as a file
(mycacert.pem). This may also be downloadable from the supplier as a bundle file
and should be common to any certificates they issue, ie RapidSSL_CA_bundle.pem.

4 - The OverbyteIcsPemTool tool has a View PEM button that allows examination of
your new PEM files.

5 - The three PEM files now need to be attached to the SslContext component in
your application, with properties SslCertFile, SslPrivKeyFile and SslCAFile.
The request certificate file has no further use.


Support:
--------

There is a mailing list to discuss F. Piette's components and applications.
To subscribe surf to http://lists.elists.org/mailman/listinfo/twsocket.
Do not use an aliased EMail address, use your real EMail address, the one
you'll use to post messages. After asking for subscription, you'll receive a
confirmation email you must reply to it or you will _not_ be added to the
subscriber's list (this is to check for email path and also make sure
someone doesn't subscribe you without your consent).

Once you have been registered with the mailing list processor, you can
send messages to twsocket@elists.org. Every subscriber will receive a copy of
your message. I will respond, but anybody is welcome to respond to each
other's messages. So every body can share his expertise. There are many other
useful mailing lists at http://www.elists.org !

Before asking a question, browse the message archive you can download from
the support page on the web site (click the "support" button from main page)
and from the mailing list web site http://lists.elists.org/mailman/listinfo/twsocket.
Google is also archiving the list with some delay.

If you found a bug, please make a short program that reproduces the problem
attach it to a message addressed to me. If I can reproduce the problem, I
can find a fix ! Do not send exe file but just source code and instructions.
Always use the latest version (beta if any) before reporting any bug.

You are also encouraged to use the support mailing list to ask for
enhancements. You are welcome to post your own code.

The support mailing list has sometimes a heavy traffic. If
it is too much for you, you can select "digest" mode in which mailing list
processor will mail you only one big message per day. To select digest mode
goto http://lists.elists.org/mailman/listinfo/twsocket.

You can also subscribe to another mailing list called twsocket-announce which
will receive only very few messages when major bug fixes or updates are done.
The subscription process is the same as for the other mailing list.
See above procedure.


Release notes
-------------

There is no global release notes. Each component and sample has his own history.
You can find those histories in the comment in the beginning of each source file.
There are also a bunch of useful comments in the source code. You should at least
browse the source for the components you are interested in.


MidWare
-------

If you wants to build client/server applications using TCP/IP protocol, you
can do it easily with ICS. But you can do it much more easily using another
freeware product from François Piette: MidWare. Available from the same web
site http://www.overbyte.be.


francois.piette@overbyte.be
francois.piette@swing.be
http://www.overbyte.be/
http://wiki.overbyte.eu/


