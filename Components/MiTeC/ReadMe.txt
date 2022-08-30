WWW.DOWNLOADLY.IR

MiTeC System Information Component Suite
========================================
(aka MSICS)

Revised: 25/11/2018

Legal issues:
-------------

              Copyright © 1997-2018 by Michal Mutl
              
              <michal.mutl@mitec.cz>

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
                 in the product documentation is required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

Register
--------
Registration is available via www.mitec.cz.

I send you current version of MiTeC System Information Component Suite with sources by e-mail
so don't forget deliver me your e-mail address.


Distribution pack:
------------------

.\                 Info directory
.\common           Common libraries and APIs source files (only in registered version)
.\common\D2007     compiled units containing common libraries and APIs and packages for Delphi 2007 (not in registered version)
.\common\D2010     compiled units containing common libraries and APIs and packages for Delphi 2010 (not in registered version)
.\common\DXE       compiled units containing common libraries and APIs and packages for Delphi XE (not in registered version)
.\common\DXE2      compiled units containing common libraries and APIs and packages for Delphi XE2 (not in registered version)
.\common\DXE3      compiled units containing common libraries and APIs and packages for Delphi XE3 (not in registered version)
.\common\DXE4      compiled units containing common libraries and APIs and packages for Delphi XE4 (not in registered version)
.\common\DXE5      compiled units containing common libraries and APIs and packages for Delphi XE5 (not in registered version)
.\common\DXE6      compiled units containing common libraries and APIs and packages for Delphi XE6 (not in registered version)
.\common\DXE7      compiled units containing common libraries and APIs and packages for Delphi XE7 (not in registered version)
.\common\DXE8      compiled units containing common libraries and APIs and packages for Delphi XE8 (not in registered version)
.\common\DXSeattle compiled units containing common libraries and APIs and packages for Delphi 10 Seattle (not in registered version)	
.\common\DXBerlin  compiled units containing common libraries and APIs and packages for Delphi 10.1 Berlin (not in registered version)	
.\common\DXTokyo   compiled units containing common libraries and APIs and packages for Delphi 10.2 Tokyo (not in registered version)	
.\common\DXRio     compiled units containing common libraries and APIs and packages for Delphi 10.3 Rio (not in registered version)	
.\msics            Source files (only in registered version)
.\msics\apps       Applications written using this component (only in registered version)
.\msics\demos      Sample applications
.\msics\dll        MSICS.DLL library for using MSI data from any app
.\msics\D2007      compiled units containing components and packages for Delphi 2007 (not in registered version)
.\msics\2010       compiled units containing components and packages for Delphi 2010 (not in registered version)
.\msics\DXE        compiled units containing components and packages for Delphi XE (not in registered version)
.\msics\DXE2       compiled units containing components and packages for Delphi XE2 (not in registered version)
.\msics\DXE3       compiled units containing components and packages for Delphi XE3 (not in registered version)
.\msics\DXE4       compiled units containing components and packages for Delphi XE4 (not in registered version)
.\msics\DXE5       compiled units containing components and packages for Delphi XE5 (not in registered version)
.\msics\DXE6       compiled units containing components and packages for Delphi XE6 (not in registered version)
.\msics\DXE7       compiled units containing components and packages for Delphi XE7 (not in registered version)
.\msics\DXE8       compiled units containing components and packages for Delphi X87 (not in registered version)
.\msics\DXSeattle  compiled units containing components and packages for Delphi 10 Seattle (not in registered version)
.\msics\DXBerlin   compiled units containing components and packages for Delphi 10.1 Berlin (not in registered version)
.\msics\DXTokyo    compiled units containing components and packages for Delphi 10.2 Tokyo (not in registered version)
.\msics\DXRio      compiled units containing components and packages for Delphi 10.3 Rio (not in registered version)

Installation:
-------------
Remove all old versions of MSICS before intallation of the new one!

Using Delphi, do a file/open and choose MiTeC_Common_xxx.dpk which is a runtime package containing common used routines. Click compile button. 
Then open MSICS_xxx_Rtl.dpk (runtime source for all components) and then click on the Compile button. 
Then open designtime package MSICS_xxx_Dsgn.dpk and then click on the Install button. 

You should see the "MSIC Suite" tab on the component palette.

!!! Be sure to uncheck 'Stop On Delphi Exception' option in Debugger Options|Language Exceptions.


Support:
--------
If you have some problem or suspicion something bad, send me e-mail to
<michal.mutl@mitec.cz> describe your problem and attach SIF file generated
by TMiTeC_SystemInfo.SaveToStorage. If you cannot obtain this file add following info
to this message: OS platform and version
                 Delphi version
                 MSICS version.
I will process all suggestions but reply only someones.


Contributors:
---------------
Richard Berends
Tony Hjort (DSI DATA A/S)
Klaus Holtorf
Dave Murphy (CSC - GIS EMEA DSE Build Engineering)
Andreas Scheller


Michal Mutl
E-Mail: <michal.mutl@mitec.cz>
Internet: http://www.mitec.cz/
