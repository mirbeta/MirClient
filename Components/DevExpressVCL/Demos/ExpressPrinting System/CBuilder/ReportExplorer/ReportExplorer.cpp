//---------------------------------------------------------------------------
#include <vcl.h>
#include <Registry.hpp>
#include <shlobj.h>
#include "dxPSCore.hpp"
#pragma hdrstop
USERES("ReportExplorer.res");
USEFORM("Main.cpp", fmLauncher);
USEFORM("Splash.cpp", fmSplash);
//---------------------------------------------------------------------------
const String ProductID = "DeveloperExpress.ExpressPrinting System Reports.1";
const String ProductDescription = "ExpressPrinting System Report Files Explorer";

void CreateDefaultKeyValue(TRegistry *ARegistry, const String AKey,
  const String AValue)
{
  if (ARegistry->OpenKey(AKey, true))
    __try{
      ARegistry->WriteString("", AValue);
    }
    __finally{
      ARegistry->CloseKey();
    }
}

void RegisterApplication()
{
  TRegistry *Registry = new TRegistry();
  __try{
    Registry->RootKey = HKEY_CLASSES_ROOT;
    CreateDefaultKeyValue(Registry, (String)"." + dxPSReportFileLongExtension, ProductID);
    CreateDefaultKeyValue(Registry, (String)"." + dxPSReportFileShortExtension, ProductID);
    CreateDefaultKeyValue(Registry, ProductID, ProductDescription);
    CreateDefaultKeyValue(Registry, ProductID + "\\CurVer", ProductID);
    CreateDefaultKeyValue(Registry, ProductID + "\\DefaultIcon", Application->ExeName + ",0");
    CreateDefaultKeyValue(Registry, ProductID + "\\Shell\\Open\\Command", Application->ExeName + " %1");
  }
  __finally{
    delete Registry;
  }
}

void DeleteKey(TRegistry *ARegistry, const String AKey)
{
  if (ARegistry->KeyExists(AKey))
    ARegistry->DeleteKey(AKey);
}

void UnregisterApplication()
{
  TRegistry *Registry = new TRegistry();
  __try{
    Registry->RootKey = HKEY_CLASSES_ROOT;

    DeleteKey(Registry, ProductID + "\\CurVer");
    DeleteKey(Registry, ProductID + "\\DefaultIcon");
    DeleteKey(Registry, ProductID + "\\Shell\\Open\\Command");
    DeleteKey(Registry, ProductID + "\\Shell\\Open");
    DeleteKey(Registry, ProductID + "\\Shell");
    DeleteKey(Registry, '.' + dxPSReportFileShortExtension);
    DeleteKey(Registry, '.' + dxPSReportFileLongExtension);
    DeleteKey(Registry, ProductID);
  }
  __finally{
    delete Registry;
  }
}

void SHAssocChangedNotify()
{
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, NULL, NULL);
}

//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();

     if (FindCmdLineSwitch('R', TSysCharSet()<<'-'<<'/', true))
     {
       RegisterApplication();
       SHAssocChangedNotify();
       exit(EXIT_SUCCESS);
     }
     if (FindCmdLineSwitch('U', TSysCharSet()<<'-'<<'/', true))
     {
       UnregisterApplication();
       SHAssocChangedNotify();
       exit(EXIT_SUCCESS);
     }


     Application->Title = "ExpressPrinting System Reports Explorer";
     Application->CreateForm(__classid(TfmLauncher), &fmLauncher);
     Application->Run();
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
