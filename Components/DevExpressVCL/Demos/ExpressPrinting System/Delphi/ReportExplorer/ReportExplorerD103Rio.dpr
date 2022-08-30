program ReportExplorerD103Rio;

uses
  ShlObj,
  Forms,
  dxPSCore,
  Registry,
  SysUtils,
  Windows,
  Main in 'Main.pas' {fmLauncher},
  Splash in 'Splash.pas' {fmSplash};

  {$R *.res}
  {$R WindowsXP.res}


const
  ProductID = 'DeveloperExpress.ExpressPrinting System Reports.1';
  ProductDescription = 'ExpressPrinting System Report Files Explorer';

procedure NotifyShell;
begin
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;
  
procedure RegisterApplication;

  procedure CreateDefaultKeyValue(ARegistry: TRegistry; const AKey, AValue: string);
  begin
    if ARegistry.OpenKey(AKey, True) then
    try
      ARegistry.WriteString('', AValue);
    finally
      ARegistry.CloseKey;
    end;
  end;
  
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CLASSES_ROOT;

    CreateDefaultKeyValue(Registry, '.' + dxPSCore.dxPSReportFileLongExtension, ProductID);
    CreateDefaultKeyValue(Registry, '.' + dxPSCore.dxPSReportFileShortExtension, ProductID);
    CreateDefaultKeyValue(Registry, ProductID, ProductDescription);
    CreateDefaultKeyValue(Registry, ProductID + '\CurVer', ProductID);
    CreateDefaultKeyValue(Registry, ProductID + '\DefaultIcon', Application.ExeName + ',0');
    CreateDefaultKeyValue(Registry, ProductID + '\Shell\Open\Command', Application.ExeName + ' "%1"');
  finally
    Registry.Free;
  end;
  NotifyShell;
end;

procedure UnregisterApplication;

  procedure DeleteKey(ARegistry: TRegistry; const AKey: string);
  begin
    if ARegistry.KeyExists(AKey) then 
      ARegistry.DeleteKey(AKey);
  end;
  
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CLASSES_ROOT;

    DeleteKey(Registry, ProductID + '\CurVer');
    DeleteKey(Registry, ProductID + '\DefaultIcon');
    DeleteKey(Registry, ProductID + '\Shell\Open\Command');
    DeleteKey(Registry, ProductID + '\Shell\Open');
    DeleteKey(Registry, ProductID + '\Shell');
    DeleteKey(Registry, '.' + dxPSCore.dxPSReportFileShortExtension);
    DeleteKey(Registry, '.' + dxPSCore.dxPSReportFileLongExtension);
    DeleteKey(Registry, ProductID);
  finally
    Registry.Free;
  end;
  NotifyShell;
end;

begin
  Application.Initialize;
  
  if FindCmdLineSwitch('R', ['-', '/'], True) then
  begin
    RegisterApplication;
    Halt(0);
  end;  
  if FindCmdLineSwitch('U', ['-', '/'], True) then
  begin
    UnregisterApplication;
    Halt(0);
  end;
    
  Application.Title := 'ExpressPrinting System Report Explorer';
  with TfmSplash.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
  Application.CreateForm(TfmLauncher, fmLauncher);
  Application.Run;
  
end.
