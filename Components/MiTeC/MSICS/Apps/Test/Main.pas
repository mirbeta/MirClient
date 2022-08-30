unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, CheckLst, Menus, MSI_Common, MSI_Defs;

type
  Twnd_msicstest_Main = class(TForm)
    bClose: TButton;
    Box: TMemo;
    bRun: TButton;
    cbxReport: TCheckBox;
    GroupBox1: TGroupBox;
    clb: TCheckListBox;
    PopupMenu1: TPopupMenu;
    Checkall1: TMenuItem;
    Uncheckall1: TMenuItem;
    Inverse1: TMenuItem;
    GroupBox2: TGroupBox;
    clbPL: TCheckListBox;
    cbxSI: TCheckBox;
    bShutdown: TButton;
    bRunAsAdmin: TButton;
    procedure bCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bRunClick(Sender: TObject);
    procedure Checkall1Click(Sender: TObject);
    procedure Inverse1Click(Sender: TObject);
    procedure bShutdownClick(Sender: TObject);
    procedure bRunAsAdminClick(Sender: TObject);
  private
    st: Int64;
    hs,hss,hsf: THeapStatus;
    ec: Integer;
  public
    procedure CheckAll;

    procedure Log(AMessage: string);

  end;

var
  wnd_msicstest_Main: Twnd_msicstest_Main;

implementation

uses Registry, MSI_CPU, MSI_Machine, MSI_Devices, MSI_Display, MSI_Network, MSI_Media,
  MSI_Memory, MSI_Engines, MSI_APM, MSI_Disk, MSI_OS, MSI_SMBIOS, MSI_BT,
  MSI_Printers, MSI_Software, MSI_Startup, MSI_Storage, MSI_SystemInfo, MSI_MSProduct,
  MSI_USB, MSI_Processes, MSI_Monitor, MiTeC_Routines, MiTeC_Shares, MiTeC_StrUtils,
  MSI_EventLog, MSI_AD, MiTeC_SysUtils, MiTeC_Dialogs, MSI_Security, MSI_WIFI,
  MiTeC_NetUtils, MiTeC_SIF, MSI_NetCreds, MSI_USBHistory, MSI_WLANC, MSI_FW;

{$R *.DFM}

procedure Twnd_msicstest_Main.bShutdownClick(Sender: TObject);
begin
  if not YesNo('Power off this machine?') then
    Exit;
  WindowsExit(EWX_POWEROFF);
end;

procedure Twnd_msicstest_Main.bCloseClick(Sender: TObject);
begin
  Close;
end;

procedure Twnd_msicstest_Main.CheckAll;
var
  pl: TMiTeC_ProcessList;
  tl,n,i: DWORD;
  th: THandle;
  s,ss: string;
  h: Boolean;
begin
  tl:=0;
  ec:=0;
  Screen.Cursor:=crHourGlass;
  try
  Box.Lines.Add('MSICS '+cVersion+' Testing Application');
  Box.Lines.Add('--------------------------------------------------------------');
  ss:=ChangeFileExt(Application.ExeName,cSIFExt);
  Box.Lines.Add('Compiler: '+GetDelphi);
  {$IFDEF WIN32}
  Box.Lines.Add('Target platform: 32-bit');
  {$ENDIF}
  {$IFDEF WIN64}
  Box.Lines.Add('Target platform: 64-bit');
  {$ENDIF}
  Box.Lines.Add(Format('OS: %s %d.%d.%d %s ',[OSName,OSVIX.dwMajorVersion,OSVIX.dwMinorVersion,OSVIX.dwBuildNumber,OSEdition]));
  Box.Lines.Add(Format('True OS version: %s ',[TrueWindowsVersion]));
  if CompatibilityMode then
    Box.Lines.Add('Compatibility Mode: Active');
  Box.Lines.Add(Format('Installation: %s',[InstallationType]));
  Box.Lines.Add(Format('BuildLab: %s',[BuildLab]));
  with TMiTeC_Memory.Create(nil) do
    try
      try
        RefreshData;
        Box.Lines.Add(Format('Memory: %d MB (%d KB free)',[PhysicalTotal div 1024 div 1024,PhysicalFree div 1024]));
      except
      end;
    finally
      Free;
    end;
  with TMiTeC_CPU.Create(nil) do
    try
      try
        RefreshData;
        Box.Lines.Add(Format('CPU: %d x %s %s - %d Mhz (%d cores, %d threads)',[CPUPhysicalCount,
                                                                                cVendorNames[Vendor].Prefix,
                                                                                CPUName,Frequency,
                                                                                CPUPhysicalCount*CorePerPackage,
                                                                                CPUPhysicalCount*LogicalPerPackage]));
      except
      end;
    finally
      Free;
    end;

  if IsAdmin then
    Box.Lines.Add(Format('Admin privs: %s',[BoolToStr(IsAdmin,True)]));
  if IsUACEnabled then
    Box.Lines.Add(Format('UAC: %s',[BoolToStr(IsUAC,True)]))
  else
    Box.Lines.Add('UAC: Disabled');
  Box.Lines.Add(Format('Elevated privs: %s',[BoolToStr(IsElevated,True)]));
  Box.Lines.Add('Session: '+GetSessionStr(Session));
  Box.Lines.Add('--------------------------------------------------------------');
  Box.Lines.Add('');

  try
  if clb.Checked[clb.Items.IndexOf('Devices')] then begin
    Log('Devices.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_Devices.Create(nil) do begin
      Log('Devices.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Devices.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Devices.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Devices.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('CPU')] then begin
    Log('CPU.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_CPU.Create(nil) do begin
      Log('CPU.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('CPU.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('CPU.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('CPU.Destroy...');
      Free;
    end;
    hsf:=GetHeapStatus;
    if hsf.TotalAllocated>hss.TotalAllocated then begin
      tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
      Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
    end;
    Log('');
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Memory')] then begin
    Log('Memory.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_Memory.Create(nil) do begin
      Log('Memory.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Memory.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Memory.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Memory.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('SMBIOS')] then begin
    Log('SMBIOS.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_SMBIOS.Create(nil) do begin
      Log('SMBIOS.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('SMBIOS.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('SMBIOS.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('SMBIOS.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('BIOS')] then begin
    Log('BIOS.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_BIOS.Create(nil) do begin
      Log('BIOS.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('BIOS.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('BIOS.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('BIOS.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Machine')] then begin
    Log('Machine.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_Machine.Create(nil) do begin
      Log('Machine.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Machine.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Machine.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Machine.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Display')] then begin
    Log('Display.Create..');
    hss:=GetHeapStatus;
    with TMiTeC_Display.Create(nil) do begin
      Log('Display.RefreshData..');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Display.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Display.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Display.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Monitor')] then begin
    Log('Monitor.Create..');
    hss:=GetHeapStatus;
    with TMiTeC_Monitor.Create(nil) do begin
      Log('Monitor.RefreshData..');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Monitor.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Monitor.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Monitor.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Winsock')] then begin
    Log('Winsock.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_Winsock.Create(nil) do begin
      Log('Winsock.RefreshData...');
      RefreshData;
      Log('Winsock.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('TCPIP')] then begin
    Log('TCPIP.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_TCPIP.Create(nil) do begin
      Log('TCPIP.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('TCPIP.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('TCPIP.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('TCPIP.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Network')] then begin
    Log('Network.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_Network.Create(nil) do begin
      Log('Network.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Network.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Network.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Network.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Network Credentials')] then begin
    Log('Network Credentials.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_NetCreds.Create(nil) do begin
      Log('Network Credentials.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Network Credentials.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Network Credentials.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Network Credentials.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Media')] then begin
    Log('Media.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_Media.Create(nil) do begin
      Log('Media.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Media.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Media.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Media.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Internet')] then begin
    Log('Internet.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_Internet.Create(nil) do begin
      Log('Internet.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Internet.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Internet.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Internet.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('LocaleInfo')] then begin
    Log('LocaleInfo.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_LocaleInfo.Create(nil) do begin
      Log('LocaleInfo.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('LocaleInfo.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('LocaleInfo.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('LocaleInfo.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('TimeZone')] then begin
    Log('TimeZone.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_TimeZone.Create(nil) do begin
      Log('TimeZone.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('TimeZone.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('TimeZone.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('TimeZone.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('OS')] then begin
    Log('OS.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_OperatingSystem.Create(nil) do begin
      Log('OS.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('OS.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('OS.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('OS.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Storage')] then begin
    Log('Storage.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_Storage.Create(nil) do begin
      Log('Storage.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Storage.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Storage.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Storage.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('DirectX')] then begin
    Log('DirectX.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_DirectX.Create(nil) do begin
      Log('DirectX.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('DirectX.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('DirectX.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('DirectX.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('ASPI32')] then begin
    Log('ASPI32.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_ASPI32.Create(nil) do begin
      Log('ASPI32.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('ASPI32.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('ASPI32.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('ASPI32.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Engines')] then begin
    Log('Engines.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_Engines.Create(nil) do begin
      Log('Engines.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Engines.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Engines.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Engines.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('APM')] then begin
    Log('APM.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_APM.Create(nil) do begin
      Log('APM.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('APM.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('APM.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('APM.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Disk')] then begin
    Log('Disk.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_Disk.Create(nil) do begin
      Log('Disk.RefreshData...');
      RefreshData;
      Log('Disk.SetDisk...');
      Drive:='C';
      if cbxReport.Checked then begin
        Log('Disk.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Disk.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Disk.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Printers')] then begin
    Log('Printers.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_Printers.Create(nil) do begin
      Log('Printers.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Printers.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Printers.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Printers.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Software')] then begin
    Log('Software.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_Software.Create(nil) do begin
      Log('Software.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Software.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Software.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Software.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('MSProduct')] then begin
    Log('MSProduct.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_MSProduct.Create(nil) do begin
      Log('MSProduct.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('MSProduct.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('MSProduct.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('MSProduct.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Startup')] then begin
    Log('Startup.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_Startup.Create(nil) do begin
      Log('Startup.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Startup.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Startup.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Startup.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('ProcessList')] then begin
    Log('ProcessList.Create...');
    hss:=GetHeapStatus;
    pl:=TMiTeC_ProcessList.Create(nil);
    pl.DetectionRange:=[];
    for i:=0 to clbPL.Items.Count-1 do
      if clbPL.Checked[i] then
        pl.DetectionRange:=pl.DetectionRange+[TDetectionType(i)];
    with pl do begin
      Log('ProcessList.RefreshData...');
      RefreshData;
      Log(Format('* Process count: %d',[pl.ProcessCount]));
      Log(Format('* Service count: %d',[pl.ServiceCount]));
      if cbxReport.Checked then begin
        Log('ProcessList.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('ProcessList.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('ProcessList.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('USB')] then begin
    Log('USB.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_USB.Create(nil) do begin
      Log('USB.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('USB.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('USB.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('USB.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('USBHistory')] then begin
    Log('USBHistory.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_USBHistory.Create(nil) do begin
      Log('USBHistory.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('USBHistory.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('USBHistory.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('USBHistory.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Bluetooth')] then begin
    Log('BT.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_BT.Create(nil) do begin
      Log('BT.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('BT.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('BT.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('BT.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('ActiveDirectory')] then begin
    Log('ActiveDirectory.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_AD.Create(nil) do begin
      Log('ActiveDirectory.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('ActiveDirectory.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('ActiveDirectory.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('ActiveDirectory.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('EventLog')] then begin
    Log('EventLog.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_EventLog.Create(nil) do begin
      Log('EventLog.RefreshData...');
      SourceName:='System';
      if cbxReport.Checked then begin
        Log('EventLog.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('EventLog.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('EventLog.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Firewall')] then begin
    Log('Firewall.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_Firewall.Create(nil) do begin
      Log('Firewall.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Firewall.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Firewall.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Firewall.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('Security')] then begin
    Log('Security.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_Security.Create(nil) do begin
      Log('Security.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('Security.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('Security.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('Security.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('WIFI')] then begin
    Log('WIFI.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_WIFI.Create(nil) do begin
      Log('WIFI.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('WIFI.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('WIFI.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('WIFI.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if clb.Checked[clb.Items.IndexOf('WLANC')] then begin
    Log('WLANC.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_WLANC.Create(nil) do begin
      Log('WLANC.RefreshData...');
      RefreshData;
      if cbxReport.Checked then begin
        Log('WLANC.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('WLANC.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;
      Log('WLANC.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR = %s'#13#10,[e.Message]));
  end end;

  try
  if cbxSI.Checked then begin
    Log('SystemInfo.Create...');
    hss:=GetHeapStatus;
    with TMiTeC_SystemInfo.Create(Self) do begin
      Log('SystemInfo.Refresh...');
      try
        RefreshData;
      finally
        s:=Debug;
      end;
      if cbxReport.Checked then begin
        Log('SystemInfo.SaveToStorage...');
        h:=True;
        SaveToStorage(ss,h);
        Log('SystemInfo.LoadFromStorage...');
        h:=True;
        LoadFromStorage(ss,h);
      end;

      Log('SystemInfo.Destroy...');
      Free;
      hsf:=GetHeapStatus;
      if hsf.TotalAllocated>hss.TotalAllocated then begin
        tl:=tl+hsf.TotalAllocated-hss.TotalAllocated;
        Log(Format('*MEMORY LEAK = %d B',[hsf.TotalAllocated-hss.TotalAllocated]));
      end;
      Log('');
    end;
  end;
  except on e: Exception do begin
    Inc(ec);
    Log(Format('*ERROR[%s] = %s'#13#10,[s,e.Message]));
  end end;

  if tl>0 then begin
    Log(Format('* TOTAL MEMORY LEAK = %d B',[tl]));
    Log('');
  end;

  if ec>0 then begin
    Log(Format('* TOTAL ERRORS = %d',[ec]));
    Log('');
  end;

  Log('Test finished.');

  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure Twnd_msicstest_Main.Log(AMessage: string);
var
  _hs: THeapStatus;
begin
  if (Box.Lines.Count>0) and (Box.Lines[Box.Lines.Count-1]<>'') and (Pos('*',Box.Lines[Box.Lines.Count-1])<>1) then begin
    _hs:=GetHeapStatus;
    Box.Lines[Box.Lines.Count-1]:=Box.Lines[Box.Lines.Count-1]+'OK '+Format('(%1.2f s / %d B)',[(GetTickCount64-st)/1000,_hs.TotalAllocated-hs.TotalAllocated]);
  end;
  if Pos('...',AMessage)>0 then begin
    st:=GetTickCount64;
    hs:=GetHeapStatus;
  end;
  Box.Lines.Add(AMessage);
  try
    Box.Lines.SaveToFile(ChangeFileExt(Application.ExeName,'.log'));
  except
  end;
  Update;
end;

procedure Twnd_msicstest_Main.FormCreate(Sender: TObject);
begin
  clbPL.CheckAll(cbChecked);
  EnablePrivilege(SE_DEBUG_NAME);
  EnablePrivilege(SE_SECURITY_NAME);
  bRunAsAdmin.Visible:=((OS>=osVista) and not IsElevated) or not IsAdmin;
end;

procedure Twnd_msicstest_Main.bRunAsAdminClick(Sender: TObject);
begin
  if RunAsAdmin(Handle,Application.ExeName,'') then
    Close;
end;

procedure Twnd_msicstest_Main.bRunClick(Sender: TObject);
begin
  Box.Lines.Clear;
  CheckAll;
end;

procedure Twnd_msicstest_Main.Checkall1Click(Sender: TObject);
var
  i: Integer;
begin
 for i:=0 to clb.Items.Count-1 do
   clb.Checked[i]:=TComponent(Sender).Tag=1;
end;

procedure Twnd_msicstest_Main.Inverse1Click(Sender: TObject);
var
  i: Integer;
begin
 for i:=0 to clb.Items.Count-1 do
   clb.Checked[i]:=not clb.Checked[i];
end;

end.
