{$INCLUDE ..\..\Compilers.inc}
unit Viewer;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.Messages, VCL.Graphics, VCL.Controls,
     VCL.Forms, VCL.StdCtrls, VCL.Dialogs, VCL.Menus, VCL.ExtCtrls, VCL.ComCtrls, VCL.Buttons,
     {$IFDEF THEMESUPPORT}WinAPI.UxTheme,{$ENDIF}
     {$IFDEF RAD15PLUS}System.Threading,{$ENDIF}
     {$ELSE}
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
     Dialogs, Menus, ImgList, ComCtrls, ExtCtrls, StdCtrls, Buttons,
     {$IFDEF THEMESUPPORT}UxTheme,{$ENDIF}
     {$ENDIF}
     {$IFDEF RAD17PLUS}System.ImageList,System.UITypes,{$ENDIF}
     MSI_SystemInfo, MSI_SysMon, MSI_Common, MSI_Memory, MSI_Defs;


type
  TCategory = (msicMachine, msicCPU, msicCPUCache, msicCPUFS, msicCPUFSStd1,
               msicCPUFSStd2,msicCPUFSExt1,msicCPUFSExt2,msicCPUFSAPM, msicMemory,
               msicMotherboard, msicBus, msicVideo,msicMonitor, msicAudio,
               msicNetwork,msicPrinter, msicDevice,  msicSMBIOS, msicUSB,
               msicWinStorage, msicPhysDrive, msicLogDrive, msicNetDrive, msicRemovableDrive, msicOtherDrive,
               msicASPI, msicSMBIOSMemoryModule, msicSMBIOSMemoryDevice, msicSMBIOSProcessor,
               msicSMBIOSMemCtrl, msicSMBIOSPMA, msicSMBIOSSystemSlot, msicSMBIOSCache,
               msicSMBIOSPort, msicSMBIOSChassis,msicSMBIOSBIOS, msicSMBIOSSystem,
               msicSMBIOSMainboard, msicSMBIOSOnBoardDevice, msicSMBIOSOnBoardDeviceEx, msicSMBIOSVoltageProbe,
               msicSMBIOSTemperatureProbe, msicSMBIOSCurrentProbe, msicSMBIOSCoolingDevice,
               msicSMBIOSTPMDevice, msicSMBIOSSystemPowerSupply, msicSMBIOSBattery, msicOther);

  Tmdi_msi_Viewer = class(TForm)
    sb: TStatusBar;
    List: TListView;
    Splitter1: TSplitter;
    TreeImages: TImageList;
    Tree: TTreeView;
    Header: TPanel;
    lMod: TLabel;
    lHeader: TLabel;
    procedure acTextOverviewExecute(Sender: TObject);
    procedure acOverviewExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure cmExit(Sender: TObject);
    procedure ListAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure ListAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure TreeGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure acFullExpandExecute(Sender: TObject);
    procedure acFullCollapseExecute(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetTreePath(ANode: TTreeNode): string;
    procedure CreateTree;
    procedure CreateCPUTree(ARoot: TTreeNode);
    procedure CreateMemoryTree(ARoot: TTreeNode);
    procedure CreateMotherboardTree(ARoot: TTreeNode);
    procedure CreateBusTree(ARoot: TTreeNode);
    procedure CreateVideoTree(ARoot: TTreeNode);
    procedure CreateMonitorTree(ARoot: TTreeNode);
    procedure CreateStorageTree(ARoot: TTreeNode);
    procedure CreateAudioTree(ARoot: TTreeNode);
    procedure CreateNetworkTree(ARoot: TTreeNode);
    procedure CreatePortsTree(ARoot: TTreeNode);
    procedure CreateUSBTree(ARoot: TTreeNode);
    procedure CreatePrintersTree(ARoot: TTreeNode);
    procedure CreateBTTree(ARoot: TTreeNode);

    procedure DisplayMachineInfo;
    procedure DisplayCPUInfo;
    procedure DisplayCPUDetail(AIndex: Integer);
    procedure DisplayCPUCache(AIndex: Integer);
    procedure DisplayCPUFS(AIndex: Integer);
    procedure DisplayCPUFSStd1(AIndex: Integer);
    procedure DisplayCPUFSStd2(AIndex: Integer);
    procedure DisplayCPUFSExt1(AIndex: Integer);
    procedure DisplayCPUFSExt2(AIndex: Integer);
    procedure DisplayCPUFSAPM(AIndex: Integer);
    procedure DisplayMemoryInfo;
    procedure DisplaySMBIOSInfo;
    procedure DisplaySMBIOSSystemInfo;
    procedure DisplaySMBIOSChassisInfo;
    procedure DisplaySMBIOSMainboardInfo;
    procedure DisplaySMBIOSBIOSInfo;
    procedure DisplaySMBIOSMemCtrlInfo;
    procedure DisplaySMBIOSPMAInfo;
    procedure DisplaySMBIOSMemoryModuleDetail(AIndex: Integer);
    procedure DisplaySMBIOSMemoryDeviceDetail(AIndex: Integer);
    procedure DisplaySMBIOSProcessorDetail(AIndex: Integer);
    procedure DisplaySMBIOSCacheDetail(AIndex: Integer);
    procedure DisplaySMBIOSSystemSlotDetail(AIndex: Integer);
    procedure DisplaySMBIOSPortDetail(AIndex: Integer);
    procedure DisplaySMBIOSOnBoardDeviceDetail(AIndex: Integer);
    procedure DisplaySMBIOSOnBoardDeviceExDetail(AIndex: Integer);
    procedure DisplaySMBIOSTempProbeDetail(AIndex: Integer);
    procedure DisplaySMBIOSVoltProbeDetail(AIndex: Integer);
    procedure DisplaySMBIOSCurrProbeDetail(AIndex: Integer);
    procedure DisplaySMBIOSCoolDevDetail(AIndex: Integer);
    procedure DisplaySMBIOSTPMDevDetail(AIndex: Integer);
    procedure DisplaySMBIOSBatteryDetail(AIndex: Integer);
    procedure DisplaySMBIOSSystemPowerSupply;
    procedure DisplayMotherboardInfo;
    procedure DisplayDeviceInfo;
    procedure DisplayDeviceDetail(AIndex: Integer);
    procedure DisplayVideoDetail(AIndex: Integer);
    procedure DisplayVideoProps;
    procedure DisplayMonitorDetail(AIndex: Integer);
    procedure DisplayPhysDriveDetail(AIndex: Integer);
    procedure DisplayLogDriveDetail(AIndex: Integer);
    procedure DisplayOtherDriveDetail(AIndex: Integer);
    procedure DisplayWinStorageDetail(AIndex: Integer);
    procedure DisplayASPIDetail(AIndex: Integer);
    procedure DisplayAudioDetail;
    procedure DisplayNetworkInfo;
    procedure DisplayNetworkDetail(AIndex: Integer);
    procedure DisplayUSBDetail(AIndex: Integer);
    procedure DisplayUSBSummary;
    procedure DisplayPrinterDetail(AIndex: Integer);
    procedure DisplayNodeInfo;

    procedure LoadFromSIF(AComponent: TMiTeC_Component);
  public
    SIC: TMiTeC_SystemInfo;
    procedure RefreshData;

    procedure OpenFile(AFilename: string);
    procedure OpenLocal;
    procedure SetControls;
  end;

var
  mdi_msi_Viewer: Tmdi_msi_Viewer;

implementation

uses {$IFDEF RAD9PLUS}VCL.Clipbrd,{$ELSE}Clipbrd,{$ENDIF}
  Main, MSI_CPU, MSI_SMBIOS, MSI_Devices, MSI_USB, MiTeC_USB, MiTeC_CtrlRtns,
  MiTeC_Datetime, MiTeC_Routines, MiTeC_NTDDK, MSI_Network, MSI_Machine,
  MSI_Splash, MiTeC_WinIOCTL, MiTeC_StrUtils, EODlg, MiTeC_SysUtils,
  OverviewDlg, MiTeC_Storage, Codecs, MiTeC_SIF, MiTeC_Windows, MiTeC_Helpers,
  LngResources, Math;

{$R *.dfm}

procedure Tmdi_msi_Viewer.RefreshData;
var
  so: TScanObjects;
  sir: TStorageInfoRecord;
  {$if defined(RAD15PLUS) and not defined(DEBUG)}
  t: ITask;
  {$ifend}
begin
  Screen.Cursor:=crHourglass;
  ShowSplash;
  if not SIC.LiveData then begin
    Header.Color:=$00727272;
  end;
  try
    if SIC.LiveData then begin
      so:=soAll;
      if not Prefs.ActiveDirectory then
        so:=so-[soAD];
      if not Prefs.EventLog then
        so:=so-[soEventLog];
      SIC.OS.DetectUpdatesAndHotfixes:=Prefs.Updates;
      {$if defined(RAD15PLUS) and not defined(DEBUG)}
      SetMainMenu(wnd_msi_Main.MainMenu,False);
      try
        t:=TTask.Run(procedure begin
                       SIC.RefreshData(so)
                     end);
        while (t.Status<>TTaskStatus.Completed) do
          Application.ProcessMessages;
      finally
        SetMainMenu(wnd_msi_Main.MainMenu,True);
      end;
      {$ELSE}
      SIC.RefreshData(so);
      {$ifend}

      lHeader.Caption:=Format(' %s [%s]',[SIC.Machine.MachineName,Trim(SIC.OS.OSName+' '+SIC.OS.OSEdition)]);
      lMod.Caption:=Format('Created: %s ',[DateTimeToStr(now)]);
    end else begin
      {$if defined(RAD15PLUS) and not defined(DEBUG)}
      SetMainMenu(wnd_msi_Main.MainMenu,False);
      try
        t:=TTask.Run(procedure
                     begin
                       LoadFromSIF(SIC);
                     end);
        while (t.Status<>TTaskStatus.Completed) do
          Application.ProcessMessages;
      finally
        SetMainMenu(wnd_msi_Main.MainMenu,True);
      end;
      {$ELSE}
      LoadFromSIF(SIC);
      {$ifend}
      if SIC.StorageInfo.OSName='' then
        lHeader.Caption:=Format(' %s [%s]',[SIC.Machine.MachineName,Trim(SIC.OS.OSName+' '+SIC.OS.OSEdition)])
      else
      lHeader.Caption:=Format(' %s [%s]',[SIC.StorageInfo.Machine,Trim(SIC.StorageInfo.OSName+' '+SIC.StorageInfo.OSEdition)]);
      lMod.Caption:=Format('Created: %s ',[DateTimeToStr(SIC.StorageInfo.Timestamp)]);
    end;
  finally
    CreateTree;
    HideSplash;
    Screen.Cursor:=crDefault;
  end;
end;

procedure Tmdi_msi_Viewer.CreateTree;
var
  Root: TTreeNode;
  pi: PInteger;
begin
  with Tree, Items do begin
    BeginUpdate;
    try
      Clear;

      new(pi);
      pi^:=Join(0,Word(msicMachine));
      if SIC.Machine.Computer='' then
        Root:=AddObject(nil,rsComputer,pi)
      else
        Root:=AddObject(nil,SIC.Machine.Computer,pi);

      CreateCPUTree(Root);
      CreateMemoryTree(Root);
      CreateMotherboardTree(Root);
      CreateBusTree(Root);
      CreateVideoTree(Root);
      CreateMonitorTree(Root);
      CreateStorageTree(Root);
      CreateUSBTree(Root);
      CreateAudioTree(Root);
      CreateNetworkTree(Root);
      CreatePortsTree(Root);
      CreatePrintersTree(Root);
    finally
      EndUpdate;
      Root.Expand(False);
      Tree.Selected:=Tree.Items.GetFirstNode;
      Tree.OnChange(Tree,Tree.Selected);
    end;
  end;
end;

procedure Tmdi_msi_Viewer.CreateUSBTree(ARoot: TTreeNode);
var
  r,n2: TTreeNode;
  pi: PInteger;
  i: Integer;
  s: string;

function FindUSBNode(ARoot: TTreeNode; AIndex: Integer): TTreeNode;
var
  n: TTreeNode;
begin
  Result:=nil;
  n:=ARoot;
  while Assigned(n) do begin
    if Assigned(n.Data) and (Lo(PInteger(n.Data)^)-1=AIndex) then begin
      Result:=n;
      Break;
    end;
    n:=n.GetNext;
  end;
  if not Assigned(Result) then
    Result:=ARoot;
end;

begin
  if SIC.USB.USBNodeCount>0 then
    with Tree.Items do
      with SIC.USB do begin
        new(pi);
        pi^:=Join(0,Word(msicUSB));
        n2:=AddChildObject(ARoot,rsUSB,pi);
        for i:=0 to USBNodeCount-1 do
          with USBNodes[i] do begin
            s:='';
            case USBClass of
              usbHostController: s:=s+Format('%s %d',[ClassNames[integer(USBClass)],USBDevice.Port]);
              usbHub: s:=s+ClassNames[integer(USBClass)];
              else begin
                if USBDevice.ConnectionStatus=1 then begin
                  if USBClass=usbExternalHub then
                    s:=s+Format('Port[%d]: %s',[USBDevice.Port,ClassNames[integer(USBClass)]])
                  else
                    s:=s+Format('Port[%d]: %s',[USBDevice.Port,{ClassNames[integer(USBClass)}USBDevice.USBClassname]);
                end else begin
                  s:=s+Format('Port[%d]: %s',[USBDevice.Port,ConnectionStates[USBDevice.ConnectionStatus]]);
                end;
              end;
          end;
        r:=FindUSBNode(n2,ParentIndex);
        new(pi);
        if (USBClass in [usbReserved..usbStorage,usbVendorSpec,usbUnknown,usbError]) then
          pi^:=Join(i+1,Word(msicUSB))
        else
          pi^:=Join(i+1,Word(msicOther));
        Tree.Items.AddChildObject(r,s,pi);
      end;
    end;
end;

procedure Tmdi_msi_Viewer.TreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node) and Assigned(Node.Data) then
    Dispose(PInteger(Node.Data));
end;

procedure Tmdi_msi_Viewer.CreateMotherboardTree(ARoot: TTreeNode);
var
  n1,n2,n3: TTreeNode;
  pi: PInteger;
  i: Integer;
  s: string;
begin
  with Tree, Items do begin
    new(pi);
    pi^:=Join(0,Word(msicMotherboard));
    n1:=AddChildObject(ARoot,rsMotherboard,pi);
    new(pi);
    pi^:=Join(0,Word(msicDevice));
    n2:=AddChildObject(n1,Format('%s %s',[rsACPI,rsDevices]),pi);
    with SIC.Devices do
      for i:=0 to DeviceCount-1 do
        if (Pos('ACPI\',Devices[i].HardwareID)=1) then begin
          new(pi);
          pi^:=Join(i+1,Word(msicDevice));
          AddChildObject(n2,Devices[i].Name,pi);
        end;
    new(pi);
    pi^:=Join(0,Word(msicSMBIOS));
    n2:=AddChildObject(n1,rsSMBIOS,pi);
    new(pi);
    pi^:=Join(0,Word(msicSMBIOSBIOS));
    AddChildObject(n2,rsBIOS,pi);
    new(pi);
    pi^:=Join(0,Word(msicSMBIOSSystem));
    AddChildObject(n2,rsSystem,pi);
    new(pi);
    pi^:=Join(0,Word(msicSMBIOSMainboard));
    AddChildObject(n2,rsMainboard,pi);
    new(pi);
    pi^:=Join(0,Word(msicSMBIOSChassis));
    AddChildObject(n2,rsChassis,pi);
    new(pi);
    pi^:=Join(0,Word(msicSMBIOSSystemPowerSupply));
    AddChildObject(n2,rsSystemPowerSupply,pi);
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,rsBatteries,pi);
    with SIC.Machine.SMBIOS do
      for i:=0 to BatteryCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicSMBIOSBattery));
        AddChildObject(n3,Format('%s',[Battery[i].Location]),pi);
      end;
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,rsProcessors,pi);
    with SIC.Machine.SMBIOS do
      for i:=0 to ProcessorCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicSMBIOSProcessor));
        AddChildObject(n3,Format('%s',[Processor[i].Manufacturer]),pi);
      end;
    new(pi);
    pi^:=Join(0,Word(msicSMBIOSMemCtrl));
    AddChildObject(n2,Format('%s %s',[rsMemory,rsController]),pi);
    new(pi);
    pi^:=Join(0,Word(msicSMBIOSPMA));
    AddChildObject(n2,rsPMA,pi);
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,Format('%s %s',[rsMemory,rsModules]),pi);
    with SIC.Machine.SMBIOS do
      for i:=0 to MemoryModuleCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicSMBIOSMemoryModule));
        AddChildObject(n3,Format('%s',[MemoryModule[i].Socket]),pi);
      end;
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,Format('%s %s',[rsMemory,rsDevices]),pi);
    with SIC.Machine.SMBIOS do
      for i:=0 to MemoryDeviceCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicSMBIOSMemoryDevice));
        AddChildObject(n3,Format('%s',[Alter(Alter(MemoryDevice[i].DeviceLocator,MemoryDevice[i].BankLocator),MemoryDeviceTypes[MemoryDevice[i].Device])]),pi);
      end;
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,rsCache,pi);
    with SIC.Machine.SMBIOS do
      for i:=0 to CacheCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicSMBIOSCache));
        AddChildObject(n3,Format('%s',[Cache[i].Designation]),pi);
      end;
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,rsPortConnectors,pi);
    with SIC.Machine.SMBIOS do
      for i:=0 to PortCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicSMBIOSPort));
        try s:=PortTypes[Port[i].Typ] except s:='Unknown' end;
        AddChildObject(n3,Format('%s',[s]),pi);
      end;
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,Format('%s %s',[rsSystem,rsSlots]),pi);
    with SIC.Machine.SMBIOS do
      for i:=0 to SystemSlotCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicSMBIOSSystemSlot));
        AddChildObject(n3,Format('%s',[SlotTypes[SystemSlot[i].Typ]]),pi);
      end;
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,Format('%s %s',[rsOnBoard,rsDevices]),pi);
    with SIC.Machine.SMBIOS do
      for i:=0 to OnBoardDeviceCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicSMBIOSOnBoardDevice));
        AddChildObject(n3,Format('%s',[OnBoardDevice[i].DeviceName]),pi);
      end;
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,Format('%s %s %s',[rsOnBoard,rsDevices,rsExtended]),pi);
    with SIC.Machine.SMBIOS do
      for i:=0 to OnBoardDeviceExCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicSMBIOSOnBoardDeviceEx));
        AddChildObject(n3,Format('%s',[OnBoardDeviceEx[i].DeviceName]),pi);
      end;
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,Format('%s %s',[rsVoltage,rsProbes]),pi);
    with SIC.Machine.SMBIOS do
      for i:=0 to VoltageProbeCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicSMBIOSVoltageProbe));
        AddChildObject(n3,Format('%s',[VoltageProbe[i].Description]),pi);
      end;
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,Format('%s %s',[rsTemperature,rsProbes]),pi);
    with SIC.Machine.SMBIOS do
      for i:=0 to TemperatureProbeCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicSMBIOSTemperatureProbe));
        AddChildObject(n3,Format('%s',[TemperatureProbe[i].Description]),pi);
      end;
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,Format('%s %s',[rsCurrent,rsProbes]),pi);
    with SIC.Machine.SMBIOS do
      for i:=0 to CurrentProbeCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicSMBIOSCurrentProbe));
        AddChildObject(n3,Format('%s',[CurrentProbe[i].Description]),pi);
      end;
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,Format('%s %s',[rsCooling,rsDevices]),pi);
    with SIC.Machine.SMBIOS do
      for i:=0 to CoolingDeviceCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicSMBIOSCoolingDevice));
        AddChildObject(n3,Format('%s',[CoolingDevice[i].Description]),pi);
      end;
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,Format('%s %s',[rsTPM,rsDevices]),pi);
    with SIC.Machine.SMBIOS do
      for i:=0 to TPMDeviceCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicSMBIOSTPMDevice));
        AddChildObject(n3,Format('%s',[TPMDevice[i].Description]),pi);
      end;
  end;
end;

procedure Tmdi_msi_Viewer.CreateCPUTree(ARoot: TTreeNode);
var
  n1,n2,n3: TTreeNode;
  pi: PInteger;
  i,c: Integer;
  s: string;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
  with Tree, Items do begin
    new(pi);
    pi^:=Join(0,Word(msicCPU));
    n1:=AddChildObject(ARoot,rsCentralProcessor,pi);
    with SIC.CPU do begin
      c:=CPUPhysicalCount;
      for i:=0 to c-1 do begin
        CPUIndex:=i;
        if LiveData then
          RefreshData
        else
          LoadFromSIF(SIC.CPU);
        if not ((sl.IndexOf(IntToStr(PhysicalID))=-1) or ((i=c-1) and (n1.Count<CPUPhysicalCount))) then
          Continue;
        sl.Add(IntToStr(PhysicalID));
        new(pi);
        pi^:=Join(i+1,Word(msicCPU));
        s:=Format('%s %s - %d MHz',[cVendorNames[Vendor].Prefix,CPUName,Frequency]);
        n2:=AddChildObject(n1,s,pi);
          new(pi);
          pi^:=Join(i+1,Word(msicCPUCache));
          AddChildObject(n2,rsCache,pi);
          new(pi);
          pi^:=Join(i+1,Word(msicCPUFS));
          n3:=AddChildObject(n2,rsFeatures,pi);
            if Features.Standard1.Count>0 then begin
              new(pi);
              pi^:=Join(i+1,Word(msicCPUFSStd1));
              AddChildObject(n3,rsStandard+'-1',pi);
            end;
            if Features.Standard2.Count>0 then begin
              new(pi);
              pi^:=Join(i+1,Word(msicCPUFSStd2));
              AddChildObject(n3,rsStandard+'-2',pi);
            end;
            if Features.Extended1.Count>0 then begin
              new(pi);
              pi^:=Join(i+1,Word(msicCPUFSExt1));
              AddChildObject(n3,rsExtended+'-1',pi);
            end;
            if Features.Extended2.Count>0 then begin
              new(pi);
              pi^:=Join(i+1,Word(msicCPUFSExt2));
              AddChildObject(n3,rsExtended+'-2',pi);
            end;
            if Features.PowerManagement.Count>0 then begin
              new(pi);
              pi^:=Join(i+1,Word(msicCPUFSAPM));
              AddChildObject(n3,rsAPM,pi);
            end;
      end;
    end;
  end;

  finally
    sl.Free;
  end;
end;

procedure Tmdi_msi_Viewer.CreateMemoryTree(ARoot: TTreeNode);
var
  n1: TTreeNode;
  pi: PInteger;
  t,i,c: Integer;
  s: string;
begin
  with Tree, Items do begin
    new(pi);
    pi^:=Join(0,Word(msicMemory));
    n1:=AddChildObject(ARoot,rsMemory,pi);
    c:=0;
    t:=(SIC.Memory.PhysicalTotal div 1024) div 1024;
    with SIC.Machine.SMBIOS do begin
      if MemoryDeviceCount>0 then begin
        for i:=0 to MemoryDeviceCount-1 do
          if (MemoryDevice[i].Size>0) and (c<t) then begin
            c:=c+MemoryDevice[i].Size;
            new(pi);
            pi^:=Join(i+1,Word(msicSMBIOSMemoryDevice));
            if MemoryDevice[i].Device>smmdUnknown then
              s:=MemoryDeviceTypes[MemoryDevice[i].Device]
            else
              s:=MemoryFormFactors[MemoryDevice[i].FormFactor];
            AddChildObject(n1,Format('Row :%d - %d MB %s',[i,MemoryDevice[i].Size,s]),pi);
          end;
      end else
        for i:=0 to MemoryModuleCount-1 do
          if (MemoryModule[i].Size>0) and (c<t) then begin
            c:=c+MemoryModule[i].Size;
            new(pi);
            pi^:=Join(i+1,Word(msicSMBIOSMemoryModule));
            AddChildObject(n1,Format('Row :%d - %d MB %s',[i,
                                                           MemoryModule[i].Size,
                                                           GetMemoryTypeStr(MemoryModule[i].Types)]),pi);
          end;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.CreateAudioTree(ARoot: TTreeNode);
var
  n1,n2: TTreeNode;
  pi: PInteger;
  i: Integer;
begin
  if (SIC.Media.Devices.Count=0) and (SIC.Media.WAVEOut.Count=0) then
    Exit;
  with Tree, Items do begin
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n1:=AddChildObject(ARoot,rsAudio,pi);
    with SIC.Devices do
      for i:=0 to DeviceCount-1 do
        if SameText(Devices[i].ClassName,'Media') and (Devices[i].SymbolicLink<>'') then begin
          new(pi);
          pi^:=Join(i+1,Word(msicAudio));
          n2:=AddChildObject(n1,Devices[i].Name,pi);
        end;

    if (n1.Count=0) and (SIC.Media.SoundCardName<>'') then
      with SIC.Media do begin
        new(pi);
        pi^:=Join(0,Word(msicAudio));
        n2:=AddChildObject(n1,SoundCardName,pi);
      end;

    if n1.Count=0 then
      with SIC.Media do
        if (WAVEOut.Count>0) then begin
          new(pi);
          pi^:=Join(0,Word(msicAudio));
          n2:=AddChildObject(n1,WaveOut[0],pi);
        end;

    if n1.Count=0 then
      n1.Delete;
  end;
end;

procedure Tmdi_msi_Viewer.CreateStorageTree(ARoot: TTreeNode);
var
  n1,n2,n3: TTreeNode;
  pi: PInteger;
  i,j: Integer;
  d,s: string;
begin
  d:='';
  s:=SIC.Disk.AvailableDisks;

  with Tree, Items do begin
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n1:=AddChildObject(ARoot,Format('%s',[rsStorage]),pi);

    new(pi);
    pi^:=Join(0,Word(msicOther));
    n2:=AddChildObject(n1,Format('%s %s',[rsWindows,rsStorage]),pi);
    with SIC.Devices do begin
      for i:=0 to DeviceCount-1 do
        if SameText(Devices[i].ClassName,'DiskDrive') or
           SameText(Devices[i].ClassName,'CDROM') or
           SameText(Devices[i].ClassName,'Tape') or
           SameText(Devices[i].ClassName,'FloppyDisk') then begin
          new(pi);
          pi^:=Join(i+1,Word(msicWinStorage));
          AddChildObject(n2,Devices[i].Name,pi);
        end;
    end;
    n2.AlphaSort;

    new(pi);
    pi^:=Join(0,Word(msicOther));
    n2:=AddChildObject(n1,Format('%s %s',[rsPhysical,rsDevices]),pi);
    with SIC, Storage do begin
      for i:=0 to PhysicalCount-1 do
        with Physical[i] do begin
          new(pi);
          pi^:=Join(i+1,Word(msicPhysDrive));
          if Size>0 then
            n3:=AddChildObject(n2,Trim(Format('%s (%d MB)',[Model,Size shr 20])),pi)
          else
            n3:=AddChildObject(n2,Format('%s',[Model]),pi);
          for j:=0 to LogicalCount-1 do
            with Logical[j] do
              if PhysicalIndex=i then begin
                new(pi);
                d:=d+Copy(Drive,1,1);
                pi^:=Join(j+1,Word(msicLogDrive));
                if not(DeviceType in [FILE_DEVICE_CD_ROM,FILE_DEVICE_DVD, FILE_DEVICE_TAPE,FILE_DEVICE_UNKNOWN]) and (Length(Layout)>0) and (LayoutIndex>-1) then
                  AddChildObject(n3,Format('%s: (%s %s - %d MB)',[
                                   Drive,
                                   GetPartitionType(Layout[LayoutIndex].Number,Layout[LayoutIndex].Typ),
                                   FileSystem,//GetPartitionSystem(Layout[LayoutIndex].Typ),
                                   Layout[LayoutIndex].Length.QuadPart shr 20]),pi)
                else begin
                  Disk.Drive:=Drive+':';
                  if Disk.Capacity=0 then
                    AddChildObject(n3,Format('%s:',[Drive]),pi)
                  else
                    AddChildObject(n3,Format('%s: (%s - %d MB)',[Drive,Disk.FileSystem,Disk.Capacity shr 20]),pi)
                end;
              end;
        end;
    end;
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,'Network drives',pi);
    with SIC.Disk do
      for i:=1 to Length(s) do begin
        Drive:=Format('%s:\',[Copy(s,i,1)]);
        if MediaType=dtRemote then begin
           d:=d+Copy(Drive,1,1);
           new(pi);
           pi^:=Join(i,Word(msicNetDrive));
           AddChildObject(n3,Format('%s (%s)',[Drive,UNCPath]),pi);
        end;
      end;
    if n3.Count=0 then
      Delete(n3);

    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,'Removable drives',pi);
    with SIC.Disk do
      for i:=1 to Length(s) do begin
        Drive:=Format('%s:\',[Copy(s,i,1)]);
        if (Pos(Copy(s,i,1),d)=0) and (MediaType=dtRemovable) then begin
          d:=d+Copy(Drive,1,1);
          new(pi);
          pi^:=Join(i,Word(msicRemovableDrive));
          AddChildObject(n3,Format('%s',[Drive]),pi);
        end;
      end;
    if n3.Count=0 then
      Delete(n3);

    new(pi);
    pi^:=Join(0,Word(msicOther));
    n3:=AddChildObject(n2,'Other drives',pi);
    with SIC.Disk do
      for i:=1 to Length(s) do begin
        if Pos(Copy(s,i,1),d)=0 then begin
          Drive:=Format('%s:\',[Copy(s,i,1)]);
          new(pi);
          pi^:=Join(i,Word(msicOtherDrive));
          AddChildObject(n3,Format('%s',[Drive]),pi);
        end;
      end;
    if n3.Count=0 then
      Delete(n3);

    new(pi);
    pi^:=Join(0,Word(msicOther));
    n2:=AddChildObject(n1,Format('%s',[rsASPI]),pi);
    with SIC.Engines.ASPI32 do
      for i:=0 to Configuration.LUN.Count-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicASPI));
        AddChildObject(n2,Format('%s%s',[Configuration.Vendor[i],Configuration.Model[i]]),pi);
      end;
  end;
end;

procedure Tmdi_msi_Viewer.CreateMonitorTree(ARoot: TTreeNode);
var
  n1: TTreeNode;
  pi: PInteger;
  i: Integer;
begin
  with Tree, Items do begin
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n1:=AddChildObject(ARoot,rsMonitor,pi);
    with SIC.Monitor do
      for i:=0 to Count-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicMonitor));
        if Monitors[i].EDID.Name<>'' then
          AddChildObject(n1,Monitors[i].EDID.Name,pi)
        else
          AddChildObject(n1,Monitors[i].DeviceDescription,pi);
      end;
    if n1.Count=0 then
      n1.Delete;
  end;
end;

procedure Tmdi_msi_Viewer.CreateNetworkTree(ARoot: TTreeNode);
var
  n1: TTreeNode;
  pi: PInteger;
  i: Integer;
begin
  with Tree, Items do begin
    new(pi);
    pi^:=Join(0,Word(msicNetwork));
    n1:=AddChildObject(ARoot,rsNetwork,pi);
    if SIC.Network.TCPIP.AdapterCount>0 then
      with SIC.Network.TCPIP do
        for i:=0 to AdapterCount-1 do begin
          new(pi);
          pi^:=Join(i+1,Word(msicNetwork));
          AddChildObject(n1,Adapter[i].Name,pi);
        end
    else
      with SIC.Network do begin
        for i:=0 to PhysicalAdapters.Count-1 do begin
          new(pi);
          pi^:=Join(i+1,Word(msicNetwork));
          AddChildObject(n1,PhysicalAdapters[i],pi);
        end;
        for i:=0 to VirtualAdapters.Count-1 do begin
          new(pi);
          pi^:=Join(PhysicalAdapters.Count+i+1,Word(msicNetwork));
          AddChildObject(n1,VirtualAdapters[i],pi);
        end;
      end;
    if n1.Count=0 then
      n1.Delete;
  end;
end;

procedure Tmdi_msi_Viewer.CreatePortsTree(ARoot: TTreeNode);
var
  n1,n2: TTreeNode;
  pi: PInteger;
  i: Integer;
begin
  with Tree, Items do begin
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n1:=AddChildObject(ARoot,rsPorts,pi);
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n2:=AddChildObject(n1,rsCOM,pi);
    with SIC.Devices do
      for i:=0 to DeviceCount-1 do
        if SameText(Devices[i].ClassName,'Ports') and (Pos('COM',Devices[i].Name)>0) then begin
          new(pi);
          pi^:=Join(i+1,Word(msicDevice));
          AddChildObject(n2,Devices[i].Name,pi);
        end;
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n2:=AddChildObject(n1,rsLPT,pi);
    with SIC.Devices do
      for i:=0 to DeviceCount-1 do
        if SameText(Devices[i].ClassName,'Ports') and (Pos('LPT',Devices[i].Name)>0) then begin
          new(pi);
          pi^:=Join(i+1,Word(msicDevice));
          AddChildObject(n2,Devices[i].Name,pi);
        end;
  end;
end;

procedure Tmdi_msi_Viewer.CreateVideoTree(ARoot: TTreeNode);
var
  n1: TTreeNode;
  pi: PInteger;
  i: Integer;
begin
  with Tree, Items do begin
    new(pi);
    pi^:=Join(0,Word(msicVideo));
    n1:=AddChildObject(ARoot,rsVideo,pi);
    with SIC.Display do
      for i:=0 to AdapterCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicVideo));
        AddChildObject(n1,Adapter[i].Name,pi);
      end;
  end;
end;

procedure Tmdi_msi_Viewer.TreeChange(Sender: TObject; Node: TTreeNode);
var
  s: string;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
    finally
      EndUpdate;
    end;
  end;

  if Assigned(Node) then begin
    s:=GetTreePath(Node);
    if Node.Count>0 then
      sb.SimpleText:=Format('%s: %d node(s)',[s,Node.Count])
    else
      sb.SimpleText:=s;
    sb.Hint:=sb.SimpleText;
    if Assigned(Node.Data) then
      case TCategory(HiWord(PInteger(Node.Data)^)) of
        msicMachine: DisplayMachineInfo;
        msicCPU: case LoWord(PInteger(Node.Data)^) of
          0: DisplayCPUInfo;
          1..31: DisplayCPUDetail(LoWord(PInteger(Node.Data)^)-1);
        end;
        msicCPUCache: DisplayCPUCache(LoWord(PInteger(Node.Data)^)-1);
        msicCPUFS: DisplayCPUFS(LoWord(PInteger(Node.Data)^)-1);
        msicCPUFSStd1: DisplayCPUFSStd1(LoWord(PInteger(Node.Data)^)-1);
        msicCPUFSStd2: DisplayCPUFSStd2(LoWord(PInteger(Node.Data)^)-1);
        msicCPUFSExt1: DisplayCPUFSExt1(LoWord(PInteger(Node.Data)^)-1);
        msicCPUFSExt2: DisplayCPUFSExt2(LoWord(PInteger(Node.Data)^)-1);
        msicCPUFSAPM: DisplayCPUFSAPM(LoWord(PInteger(Node.Data)^)-1);
        msicMemory: if LoWord(PInteger(Node.Data)^)=0 then
                      DisplayMemoryInfo;
        msicMotherBoard: if LoWord(PInteger(Node.Data)^)=0 then
                           DisplayMotherBoardInfo;
        msicDevice: if LoWord(PInteger(Node.Data)^)=0 then
                     DisplayDeviceInfo
                   else
                     DisplayDeviceDetail(LoWord(PInteger(Node.Data)^)-1);
        msicVideo: if LoWord(PInteger(Node.Data)^)>0 then
                     DisplayVideoDetail(LoWord(PInteger(Node.Data)^)-1)
                   else
                     DisplayVideoProps;
        msicMonitor: if LoWord(PInteger(Node.Data)^)>0 then
                       DisplayMonitorDetail(LoWord(PInteger(Node.Data)^)-1);
        msicAudio: //if LoWord(PInteger(Node.Data)^)>0 then
                    DisplayAudioDetail;
        msicNetwork: if LoWord(PInteger(Node.Data)^)>0 then
                       DisplayNetworkDetail(LoWord(PInteger(Node.Data)^)-1)
                     else
                       DisplayNetworkInfo;
        msicWinStorage: if LoWord(PInteger(Node.Data)^)>0 then
                    DisplayWinStorageDetail(LoWord(PInteger(Node.Data)^)-1);
        msicPhysDrive: if LoWord(PInteger(Node.Data)^)>0 then
                    DisplayPhysDriveDetail(LoWord(PInteger(Node.Data)^)-1);
        msicLogDrive: if LoWord(PInteger(Node.Data)^)>0 then
                    DisplayLogDriveDetail(LoWord(PInteger(Node.Data)^)-1);
        msicNetDrive,
        msicRemovableDrive,
        msicOtherDrive: if LoWord(PInteger(Node.Data)^)>0 then
                    DisplayOtherDriveDetail(LoWord(PInteger(Node.Data)^));
        msicASPI:if LoWord(PInteger(Node.Data)^)>0 then
                    DisplayASPIDetail(LoWord(PInteger(Node.Data)^)-1);
        msicUSB: if LoWord(PInteger(Node.Data)^)>0 then
                   DisplayUSBDetail(LoWord(PInteger(Node.Data)^)-1)
                 else
                   DisplayUSBSummary;
        msicPrinter: if LoWord(PInteger(Node.Data)^)>0 then
                    DisplayPrinterDetail(LoWord(PInteger(Node.Data)^)-1);
        msicSMBIOS: if LoWord(PInteger(Node.Data)^)=0 then
                      DisplaySMBIOSInfo;
        msicSMBIOSProcessor: if LoWord(PInteger(Node.Data)^)>0 then
                               DisplaySMBIOSProcessorDetail(LoWord(PInteger(Node.Data)^)-1);
        msicSMBIOSMemoryModule: if LoWord(PInteger(Node.Data)^)>0 then
                      DisplaySMBIOSMemoryModuleDetail(LoWord(PInteger(Node.Data)^)-1);
        msicSMBIOSMemorydevice: if LoWord(PInteger(Node.Data)^)>0 then
                      DisplaySMBIOSMemoryDeviceDetail(LoWord(PInteger(Node.Data)^)-1);
        msicSMBIOSMemCtrl: if LoWord(PInteger(Node.Data)^)=0 then
                             DisplaySMBIOSMemCtrlInfo;
        msicSMBIOSPMA: if LoWord(PInteger(Node.Data)^)=0 then
                             DisplaySMBIOSPMAInfo;
        msicSMBIOSBIOS: if LoWord(PInteger(Node.Data)^)=0 then
                          DisplaySMBIOSBIOSInfo;
        msicSMBIOSSystem: if LoWord(PInteger(Node.Data)^)=0 then
                            DisplaySMBIOSSystemInfo;
        msicSMBIOSChassis: if LoWord(PInteger(Node.Data)^)=0 then
                            DisplaySMBIOSChassisInfo;
        msicSMBIOSMainBoard: if LoWord(PInteger(Node.Data)^)=0 then
                               DisplaySMBIOSMainboardInfo;
        msicSMBIOSSystemPowerSupply: if LoWord(PInteger(Node.Data)^)=0 then
                               DisplaySMBIOSSystemPowerSupply;
        msicSMBIOSCache: if LoWord(PInteger(Node.Data)^)>0 then
                           DisplaySMBIOSCacheDetail(LoWord(PInteger(Node.Data)^)-1);
        msicSMBIOSSystemSlot: if LoWord(PInteger(Node.Data)^)>0 then
                                DisplaySMBIOSSystemSlotDetail(LoWord(PInteger(Node.Data)^)-1);
        msicSMBIOSPort: if LoWord(PInteger(Node.Data)^)>0 then
                          DisplaySMBIOSPortDetail(LoWord(PInteger(Node.Data)^)-1);
        msicSMBIOSOnBoardDevice: if LoWord(PInteger(Node.Data)^)>0 then
                          DisplaySMBIOSOnBoardDeviceDetail(LoWord(PInteger(Node.Data)^)-1);
        msicSMBIOSOnBoardDeviceEx: if LoWord(PInteger(Node.Data)^)>0 then
                          DisplaySMBIOSOnBoardDeviceExDetail(LoWord(PInteger(Node.Data)^)-1);
        msicSMBIOSTemperatureProbe: if LoWord(PInteger(Node.Data)^)>0 then
                          DisplaySMBIOSTempProbeDetail(LoWord(PInteger(Node.Data)^)-1);
        msicSMBIOSVoltageProbe: if LoWord(PInteger(Node.Data)^)>0 then
                          DisplaySMBIOSVoltProbeDetail(LoWord(PInteger(Node.Data)^)-1);
        msicSMBIOSCurrentProbe: if LoWord(PInteger(Node.Data)^)>0 then
                          DisplaySMBIOSCurrProbeDetail(LoWord(PInteger(Node.Data)^)-1);
        msicSMBIOSCoolingDevice: if LoWord(PInteger(Node.Data)^)>0 then
                          DisplaySMBIOSCoolDevDetail(LoWord(PInteger(Node.Data)^)-1);
        msicSMBIOSTPMDevice: if LoWord(PInteger(Node.Data)^)>0 then
                          DisplaySMBIOSTPMDevDetail(LoWord(PInteger(Node.Data)^)-1);
        msicSMBIOSBattery: if LoWord(PInteger(Node.Data)^)>0 then
                          DisplaySMBIOSBatteryDetail(LoWord(PInteger(Node.Data)^)-1);
        else
          DisplayNodeInfo;
      end;
  end;
end;

function Tmdi_msi_Viewer.GetTreePath(ANode: TTreeNode): string;
begin
  Result:=ANode.Text;
  ANode:=ANode.Parent;
  while Assigned(ANode) do begin
    Result:=ANode.Text+' -> '+Result;
    ANode:=ANode.Parent;
  end;
end;

procedure Tmdi_msi_Viewer.CreateBTTree(ARoot: TTreeNode);
begin

end;

procedure Tmdi_msi_Viewer.CreateBusTree(ARoot: TTreeNode);
var
  n1,n2,n3: TTreeNode;
  pi: PInteger;
  i,j,k,lastpci,lastdev: Integer;
  sl: TStringList;
  s: string;
begin
  with Tree, Items do begin
    new(pi);
    pi^:=Join(0,Word(msicBus));
    n1:=AddChildObject(ARoot,rsBus,pi);
    n2:=nil;

    sl:=TStringList.Create;
    with SIC.Devices do
      for i:=0 to DeviceCount-1 do
        if (Pos('PCI\',Devices[i].HardwareID)=1) then begin
          if (sl.IndexOf(IntToStr(Devices[i].PCINumber))=-1) {and (Devices[i].DeviceNumber=0)} then begin
            sl.Add(IntToStr(Devices[i].PCINumber));
            lastpci:=Devices[i].PCINumber;
            new(pi);
            pi^:=Join(0,Word(msicDevice));
            n2:=AddChildObject(n1,Format('%s %s#%d',[rsPCI,rsBus,Devices[i].PCINumber]),pi);
            for j:=0 to DeviceCount-1 do
              if (Pos('PCI\',Devices[j].HardwareID)=1) then begin
                if (Devices[j].PCINumber=lastpci) and (Devices[j].FunctionNumber=0) then begin
                  lastdev:=Devices[j].DeviceNumber;
                  new(pi);
                  pi^:=Join(j+1,Word(msicDevice));
                  n3:=AddChildObject(n2,Devices[j].Name,pi);
                  for k:=0 to DeviceCount-1 do
                    if (Pos('PCI\',Devices[k].HardwareID)=1) then begin
                      if (Devices[k].PCINumber=lastpci) and (Devices[k].DeviceNumber=lastdev) and (Devices[k].FunctionNumber>0) then begin
                        new(pi);
                        pi^:=Join(k+1,Word(msicDevice));
                        s:=Devices[k].Name;
                        AddChildObject(n3,s,pi);
                     end;
                   end;
                end;
              end;
          end;
        end;
      sl.Free;
    if n1.Count=0 then
      n1.Delete;
  end;
end;

procedure Tmdi_msi_Viewer.cmExit(Sender: TObject);
begin
  Close;
end;

procedure Tmdi_msi_Viewer.DisplayMachineInfo;
var
  s: string;
  i,c,idx: Integer;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine do begin
        with Add do begin
          Caption:=Format('%s %s',[rsMachine,rsName]);
          SubItems.Add(SIC.Machine.MachineName);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsSession;

          if ((stTerminal in Session) or (stCitrix in Session)) and (SessionProtocol<>spNone) then
            SubItems.Add(Format('%s - %s',[GetSessionStr(Session),cSessionProto[SessionProtocol]]))
          else
            SubItems.Add(GetSessionStr(Session));
          ImageIndex:=-1;
        end;
        with Add do begin
          if IsInDomain then
            Caption:=rsDomain
          else
            Caption:=rsWkg;
          SubItems.Add(JoinedTo);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsUsername;
          if LiveData or (User<>'') then begin
            if SIC.OS.LiveID<>'' then
              s:=SIC.OS.LiveID
            else
              s:=User;
          end;
          if AdminRights then
            s:=s+rsAsAdmin;
          SubItems.Add(s);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsLastBoot;
          if LiveData or (LastBoot<>0) then
            SubItems.Add(DateTimeToStr(LastBoot))
          else
            SubItems.Add('');
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSystemUpTime;
          if LiveData or (SystemUpTime<>0) then
            SubItems.Add(FormatSeconds(SystemUpTime))
          else
            SubItems.Add('');
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:='';
          ImageIndex:=-2;
        end;

        if BIOS.BIOSDataCount>0 then begin
          s:='SystemManufacturer';
          with Add do begin
            Caption:=SplitByCaps(s);
            SubItems.Add(BIOS.BIOSValue[s].Value);
            ImageIndex:=-3;
          end;
          s:='SystemFamily';
          with Add do begin
            Caption:=SplitByCaps(s);
            SubItems.Add(BIOS.BIOSValue[s].Value);
            ImageIndex:=-1;
          end;
          s:='SystemProductName';
          with Add do begin
            Caption:=SplitByCaps(s);
            SubItems.Add(BIOS.BIOSValue[s].Value);
            ImageIndex:=-1;
          end;
        end else begin
          with Add do begin
            Caption:=rsManufacturer;
            SubItems.Add(SMBIOS.MainBoardManufacturer);
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsMainboard;
            SubItems.Add(SMBIOS.MainboardModel);
            ImageIndex:=-3;
          end;
        end;
      end;

      with Add do begin
        Caption:='';
        ImageIndex:=-2;
      end;

      with Add do begin
        Caption:=rsCPU;
        SubItems.Add(Format('%d x %s - %d MHz',[SIC.CPU.CPUPhysicalCount,SIC.CPU.CPUName,SIC.CPU.Frequency]));
        ImageIndex:=-3;
      end;
      with Add do begin
        Caption:='RAM';
        if SIC.Machine.SMBIOS.MemoryDeviceCount>0 then begin
          c:=0;
          idx:=-1;
          for i:=0 to SIC.Machine.SMBIOS.MemoryDeviceCount-1 do
            if SIC.Machine.SMBIOS.MemoryDevice[i].Size>0 then begin
              Inc(c,SIC.Machine.SMBIOS.MemoryDevice[i].Size);
              if idx=-1 then
                idx:=i;
            end;
          SubItems.Add(Format('%d MB %s',[c,MemoryDeviceTypes[SIC.Machine.SMBIOS.MemoryDevice[idx].Device]]))
        end else
          SubItems.Add(Format('%d MB',[SIC.Memory.PhysicalTotal shr 20]));
        ImageIndex:=-1;
      end;
      for i:=0 to SIC.Display.AdapterCount-1 do
        with Add do begin
          Caption:='Graphics';
          if SIC.Display.Adapter[i].Memory>0 then
            SubItems.Add(Format('%s - %d MB',[SIC.Display.Adapter[i].Name,SIC.Display.Adapter[i].Memory shr 20]))
          else
            SubItems.Add(SIC.Display.Adapter[i].Name);
          ImageIndex:=-1;
        end;
      for i:=0 to SIC.Storage.PhysicalCount-1 do
        if (SIC.Storage.Physical[i].DeviceType=FILE_DEVICE_DISK) and (SIC.Storage.Physical[i].MediaType=FixedMedia) then
          with Add do begin
            Caption:='HDD';
            SubItems.Add(Format('%s - %d GB',[SIC.Storage.Physical[i].Model,SIC.Storage.Physical[i].Size shr 30]));
            ImageIndex:=-1;
          end;

      for i:=0 to SIC.Storage.PhysicalCount-1 do
        if (SIC.Storage.Physical[i].BusType in [BusTypeScsi..BusTypeSata]) and (SIC.Storage.Physical[i].DeviceType in [FILE_DEVICE_CD_ROM,FILE_DEVICE_DVD,FILE_DEVICE_TAPE]) then
          with Add do begin
            case SIC.Storage.Physical[i].DeviceType of
              FILE_DEVICE_CD_ROM: s:='CD-ROM';
              FILE_DEVICE_DVD: s:='DVD';
              FILE_DEVICE_TAPE: s:='Tape';
            end;
            Caption:=s;
            SubItems.Add(SIC.Storage.Physical[i].Model);
            ImageIndex:=-1;
          end;

      i:=SIC.Network.TCPIP.FindAdapter(SIC.Network.TCPIP.BestInterfaceIdx);
      if i>-1 then begin
        idx:=SIC.Network.TCPIP.FindAddress(SIC.Network.TCPIP.BestInterfaceIdx);
        with Add do begin
          Caption:='Network Adapter';
          s:=SIC.Network.TCPIP.Adapter[i].Name;
          if idx>-1 then
            s:=s+' - '+SIC.Network.TCPIP.AddressRecord[idx].IP;
          SubItems.Add(s);
          ImageIndex:=-1;
        end;
      end;

      if (SIC.Media.SoundCardName<>'') then
        with Add do begin
          Caption:='Sound Card';
          SubItems.Add(SIC.Media.SoundCardName);
        end;

      with Add do begin
        Caption:='';
        ImageIndex:=-2;
      end;

      with SIC.OS do begin
        with Add do begin
          Caption:=rsOS;
          SubItems.Add(SIC.OS.OSName);
          with Add do begin
            Caption:=rsEdition;
            SubItems.Add(SIC.OS.OSEdition);
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsVersion;
            SubItems.Add(Format('%d.%d.%d',[MajorVersion,MinorVersion,BuildNumber]));
            ImageIndex:=-1;
          end;
          ImageIndex:=-3;
        end;
        if ReleaseId<>'' then
          with Add do begin
            Caption:=rsReleaseId;
            SubItems.Add(ReleaseID);
            ImageIndex:=-1;
          end;
        with Add do begin
          Caption:='OS Build';
          SubItems.Add(OSBuild);
          ImageIndex:=-1;
        end;
        s:='';
        with Add do begin
          Caption:=rsSP;
          s:=CSD;
          if (s='') and (ServicePackMajorVersion+ServicePackMinorVersion>0) then
            s:=Format('Service Pack %d.%d',[ServicePackMajorVersion,ServicePackMinorVersion]);
          SubItems.Add(s);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsInstallType;
          SubItems.Add(InstallationType);
          ImageIndex:=-1;
        end;
        if CompatibilityMode then
          with Add do begin
            Caption:=rsCompMode;
            SubItems.Add(TrueWindowsVersion);
            ImageIndex:=-1;
          end;
        {with Add do begin
          Caption:=rsProductID;
          SubItems.Add(ProductID);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsProductKey;
          if SameText(ProductKey,'BBBBB-BBBBB-BBBBB-BBBBB-BBBBB') then
            SubItems.Add('Multi Volume License')
          else
            SubItems.Add(ProductKey);
          ImageIndex:=-1;
        end;}
        with Add do begin
          Caption:=rsGenuine+' '+rsWindows;
          SubItems.Add(BooleanEn[GenuineWindows]);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsInstallDate;
          if Livedata or (InstallDate<>0) then
            SubItems.Add(DateTimeToStr(InstallDate))
          else
            SubItems.Add('');
          ImageIndex:=-1;
        end;
        {with Add do begin
          Caption:=rsRegUser;
          SubItems.Add(RegisteredUser);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsRegOrg;
          SubItems.Add(RegisteredOrg);
          ImageIndex:=-1;
        end;}
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.ListAdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  DefaultDraw:=True;
  with TListView(Sender) do begin
    Canvas.Font.Style:=[];
    Canvas.Font.Color:=Font.Color;
    Canvas.Font.Style:=Canvas.Font.Style+[fsUnderline];
    if not HotTrack or not(htUnderlineCold in HotTrackStyles) then
      Canvas.Font.Style:=Font.Style;
    if (cdsHot in State) and HotTrack then begin
      if htUnderlineHot in HotTrackStyles then
        Canvas.Font.Style:=Canvas.Font.Style+[fsUnderline];
      Canvas.Font.Color:=clHotLight;
    end;
    if Item.ImageIndex=-3 then
      Canvas.Font.Style:=[fsBold];
    if Item.ImageIndex=-2 then
      ListView_DrawLine(Sender,Item,State,DefaultDraw,clSilver);
  end;
  {$IFDEF THEMESUPPORT}
  if UseThemes then
    SetBkMode(Sender.Canvas.Handle, TRANSPARENT);
  {$ENDIF}
end;

procedure Tmdi_msi_Viewer.CreatePrintersTree(ARoot: TTreeNode);
var
  n1: TTreeNode;
  pi: PInteger;
  i: Integer;
begin
  with Tree, Items do begin
    new(pi);
    pi^:=Join(0,Word(msicOther));
    n1:=AddChildObject(ARoot,rsPrinters,pi);
    with SIC.Printers do
      for i:=0 to PrinterCount-1 do begin
        new(pi);
        pi^:=Join(i+1,Word(msicPrinter));
        AddChildObject(n1,PrinterName[i],pi);
      end;
    if n1.Count=0 then
      n1.Delete;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayCPUInfo;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.CPU do begin
        with Add do begin
          Caption:=rsCPUPhysCount;
          SubItems.Add(Format('%d',[CPUPhysicalCount]));
          ImageIndex:=-3;
        end;
        Add.ImageIndex:=-2;
        with Add do begin
          Caption:=rsCN;
          SubItems.Add(Format('%d',[CoreCount]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsTN;
          SubItems.Add(Format('%d',[ThreadCount]));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayCPUDetail;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.CPU do begin
        CPUIndex:=AIndex;
        if LiveData then
          RefreshData
        else
          LoadFromSIF(SIC.CPU);
        with Add do begin
          Caption:=Format('%s %s',[rsCPU,rsName]);
          SubItems.Add(CPUName);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsArchitecture;
          case Architecture of
            PROCESSOR_ARCHITECTURE_AMD64: SubItems.Add('x64 (AMD or Intel)');
            PROCESSOR_ARCHITECTURE_IA32_ON_WIN64: SubItems.Add('WOW64');
            PROCESSOR_ARCHITECTURE_IA64: SubItems.Add('Intel Itanium Processor Family (IPF)');
            PROCESSOR_ARCHITECTURE_INTEL: SubItems.Add('x86');
            else SubItems.Add(Format('%d',[Architecture]));
          end;
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsCPP;
          SubItems.Add(IntToStr(CorePerPackage));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsLPC;
          SubItems.Add(IntToStr(LogicalPerCore));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsVendor;
          SubItems.Add(cVendorNames[Vendor].Name);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsFreq;
          SubItems.Add(Format('%d MHz',[Frequency]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsGeneric,rsName]);
          SubItems.Add(GenericName);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsMarketing,rsName]);
          SubItems.Add(MarketingName);
          ImageIndex:=-1;
        end;
        if Codename<>'' then
          with Add do begin
            Caption:=rsCodeName;
            SubItems.Add(CodeName);
            ImageIndex:=-1;
          end;
        if Revision<>'' then
          with Add do begin
            Caption:=rsRevision;
            SubItems.Add(Revision);
            ImageIndex:=-1;
          end;
        if Technology<>'' then
          with Add do begin
            Caption:=rsTechnology;
            SubItems.Add(Technology);
            ImageIndex:=-1;
          end;
        if Features.Standard1.FeaturesByFlag[SFS_PSN].Available then
          with Add do begin
            Caption:=rsSerial;
            SubItems.Add(SerialNumber);
            ImageIndex:=-1;
          end;
        {if CPUCount>1 then begin
          with Add do begin
            Caption:=Format('%s',[rsAPIC]);
            SubItems.Add(Format('%d',[APICID]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=Format('%s',[rsPhysID]);
            SubItems.Add(Format('%d',[PhysicalID]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=Format('%s',[rsLogID]);
            SubItems.Add(Format('%d',[LogicalID]));
            ImageIndex:=-1;
          end;
        end;}
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.ListAdvancedCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  DefaultDraw:=True;

  with TListView(Sender) do begin
    Canvas.Font.Style:=[];
    if Item.ImageIndex=-4 then
      ListView_DrawCheckBox(Sender,Item,SubItem,State,DefaultDraw,'1')
    else
      Canvas.Brush.Color:=clWhite;
    Canvas.Font.Color:=Font.Color;
    Canvas.Font.Style:=Canvas.Font.Style+[fsUnderline];
    if not HotTrack or not(htUnderlineCold in HotTrackStyles) then
      Canvas.Font.Style:=Font.Style;
    if (cdsHot in State) and HotTrack then begin
      if htUnderlineHot in HotTrackStyles then
        Canvas.Font.Style:=Canvas.Font.Style+[fsUnderline];
      Canvas.Font.Color:=clHotLight;
    end;
    if Item.ImageIndex=-3 then
      Canvas.Font.Style:=[fsBold];
  end;
  {$IFDEF THEMESUPPORT}
  if UseThemes then
    SetBkMode(Sender.Canvas.Handle, TRANSPARENT);
  {$ENDIF}  
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSMemoryModuleDetail;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS do begin
        with Add do begin
          Caption:=Format('%s %s',[rsSocket,rsDesignation]);
          SubItems.Add(MemoryModule[AIndex].Socket);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsType;
          SubItems.Add(GetMemoryTypeStr(MemoryModule[AIndex].Types));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSize;
          SubItems.Add(Format('%d MB',[MemoryModule[AIndex].Size]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSpeed;
          SubItems.Add(Format('%d ns',[MemoryModule[AIndex].Speed]));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayMemoryInfo;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Memory do begin
        with Add do begin
          Caption:=rsAppAddrRange;
          SubItems.Add(Format('%8.8xh - %8.8xh',[MinAppAddress,MaxAppAddress]));
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsAllocationGranularity;
          SubItems.Add(FormatFloat('#,#0 B',AllocGranularity));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsPageSize;
          SubItems.Add(FormatFloat('#,#0 B',PageSize));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:='';
          ImageIndex:=-2;
        end;
        with Add do begin
          Caption:=rsPhysTotal;
          SubItems.Add(FormatFloat('#,#0 B',PhysicalTotal)+Format(' (%d MB)',[(PhysicalTotal div 1024) div 1024]));
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsPhysFree;
          SubItems.Add(FormatFloat('#,#0 B',PhysicalFree)+Format(' (%d MB)',[(PhysicalFree div 1024) div 1024]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsPageFileTotal;
          SubItems.Add(FormatFloat('#,#0 B',PageFileTotal)+Format(' (%d MB)',[(PageFileTotal div 1024) div 1024]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsPageFileFree;
          SubItems.Add(FormatFloat('#,#0 B',PageFileFree)+Format(' (%d MB)',[(PageFileFree div 1024) div 1024]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsVirtTotal;
          SubItems.Add(FormatFloat('#,#0 B',VirtualTotal)+Format(' (%d MB)',[(VirtualTotal div 1024) div 1024]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsVirtFree;
          SubItems.Add(FormatFloat('#,#0 B',VirtualFree)+Format(' (%d MB)',[(VirtualFree div 1024) div 1024]));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayMotherboardInfo;
var
  s,v: string;
  k: TSMBIOS_SlotType;
  i,c: Integer;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS do begin
        with Add do begin
          Caption:=Format('%s %s',[rsSystem,rsModel]);
          SubItems.Add(SystemModel);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsSystem,rsManufacturer]);
          SubItems.Add(SystemManufacturer);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:='';
          ImageIndex:=-2;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsMotherboard,rsModel]);
          SubItems.Add(MainboardModel);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsMotherboard,rsManufacturer]);
          SubItems.Add(MainboardManufacturer);
          ImageIndex:=-1;
        end;
        with Add do begin
          v:='';
          for k:=Low(TSMBIOS_SlotType) to High(TSMBIOS_SlotType) do begin
            c:=0;
            for i:=0 to SystemSlotCount-1 do
              if SystemSlot[i].Typ=k then begin
                Inc(c);
                s:=SlotTypes[SystemSlot[i].Typ];
             end;
           if c>0 then
             v:=v+Format('%dx%s, ',[c,s]);
          end;
          SetLength(v,Length(v)-2);
          Caption:=Format('%s %s',[rsSystem,rsSlots]);
          SubItems.Add(v);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:='';
          ImageIndex:=-2;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsBIOS,rsVendor]);
          SubItems.Add(BIOSVendor);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsBIOS,rsVersion]);
          SubItems.Add(BIOSVersion);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsBIOS,rsDate]);
          SubItems.Add(BIOSDate);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsBIOS,rsSize]);
          SubItems.Add(Format('%d KB',[BIOSSize]));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayDeviceDetail(AIndex: Integer);
var
  DR: TDeviceResources;
  i: Integer;
  v,d: string;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Devices do begin
        with Add do begin
          Caption:=Format('%s %s',[rsDevice,rsName]);
          SubItems.Add(Devices[AIndex].Name);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsDriver,rsProvider]);
          SubItems.Add(Devices[AIndex].DriverProvider);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsDriver,rsVersion]);
          SubItems.Add(Devices[AIndex].DriverVersion);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsDriver,rsDate]);
          SubItems.Add(Devices[AIndex].DriverDate);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:='';
          ImageIndex:=-2;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsPCI,rsNumber]);
          SubItems.Add(Format('%d',[Devices[AIndex].PCINumber]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsDevice,rsNumber]);
          SubItems.Add(Format('%d',[Devices[AIndex].DeviceNumber]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsFunction,rsNumber]);
          SubItems.Add(Format('%d',[Devices[AIndex].FunctionNumber]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:='';
          ImageIndex:=-2;
        end;
        //ExpandHID(Devices[AIndex].VendorID,Devices[AIndex].DeviceID,Devices[AIndex].SubSysID,Devices[AIndex].Revision,v,d);
        if v<>'' then
          with Add do begin
            Caption:=rsVendor;
            SubItems.Add(v);
            ImageIndex:=-1;
          end;
        if d<>'' then
          with Add do begin
            Caption:=rsDevice;
            SubItems.Add(d);
            ImageIndex:=-3;
          end;
        with Add do begin
          Caption:=Format('%s %s',[rsVendor,rsID]);
          SubItems.Add(Format('%4.4x',[Devices[AIndex].VendorID]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsDevice,rsID]);
          SubItems.Add(Format('%4.4x',[Devices[AIndex].DeviceID]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s',[rsSubSystem]);
          SubItems.Add(Format('%8.8x',[Devices[AIndex].SubSysID]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s',[rsRevision]);
          SubItems.Add(Format('%2.2x',[Devices[AIndex].Revision]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:='';
          ImageIndex:=-2;
        end;
        if LiveData and (Devices[AIndex].ResourceListKey<>'') then begin
          GetDeviceResources(Devices[AIndex],DR);
          for i:=0 to High(DR.Resources) do
            case DR.Resources[i].Typ of
              CmResourceTypePort: with Add do begin
                Caption:=rsPortRange;
                SubItems.Add(Format('0x%8.8x - 0x%8.8x',[DR.Resources[i].Port.Start.QuadPart,
                                                         DR.Resources[i].Port.Start.QuadPart+DR.Resources[i].Port.Length-1]));
                ImageIndex:=-1;
              end;
              CmResourceTypeInterrupt: with Add do begin
                Caption:=rsIRQ;
                SubItems.Add(Format('IRQ%d',[DR.Resources[i].Interrupt.Vector]));
                ImageIndex:=-1;
              end;
              CmResourceTypeMemory: with Add do begin
                Caption:=rsMemoryRange;
                SubItems.Add(Format('0x%8.8x - 0x%8.8x',[DR.Resources[i].Memory.Start.QuadPart,
                                                         DR.Resources[i].Memory.Start.QuadPart-DR.Resources[i].Memory.Length]));
                ImageIndex:=-1;
              end;
              CmResourceTypeDma: with Add do begin
                Caption:=rsDMA;
                SubItems.Add(Format('%d',[DR.Resources[i].DMA.Channel]));
                ImageIndex:=-1;
              end;
          end;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayDeviceInfo;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with Add do begin
        Caption:=Format('%s %s',[rsDevice,rsCount]);
        SubItems.Add(Format('%d',[Tree.Selected.Count]));
        ImageIndex:=-3;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSInfo;
var
  i: Integer;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS do begin
        with Add do begin
          Caption:=rsVersion;
          SubItems.Add(Format('%d.%d Rev. %d.%d',[MajorVersion,MinorVersion,RevisionMajor,RevisionMinor]));
          ImageIndex:=-3;
        end;
        if Livedata then begin
          with Add do begin
            Caption:='';
            ImageIndex:=-2;
          end;
          with Add do begin
            Caption:=Format('%s %s',[rsTable,rsCount]);
            SubItems.Add(Format('%d',[Length(StructTables)]));
            ImageIndex:=-3;
          end;
          for i:=0 to High(StructTables) do
            with Add do begin
              Caption:=StructTables[i].Name;
              ImageIndex:=-1;
            end;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayVideoDetail;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Display do begin
        with Add do begin
          Caption:=Format('%s %s',[rsVideo,rsAdapter]);
          SubItems.Add(Format('%s',[Adapter[AIndex].Name]));
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsChipset;
          SubItems.Add(Format('%s',[Adapter[AIndex].Chipset]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsDAC;
          SubItems.Add(Format('%s',[Adapter[AIndex].DAC]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsMemory;
          SubItems.Add(Format('%d B (%d MB)',[Adapter[AIndex].Memory,(Adapter[AIndex].Memory div 1024) div 1024]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsBIOS;
          SubItems.Add(Format('%s',[Adapter[AIndex].BIOS]));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;


procedure Tmdi_msi_Viewer.DisplayVideoProps;
var
  i: Integer;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Display do begin
        with Add do begin
          Caption:=Format('%s %s',[rsBIOS,rsString]);
          SubItems.Add(Format('%s',[BIOSString]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsBIOS,rsVersion]);
          SubItems.Add(Format('%s',[BIOSVersion]));
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsBIOS,rsDate]);
          SubItems.Add(Format('%s',[BIOSDate]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:='';
          ImageIndex:=-2;
        end;
        with Add do begin
          Caption:=rsTechnology;
          SubItems.Add(Format('%s',[Technology]));
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsResolution;
          SubItems.Add(Format('%d x %d - %dbit',[HorzRes,VertRes,ColorDepth]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsPhysical,rsSize]);
          SubItems.Add(Format('(%d x %d) mm',[HorzSize,VertSize]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsPixel,rsWidth]);
          SubItems.Add(Format('%d',[PixelWidth]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsPixel,rsHeight]);
          SubItems.Add(Format('%d',[PixelHeight]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsPixel,rsDiagonal]);
          SubItems.Add(Format('%d',[PixelDiagonal]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsFont,rsResolution]);
          SubItems.Add(Format('%d dpi',[FontResolution]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsVRR;
          SubItems.Add(Format('%d MHz',[VerticalRefreshRate]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:='';
          ImageIndex:=-2;
        end;
        with Add do begin
          Caption:=rsConnectedMonitors;
          SubItems.Add(Format('%d',[Screen.MonitorCount]));
          ImageIndex:=-3;
        end;
        for i:=0 to Screen.MonitorCount-1 do
          with Add do begin
            if Screen.Monitors[i].Primary then
              Caption:='Primary Monitor'
            else
              Caption:='Monitor #'+IntToStr(Screen.Monitors[i].MonitorNum);
            SubItems.Add(Format('%d x %d',[Screen.Monitors[i].Width,Screen.Monitors[i].Height]));
            ImageIndex:=-1;
          end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayMonitorDetail;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Monitor,Monitors[AIndex] do begin
        with Add do begin
          Caption:=rsMonitor;
          SubItems.Add(Format('%s',[EDID.Name]));
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsDescription;
          SubItems.Add(Format('%s',[DeviceDescription]));
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsProduct,rsNumber]);
          SubItems.Add(Format('%s',[EDID.ProductNumber]));
          ImageIndex:=-1;
        end;
        //ExpandMON(Monitors[AIndex].Model,m);
        with Add do begin
          Caption:=rsModel;
          SubItems.Add(Format('%s',[Model]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsManufacturer;
          SubItems.Add(Manufacturer);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsDisplay,rsSize]);
          SubItems.Add(Format('(%d x %d) cm',[EDID.Width,EDID.Height]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsResolution;
          SubItems.Add(Format('(%d x %d) px',[RectWidth(Bounds),RectHeight(Bounds)]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsDisplay,rsGamma]);
          SubItems.Add(Format('%1.2f',[EDID.Gamma]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s',[rsDate]);
          SubItems.Add(Format('%d/%d',[EDID.Year,EDID.Week]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s',[rsSerial]);
          SubItems.Add(Format('%d',[EDID.SerialNumber]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:='Primary';
          SubItems.Add(IntToStr(Integer(Primary)));
          ImageIndex:=-4;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayPhysDriveDetail;
var
  s: string;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Storage.Physical[AIndex] do begin
        with Add do begin
          Caption:=rsModel;
          SubItems.Add(Format('%s',[Model]));
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsRevision;
          SubItems.Add(Format('%s',[Revision]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSerial;
          SubItems.Add(Format('%s',[SerialNumber]));
          ImageIndex:=-1;
        end;
        with List.Items.Add do begin
          Caption:='Bus Type';
          SubItems.Add(GetStorageBusTypeStr(BusType));
          ImageIndex:=-3;
        end;
        s:='';
        case DeviceType of
          FILE_DEVICE_CD_ROM: s:='CD-ROM';
          FILE_DEVICE_DVD: s:='DVD';
          FILE_DEVICE_MASS_STORAGE: s:='Mass Storage';
          FILE_DEVICE_TAPE: s:='Tape';
          FILE_DEVICE_DISK: begin
            s:='Drive';
            if SSD then
              s:='Solid State Drive';
          end;
        end;
        if s<>'' then
          with List.Items.Add do begin
            Caption:='Device Type';
            SubItems.Add(s);
          end;
        with List.Items.Add do begin
          Caption:='Media Type';
          SubItems.Add(GetDeviceMediaTypeStr(MediaType));
        end;
        if (DeviceType=FILE_DEVICE_CD_ROM) or (DeviceType=FILE_DEVICE_DVD) then begin
          s:='';
          if Read_CDRW then
            s:=s+'CD-R,';
          if Read_CDR then
            s:=s+'CD-RW,';
          if Read_DVDROM then
            s:=s+'DVD-ROM,';
          if Read_DVDR then
            s:=s+'DVD-R,';
          if Read_DVDRAM then
            s:=s+'DVD-RAM,';
          SetLength(s,Length(s)-1);
          with List.Items.Add do begin
            Caption:='Read Caps';
            SubItems.Add(s);
          end;
          s:='';
          if Write_CDRW then
            s:=s+'CD-R,';
          if Write_CDR then
            s:=s+'CD-RW,';
          if Write_DVDR then
            s:=s+'DVD-R,';
          if Write_DVDRAM then
            s:=s+'DVD-RAM,';
          SetLength(s,Length(s)-1);
          with List.Items.Add do begin
            Caption:='Write Caps';
            SubItems.Add(s);
          end;
        end;
        if HaId>=0 then
          with Add do begin
            Caption:=rsAdapter;
            SubItems.Add(Format('%d',[HaId]));
            ImageIndex:=-1;
          end;
        if Target>=0 then
          with Add do begin
            Caption:=rsTarget;
            SubItems.Add(Format('%d',[Target]));
            ImageIndex:=-1;
          end;
        if Lun>=0 then
          with Add do begin
            Caption:=rsLUN;
            SubItems.Add(Format('%d',[Lun]));
            ImageIndex:=-1;
          end;

        if Temperature>0 then begin
          with Add do begin
            Caption:='';
            ImageIndex:=-2;
          end;
          with List.Items.Add do begin
            Caption:=rsTemperature;
            SubItems.Add(Format('%d%sC',[Temperature,{$IFDEF UNICODE}#$00B0{$ELSE}'°'{$ENDIF}]));
            ImageIndex:=-3;
          end;
        end;
        if Size>0 then begin
          with Add do begin
            Caption:='';
            ImageIndex:=-2;
          end;
          with Add do begin
            Caption:=rsCapacity;
            SubItems.Add(Format('%d MB',[Size shr 20]));
            ImageIndex:=-3;
          end;
          with Add do begin
            Caption:=rsNumCyls;
            SubItems.Add(Format('%d',[Geometry.Cylinders.QuadPart]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsNumHeads;
            SubItems.Add(Format('%d',[Geometry.TracksPerCylinder]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsSPT;
            SubItems.Add(Format('%d',[Geometry.SectorsPerTrack]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsBPS;
            SubItems.Add(Format('%d',[Geometry.BytesPerSector]));
            ImageIndex:=-1;
          end;
          if IdentifyDeviceData.Max48BitLBA[0]=0 then
            with Add do begin
              Caption:='Physical sectors';
              if (Geometry.BytesPerSector>0) then
                SubItems.Add(Format('%d',[LengthInBytes div Geometry.BytesPerSector]))
              else
                SubItems.Add(Format('%d',[Geometry.Cylinders.QuadPart*Geometry.TracksPerCylinder*Geometry.SectorsPerTrack]));
              ImageIndex:=-1;
            end
          else begin
            with Add do begin
              Caption:='Number of sectors';
              SubItems.Add(Format('%d',[IdentifyDeviceData.CurrentSectorCapacity]));
              ImageIndex:=-1;
            end;
            with Add do begin
              Caption:='Total 32-bit LBA sectors';
              SubItems.Add(Format('%d',[IdentifyDeviceData.UserAddressableSectors]));
              ImageIndex:=-1;
            end;
            with Add do begin
              Caption:='Total 48-bit LBA sectors';
              SubItems.Add(Format('%d',[IdentifyDeviceData.Max48BitLBA[0]]));
              ImageIndex:=-1;
            end;
          end;
        end;
        if IdentifyDeviceData.MajorRevision>0 then
          with Add do
            ImageIndex:=-2;
        if IdentifyDeviceData.MajorRevision>0 then
          with Add do begin
            Caption:='ATA Major version';
            SubItems.Add(GetATAMajorVersion(IdentifyDeviceData.MajorRevision));
            ImageIndex:=-3;
          end;
        if IdentifyDeviceData.MinorRevision>0 then
          with Add do begin
            Caption:='ATA Minor version';
            SubItems.Add(GetATAMinorVersion(IdentifyDeviceData.MinorRevision));
            ImageIndex:=-1;
          end;
        if IdentifyDeviceData.ReservedWord220[2]>0 then
          with Add do begin
            Caption:='ATA Transport version';
            SubItems.Add(GetATATransportVersion(IdentifyDeviceData.ReservedWord220[2]));
            ImageIndex:=-1;
          end;
        if ECCCode<>0 then
          with Add do begin
            Caption:=rsECC;
            SubItems.Add(Format('%d',[ECCCode]));
            ImageIndex:=-1;
          end;
        if CtlBufSize<>0 then
          with Add do begin
            Caption:='Cache Buffer Size';
            SubItems.Add(Format('%d KB',[CtlBufSize shr 10]));
            ImageIndex:=-1;
          end;
        with Add do begin
          ImageIndex:=-2;
        end;
        with Add do begin
          Caption:=Format('%s',[rsSMART]);
          if SMARTSupport then
            SubItems.Add(rsSupported)
          else
            SubItems.Add(Format('%s %s',[rsNOT,rsSupported]));
          ImageIndex:=-3;
        end;
        if SMARTSupport then
          with Add do begin
            Caption:=rsActive;
            SubItems.Add(IntToStr(Integer(SMARTActive)));
            ImageIndex:=-4;
          end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayWinStorageDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Devices.Devices[AIndex] do begin
        with Add do begin
          Caption:=Format('%s',[rsClass]);
          SubItems.Add(Format('%s',[ClassDesc]));
          ImageIndex:=-3;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(SIC);
  Action:=caFree;
end;

procedure Tmdi_msi_Viewer.FormCreate(Sender: TObject);
begin
  SIC:=TMiTeC_SystemInfo.Create(Self);
  {$IFDEF THEMESUPPORT}
  Header.ParentBackground:=False;
  {$ENDIF}
end;

procedure Tmdi_msi_Viewer.DisplayLogDriveDetail(AIndex: Integer);
var
  b: Integer;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Storage.Logical[AIndex] do begin
        if not(DeviceType in [FILE_DEVICE_CD_ROM,FILE_DEVICE_DVD, FILE_DEVICE_TAPE,FILE_DEVICE_UNKNOWN]) and (Length(Layout)>0) and (LayoutIndex>-1) then begin
          with Add do begin
            Caption:=rsType;
            SubItems.Add(GetPartitionType(Layout[LayoutIndex].Number,Layout[LayoutIndex].Typ));
            ImageIndex:=-3;
          end;
          with Add do begin
            Caption:=rsActive;
            if Layout[LayoutIndex].BootIndicator then
              b:=1
            else
              b:=0;
            SubItems.Add(Format('%d',[b]));
            ImageIndex:=-4;
          end;
          with Add do begin
            Caption:=rsSystem;
            SubItems.Add(FileSystem{GetPartitionSystem(Layout[LayoutIndex].Typ)});
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsStartOffset;
            SubItems.Add(Format('%d B',[Layout[LayoutIndex].StartingOffset.QuadPart]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsSize;
            SubItems.Add(Format('%d MB',[Layout[LayoutIndex].Length.QuadPart shr 20]));
            ImageIndex:=-1;
          end;
          with Add do begin
            ImageIndex:=-2;
          end;
        end;
        with Add do begin
          Caption:=rsDrive;
          SubItems.Add(Format('%s:',[Drive]));
          ImageIndex:=-3;
        end;
        SIC.Disk.Drive:=Format('%s:',[Drive]);
        with Add do begin
          Caption:=rsType;
          SubItems.Add(SIC.Disk.GetMediaTypeStr(SIC.Disk.MediaType));
          ImageIndex:=-1;
        end;
        if SIC.Disk.MediaPresent then begin
          with Add do begin
            Caption:=rsVolumeLabel;
            SubItems.Add(SIC.Disk.VolumeLabel);
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsSerial;
            SubItems.Add(SIC.Disk.SerialNumber);
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsCapacity;
            SubItems.Add(Format('%d MB',[SIC.Disk.capacity shr 20]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsFreeSpace;
            SubItems.Add(Format('%d MB',[SIC.Disk.FreeSpace shr 20]));
            ImageIndex:=-1;
          end;
          with List.Items.Add do begin
            Caption:='File system';
            SubItems.Add(SIC.Disk.FileSystem);
          end;
          with SIC.Storage.Logical[AIndex] do
            if LayoutIndex>-1 then begin
              with List.Items.Add do begin
                Caption:='First sector';
                SubItems.Add(IntToStr(Layout[LayoutIndex].StartingOffset.QuadPart div Geometry.BytesPerSector));
              end;
              with List.Items.Add do begin
                Caption:='Last sector';
                SubItems.Add(IntToStr((Layout[LayoutIndex].StartingOffset.QuadPart+Layout[LayoutIndex].Length.QuadPart-1) div Geometry.BytesPerSector));
              end;
              with List.Items.Add do begin
                Caption:='Cluster size';
                SubItems.Add(Format('%d B',[ClusterSize]));
              end;
              with List.Items.Add do begin
                Caption:='Total sectors';
                SubItems.Add(IntToStr(Layout[LayoutIndex].Length.QuadPart div Geometry.BytesPerSector));
              end;
            end;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayASPIDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Engines.ASPI32 do begin
        with Add do begin
          Caption:=Format('%s',[rsModel]);
          SubItems.Add(Format('%s%s',[Configuration.Vendor[Aindex],Configuration.Model[Aindex]]));
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=Format('%s',[rsRevision]);
          SubItems.Add(Format('%s',[Configuration.Revision[Aindex]]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s',[rsType]);
          SubItems.Add(Format('%s',[GetTypeStr(StrToInt(Configuration.Typ[Aindex]))]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s',[rsAdapter]);
          SubItems.Add(Format('%s',[Configuration.Host[Aindex]]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s',[rsID]);
          SubItems.Add(Format('%s',[Configuration.ID[Aindex]]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s',[rsLUN]);
          SubItems.Add(Format('%s',[Configuration.LUN[Aindex]]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s',[rsExtra]);
          SubItems.Add(Format('%s',[Configuration.Extra[Aindex]]));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayAudioDetail;
var
  i: integer;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Media do begin
        with Add do begin
          Caption:=Format('%s %s',[rsAudio,rsAdapter]);
          SubItems.Add(Tree.Selected.Text);
          ImageIndex:=-3;
        end;
        for i:=0 to WaveIn.Count-1 do
          with Add do begin
            Caption:='WAVE In';
            SubItems.Add(WaveIn[i]);
            ImageIndex:=-1;
          end;
        for i:=0 to WaveOut.Count-1 do
          with Add do begin
            Caption:='WAVE Out';
            SubItems.Add(WaveOut[i]);
            ImageIndex:=-1;
          end;
        for i:=0 to MIDIIn.Count-1 do
          with Add do begin
            Caption:='MIDI In';
            SubItems.Add(MIDIIn[i]);
            ImageIndex:=-1;
          end;
        for i:=0 to MIDIOut.Count-1 do
          with Add do begin
            Caption:='MIDI Out';
            SubItems.Add(MIDIOut[i]);
            ImageIndex:=-1;
          end;
        for i:=0 to AUX.Count-1 do
          with Add do begin
            Caption:='AUX';
            SubItems.Add(AUX[i]);
            ImageIndex:=-1;
          end;
        for i:=0 to Mixer.Count-1 do
          with Add do begin
            Caption:='Mixer';
            SubItems.Add(Mixer[i]);
            ImageIndex:=-1;
          end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayNetworkDetail;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      if SIC.Network.TCPIP.AdapterCount>0 then
        with SIC.Network.TCPIP.Adapter[AIndex] do begin
          with Add do begin
            Caption:=Format('%s %s',[rsAdapter,rsName]);
            SubItems.Add(Name);
            ImageIndex:=-3;
          end;
          with Add do begin
            Caption:='Alias';
            SubItems.Add(Alias);
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsMACAddress;
            SubItems.Add(Address);
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:='IPv4';
            SubItems.Add(IPAddress.CommaText);
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:='IPv6';
            SubItems.Add(IPv6Address.CommaText);
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsType;
            try
              SubItems.Add(AdapterTypes[Typ]);
            except
              SubItems.Add('<unknown>');
            end;
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=Format('%s %s',[rsMax,rsSpeed]);
            SubItems.Add(Format('%1.0n Kbps',[MaxSpeed/1000]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsMTU;
            SubItems.Add(Format('%1.0n B',[MTU/1]));
            ImageIndex:=-1;
          end;
        end
      else
        with SIC.Network do begin
          with Add do begin
            Caption:=Format('%s %s',[rsAdapter,rsName]);
            if (AIndex>=PhysicalAdapters.Count) then
              SubItems.Add(VirtualAdapters[AIndex-PhysicalAdapters.Count])
            else
              SubItems.Add(PhysicalAdapters[AIndex]);
            ImageIndex:=-3;
          end;
          with Add do begin
            Caption:=rsMACAddress;
            try
              SubItems.Add(MACAddresses[AIndex]);
            except
              SubItems.Add('');
            end;
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsIPAddress;
            try
              SubItems.Add(IPAddresses[AIndex]);
            except
              SubItems.Add('');
            end;
            ImageIndex:=-1;
          end;
        end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayUSBDetail(AIndex: Integer);
var
  s: string;
  i: Integer;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.USB.USBNodes[AIndex] do begin
        if (USBClass in [usbReserved..usbStorage,usbVendorSpec,usbUnknown,usbError]) and (USBDevice.ConnectionStatus>0) then begin
          with Add do begin
            Caption:=rsClass;
            if USBDevice.USBClassname<>'' then
              SubItems.Add(USBDevice.USBClassname)
            else
              SubItems.Add(ClassNames[Integer(USBClass)]);
            ImageIndex:=-3;
          end;
          with Add do begin
            Caption:='Name';
            SubItems.Add(Connectionname);
            ImageIndex:=-1;
          end;
          //ExpandUSB(USBDevice.VendorID,USBDevice.ProductID,s);
          if s<>'' then
            with Add do begin
              Caption:=rsVendor;
              SubItems.Add(s);
              ImageIndex:=-1;
            end;
          with Add do begin
            Caption:=Format('%s %s',[rsVendor,rsID]);
            SubItems.Add(Format('%4.4x',[USBDevice.VendorID]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=Format('%s %s',[rsProduct,rsID]);
            SubItems.Add(Format('%4.4x',[USBDevice.ProductID]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsProduct;
            SubItems.Add(USBDevice.Product);
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsManufacturer;
            SubItems.Add(USBDevice.Manufacturer);
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsSerial;
            SubItems.Add(USBDevice.Serial);
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsPowerCons;
            SubItems.Add(Format('%d mA',[USBDevice.MaxPower]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsSpecification;
            SubItems.Add(Format('%d.%d',[USBDevice.MajorVersion,USBDevice.MinorVersion]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:='Featured devices';
            SubItems.Add(Format('%d',[Length(USBDevice.Registry)]));
            ImageIndex:=-1;
          end;
          if Length(USBDevice.Registry)>0 then begin
            Add.ImageIndex:=-2;
            for i:=0 to High(USBDevice.Registry) do begin
              with Add do begin
                Caption:='Device';
                SubItems.Add(USBDevice.Registry[i].DeviceClass);
                ImageIndex:=-3;
              end;
              with Add do begin
                Caption:='Name';
                SubItems.Add(USBDevice.Registry[i].Name);
                ImageIndex:=-1;
              end;
              if USBDevice.Registry[i].Drive<>'' then
                with Add do begin
                  Caption:='Drive';
                  SubItems.Add(USBDevice.Registry[i].Drive+':');
                  ImageIndex:=-1;
                end;
            end;
          end;
        end;
      end;
    finally
      EndUpdate;                             
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayUSBSummary;
var
  i: Integer;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.USB do begin
        with Add do begin
          Caption:=Format('%s %s',[rsConnected,rsDevices]);
          SubItems.Add(IntToStr(ConnectedDevices));
          ImageIndex:=-3;
        end;
        for i:=0 to USBNodeCount-1 do
          if USBNodes[i].USBDevice.ConnectionStatus=1 then
            if not(USBNodes[i].USBClass in [usbHostController,usbHub,usbExternalHub]) then begin
              with Add do begin
                if USBNodes[i].USBDevice.USBClassname<>'' then
                  Caption:=USBNodes[i].USBDevice.USBClassname
                else
                  Caption:=ClassNames[Integer(USBNodes[i].USBClass)];
                SubItems.Add(Format('%s (%s)',[USBNodes[i].USBDevice.Product,Trim(USBNodes[i].USBDevice.Manufacturer)]));
              end;
            end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayNetworkInfo;
var
  i: Integer;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Network do begin
        with Add do begin
          Caption:=rsProtocols;
          SubItems.Add(Format('%d',[Protocols.Count]));
          ImageIndex:=-3;
        end;
        for i:=0 to Protocols.Count-1 do
          with Add do begin
            Caption:=Protocols[i];
            ImageIndex:=-1;
          end;
        with Add do begin
          Caption:='';
          ImageIndex:=-2;
        end;
        with Add do begin
          Caption:=rsClients;
          SubItems.Add(Format('%d',[Clients.Count]));
          ImageIndex:=-3;
        end;
        for i:=0 to Clients.Count-1 do
          with Add do begin
            Caption:=Clients[i];
            ImageIndex:=-1;
          end;
        with Add do begin
          Caption:='';
          ImageIndex:=-2;
        end;
        with Add do begin
          Caption:=rsServices;
          SubItems.Add(Format('%d',[Services.Count]));
          ImageIndex:=-3;
        end;
        for i:=0 to Services.Count-1 do
          with Add do begin
            Caption:=Services[i];
            ImageIndex:=-1;
          end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayPrinterDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Printers do begin
        with Add do begin
          Caption:=rsDriver;
          SubItems.Add(Driver[AIndex]);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsDriver,rsPath]);
          SubItems.Add(DriverPath[AIndex]);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsDriver,rsVersion]);
          SubItems.Add(DriverVersion[AIndex]);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsPort;
          SubItems.Add(Port[AIndex]);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsMonitor;
          SubItems.Add(Monitor[AIndex]);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsComment;
          SubItems.Add(Comment[AIndex]);
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSProcessorDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS.Processor[AIndex] do begin
        with Add do begin
          Caption:=rsManufacturer;
          SubItems.Add(Manufacturer);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsSocket,rsDesignation]);
          SubItems.Add(Socket);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsUpgrade, rsInterface]);
          try
            SubItems.Add(Upgrades[Upgrade]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(Upgrade)]));
          end;
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsVoltage;
          SubItems.Add(Format('%1.1f V',[Voltage]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsFreq;
          SubItems.Add(Format('%d MHz',[Frequency]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsExternalClock;
          SubItems.Add(Format('%d MHz',[ExternalClock]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSerial;
          SubItems.Add(SerialNumber);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsAssetTag;
          SubItems.Add(AssetTag);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsPart,rsNumber]);
          SubItems.Add(PartNumber);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsCN;
          SubItems.Add(IntToStr(CoreCount));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsTN;
          SubItems.Add(IntToStr(ThreadCount));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.TreeGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if Assigned(Node) then begin
    Node.ImageIndex:=Node.Level mod 6;
    Node.SelectedIndex:=Node.ImageIndex;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSMemCtrlInfo;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS do begin
        with Add do begin
          Caption:=Format('%s %s',[rsNumberOf,rsSlots]);
          SubItems.Add(Format('%d',[MemCtrlSlotCount]));
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsCurrent,rsInterleave]);
          try
            SubItems.Add(InterleaveSupports[MemCtrlCurrentInterleave]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(MemCtrlCurrentInterleave)]));
          end;
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsSupported,rsInterleave]);
          try
            SubItems.Add(InterleaveSupports[MemCtrlSupportedInterleave]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(memCtrlSupportedInterleave)]));
          end;
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsSupported,rsSpeeds]);
          SubItems.Add(GetMemorySpeedStr(MemCtrlSupportedSpeeds));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsSupported,rsTypes]);
          SubItems.Add(GetMemoryTypeStr(MemCtrlSupportedTypes));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsSupported,rsVoltages]);
          SubItems.Add(GetMemoryVoltageStr(MemCtrlSupportedVoltages));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s %s',[rsMaximum,rsSlot,rsSize]);
          SubItems.Add(Format('%d MB',[MemCtrlMaxSize]));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSBatteryDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS.Battery[AIndex] do begin
        with Add do begin
          Caption:=rsLocation;
          SubItems.Add(Location);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsManufacturer;
          SubItems.Add(Manufacturer);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsManufacturer+' '+rsDate;
          if ManufacturerDate='' then
            SubItems.Add(DateToStr(EncodeDate(1980+(SBDSManufactureDate shr 9),(SBDSManufactureDate shr 5) and 15,SBDSManufactureDate and 31)))
          else
            SubItems.Add(ManufacturerDate);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSerial+' '+rsNumber;
          if SerialNumber='' then
            SubItems.Add(IntToHex(SBDSSerialNumber,4))
          else
            SubItems.Add(SerialNumber);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsdevice+' '+rsName;
          SubItems.Add(DeviceName);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsChemistry;
          if DeviceChemistry=dcUnknown then
            SubItems.Add(SBDSDeviceChemistry)
          else
            SubItems.Add(DeviceChemistries[DeviceChemistry]);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsCapacity;
          SubItems.Add(Format('%d mWh',[DesignCapacity*Min(1,DesignCapacityMultiplier)]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsVoltage;
          SubItems.Add(Format('%d mV',[DesignVoltage]));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSBIOSInfo;
var
  bc: TSMBIOS_BIOSChar;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS do begin
        with Add do begin
          Caption:=rsVendor;
          SubItems.Add(BIOSVendor);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsVersion;
          SubItems.Add(BIOSVersion);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsDate;
          SubItems.Add(BIOSDate);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSize;
          SubItems.Add(Format('%d KB',[BIOSSize]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSystem+' '+rsVersion;
          SubItems.Add(Format('%d.%d',[BIOSMajorVersion,BIOSMinorVersion]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:='';
          ImageIndex:=-2;
        end;
        with Add do begin
          Caption:=rsBIOSChars;
          SubItems.Add('');
          ImageIndex:=-3;
        end;
        for bc:=Low(TSMBIOS_BIOSChar) to High(TSMBIOS_BIOSChar) do
          with Add do begin
            Caption:=Format('%s',[BIOSChars[bc]]);
            SubItems.Add(Format('%d',[Integer(bc in BIOSCharacteristics)]));
            ImageIndex:=-4;
          end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSChassisInfo;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS do begin
        with Add do begin
          Caption:=rsModel;
          try
            SubItems.Add(ChassisTypes[ChassisModel]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(ChassisModel)]));
          end;
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsManufacturer;
          SubItems.Add(ChassisManufacturer);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsVersion;
          SubItems.Add(ChassisVersion);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSerial;
          SubItems.Add(ChassisSerial);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsAssetTag;
          SubItems.Add(ChassisAssetTag);
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSCoolDevDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS.CoolingDevice[AIndex] do begin
        with Add do begin
          Caption:=rsDescription;
          SubItems.Add(Description);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsType;
          if Typ in [Low(TSMBIOS_CoolingType)..High(TSMBIOS_CoolingType)] then
            SubItems.Add(CoolingTypes[Typ])
          else
            SubItems.Add('?');
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsStatus;
          if Status>High(TSMBIOS_StatusType) then
            SubItems.Add(Format('Unknown status: %d',[Integer(Status)]))
          else
            SubItems.Add(StatusTypes[Status]);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsNominal+' '+rsSpeed;
          if NominalSpeed=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(NominalSpeed)+' rpm');
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsGroup+' '+rsUnit;
          SubItems.Add(IntToStr(GroupUnit));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSCurrProbeDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS.CurrentProbe[AIndex] do begin
        with Add do begin
          Caption:=rsDescription;
          SubItems.Add(Description);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsType;
          if Location in [Low(TSMBIOS_CurrProbeLocationType)..High(TSMBIOS_CurrProbeLocationType)] then
            SubItems.Add(CurrProbeLocationTypes[Location])
          else
            SubItems.Add('?');
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsStatus;
          if Status>High(TSMBIOS_StatusType) then
            SubItems.Add(Format('Unknown status: %d',[Integer(Status)]))
          else
            SubItems.Add(StatusTypes[Status]);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsMin;
          if Min=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(Min));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsMax;
          if Max=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(Max));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsResolution;
          if Resolution=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(Resolution));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsTolerance;
          if Tolerance=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(Tolerance));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsAccuracy;
          if Accuracy=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(Accuracy));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsValue;
          if NominalValue=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(NominalValue));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSMainboardInfo;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS do begin
        with Add do begin
          Caption:=rsModel;
          SubItems.Add(MainboardModel);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsManufacturer;
          SubItems.Add(MainboardManufacturer);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsVersion;
          SubItems.Add(MainboardVersion);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSerial;
          SubItems.Add(MainboardSerial);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsAssetTag;
          SubItems.Add(MainboardAssetTag);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsLIC;
          SubItems.Add(MainboardLocationInChassis);
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSSystemInfo;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS do begin
        with Add do begin
          Caption:=rsModel;
          SubItems.Add(SystemModel);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsManufacturer;
          SubItems.Add(SystemManufacturer);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsVersion;
          SubItems.Add(SystemVersion);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSerial;
          SubItems.Add(SystemSerial);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsUUID;
          SubItems.Add(SystemUUID);
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSSystemPowerSupply;
var
  s: string;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS do begin
        with Add do begin
          Caption:=rsPowerUnitGroup;
          SubItems.Add(IntToStr(SystemPowerSupplyPowerUnitGroup));
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsLocation;
          SubItems.Add(SystemPowerSupplyLocation);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsDevice+' '+rsName;
          SubItems.Add(SystemPowerSupplyDeviceName);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsManufacturer;
          SubItems.Add(SystemPowerSupplyManufacturer);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSerial+' '+rsNumber;
          SubItems.Add(SystemPowerSupplySerialNumber);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsAssetTag;
          SubItems.Add(SystemPowerSupplyAssetTagNumber);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsPart+' '+rsNumber;
          SubItems.Add(SystemPowerSupplyModelPartNumber);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsRevision+' '+rsLevel;
          SubItems.Add(SystemPowerSupplyRevisionLevel);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsMaxPowerCapacity;
          if SystemPowerSupplyMaxPowerCapacity=$8000 then
            s:='n/a'
         else
           s:=IntToStr(SystemPowerSupplyMaxPowerCapacity)+' mW';
          SubItems.Add(s);
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSCacheDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS.Cache[AIndex] do begin
        with Add do begin
          Caption:=Format('%s %s',[rsSocket,rsDesignation]);
          SubItems.Add(Designation);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsType;
          try
            SubItems.Add(CacheTypes[Typ]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(Typ)]));
          end;
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsAssoc;
          try
            SubItems.Add(CacheAssociativities[Associativity]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(Associativity)]));
          end;
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSRAM;
          try
            SubItems.Add(SRAMTypes[SRAMType]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(SRAMType)]));
          end;
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSize;
          SubItems.Add(Format('%d KB',[InstalledSize]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsMaximum,rsSize]);
          SubItems.Add(Format('%d KB',[MaxSize]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSpeed;
          SubItems.Add(Format('%d ns',[Speed]));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSPMAInfo;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS do begin
        with Add do begin
          Caption:=rsLocation;
          SubItems.Add(PMALocations[PMALocation]);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsUse;
          SubItems.Add(PMAUses[PMAUse]);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsErrorCorrectionType;
          SubItems.Add(PMAErrorCorrectionTypes[PMAErrorCorrectionType]);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsMaximumCapacity;
          SubItems.Add(IntToStr(PMAMaximumCapacity));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsNumberOfMemoryDevices;
          SubItems.Add(IntToStr(PMANumberOfMemoryDevices));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSPortDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS.Port[AIndex] do begin
        with Add do begin
          Caption:=rsType;
          try
            SubItems.Add(PortTypes[Typ]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(Typ)]));
          end;
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsInternal,rsDesignation]);
          SubItems.Add(InternalDesignator);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsInternal,rsConnector]);
          try
            SubItems.Add(ConnectorTypes[InternalConnector]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(InternalConnector)]));
          end;
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsExternal,rsDesignation]);
          SubItems.Add(ExternalDesignator);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsExternal,rsConnector]);
          try
            SubItems.Add(ConnectorTypes[ExternalConnector]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(ExternalConnector)]));
          end;
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSSystemSlotDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS.SystemSlot[AIndex] do begin
        with Add do begin
          Caption:=rsDesignation;
          SubItems.Add(Designation);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsType;
          try
            SubItems.Add(SlotTypes[Typ]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(Typ)]));
          end;
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsData,rsBus]);
          try
            SubItems.Add(DataBusTypes[DataBus]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(DataBus)]));
          end;
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsCurrent,rsUsage]);
          try
            SubItems.Add(SlotUsages[Usage]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(Usage)]));
          end;
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsLen;
          try
            SubItems.Add(SlotLengths[Length]);
          except
            SubItems.Add(Format('unknown(%d)',[Integer(Length)]));
          end;
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsBus,rsNumber]);
          SubItems.Add(IntToStr(BusNumber));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsDevice,rsNumber]);
          SubItems.Add(IntToStr(DevNumber));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsFunction,rsNumber]);
          SubItems.Add(IntToStr(FuncNumber));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSTempProbeDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS.TemperatureProbe[AIndex] do begin
        with Add do begin
          Caption:=rsDescription;
          SubItems.Add(Description);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsType;
          if Location in [Low(TSMBIOS_TempProbeLocationType)..High(TSMBIOS_TempProbeLocationType)] then
            SubItems.Add(TempProbeLocationTypes[Location])
          else
            SubItems.Add('?');
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsStatus;
          if Status>High(TSMBIOS_StatusType) then
            SubItems.Add(Format('Unknown status: %d',[Integer(Status)]))
          else
            SubItems.Add(StatusTypes[Status]);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsMin;
          if Min=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(Min));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsMax;
          if Max=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(Max));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsResolution;
          if Resolution=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(Resolution));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsTolerance;
          if Tolerance=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(Tolerance));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsAccuracy;
          if Accuracy=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(Accuracy));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsValue;
          if NominalValue=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(NominalValue));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSTPMDevDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS.TPMDevice[AIndex] do begin
        with Add do begin
          Caption:=rsVendorID;
          SubItems.Add(VendorID);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsSpecVersion;
          SubItems.Add(Format('%d.%d',[MajorSpecVersion,MinorSpecVersion]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsDescription;
          SubItems.Add(Description);
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSVoltProbeDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS.VoltageProbe[AIndex] do begin
        with Add do begin
          Caption:=rsDescription;
          SubItems.Add(Description);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsType;
          if Location in [Low(TSMBIOS_VoltProbeLocationType)..High(TSMBIOS_VoltProbeLocationType)] then
            SubItems.Add(VoltProbeLocationTypes[Location])
          else
            SubItems.Add('?');
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsStatus;
          if Status>High(TSMBIOS_StatusType) then
            SubItems.Add(Format('Unknown status: %d',[Integer(Status)]))
          else
            SubItems.Add(StatusTypes[Status]);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsMin;
          if Min=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(Min));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsMax;
          if Max=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(Max));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsResolution;
          if Resolution=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(Resolution));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsTolerance;
          if Tolerance=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(Tolerance));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsAccuracy;
          if Accuracy=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(Accuracy));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsValue;
          if NominalValue=$8000 then
            SubItems.Add('N/A')
          else
            SubItems.Add(IntToStr(NominalValue));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayNodeInfo;
var
  n: TTreeNode;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC do begin
        with Add do begin
          Caption:=Format('%s %s',[rsNode,rsCount]);
          SubItems.Add(Format('%d',[Tree.Selected.Count]));
          ImageIndex:=-3;
        end;
        n:=Tree.Selected.GetFirstChild;
        while Assigned(n) do begin
          with Add do begin
            Caption:=n.Text;
            ImageIndex:=-1;
          end;
          if n=Tree.Selected.GetLastChild then
            Break;
          n:=n.getNextSibling;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayOtherDriveDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Disk do begin
        Drive:=Format('%s:',[Copy(AvailableDisks,AIndex,1)]);
        with Add do begin
          Caption:='Drive';
          SubItems.Add(Drive);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsType;
          SubItems.Add(GetMediaTypeStr(MediaType));
          ImageIndex:=-1;
        end;
        if MediaType=dtRemote then
          with Add do begin
            Caption:='UNC Path';
            SubItems.Add(UNCPath);
            ImageIndex:=-1;
          end;
        if MediaPresent then begin
          with Add do begin
            Caption:=rsVolumeLabel;
            SubItems.Add(VolumeLabel);
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsSerial;
            SubItems.Add(SerialNumber);
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsCapacity;
            SubItems.Add(Format('%d MB',[capacity shr 20]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:=rsFreeSpace;
            SubItems.Add(Format('%d MB',[FreeSpace shr 20]));
            ImageIndex:=-1;
          end;
          with List.Items.Add do begin
            Caption:='File system';
            SubItems.Add(FileSystem);
          end;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.acFullExpandExecute(Sender: TObject);
begin
  Tree.Items.BeginUpdate;
  try
    Tree.FullExpand;
  finally
    Tree.Items.EndUpdate;
  end;
end;

procedure Tmdi_msi_Viewer.acOverviewExecute(Sender: TObject);
var
  p: TInfoPages;
begin
  p:=pgAll;
  if SIC.LiveData then begin
    if not Prefs.ActiveDirectory then
      p:=p-[pgAD];
    if not Prefs.EventLog then
      p:=p-[pgEventLog];
  end;
  DisplayOverviewDlg(SIC,p);
end;

procedure Tmdi_msi_Viewer.acTextOverviewExecute(Sender: TObject);
begin
  with Tdlg_MSI_EO.Create(Application.Mainform) do
    try
      lMachine.Caption:=SIC.Machine.MachineName;
      SIC.ExportOverview(Memo.Lines);
      ShowModal;
    finally
      Free;
    end; 
end;

procedure Tmdi_msi_Viewer.OpenFile(AFilename: string);
begin
  Caption:=ExtractFilename(AFilename);
  SIC.SetLiveData(False);
  SIC.SetStorageName(AFilename);
  RefreshData;
end;

procedure Tmdi_msi_Viewer.OpenLocal;
begin
  RefreshData;
end;

procedure Tmdi_msi_Viewer.acFullCollapseExecute(Sender: TObject);
begin
  Tree.Items.BeginUpdate;
  try
    Tree.FullCollapse;
  finally
    Tree.Items.EndUpdate;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSOnBoardDeviceDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS.OnBoardDevice[AIndex] do begin
        with Add do begin
          Caption:=rsDevice;
          SubItems.Add(DeviceName);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsType;
          SubItems.Add(OnBoardDeviceTypes[Typ]);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsStatus;
          if Status then
            SubItems.Add(rsEnabled)
          else
            SubItems.Add(rsDisabled);
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSOnBoardDeviceExDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS.OnBoardDeviceEx[AIndex] do begin
        with Add do begin
          Caption:=rsDevice;
          SubItems.Add(DeviceName);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=rsType;
          SubItems.Add(OnBoardDeviceTypes[Typ]);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsStatus;
          if Status then
            SubItems.Add(rsEnabled)
          else
            SubItems.Add(rsDisabled);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsInstance;
          SubItems.Add(IntToStr(Instance));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSegmentGroup;
          SubItems.Add(IntToStr(SegmentGroupNumber));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsBus+' '+rsNumber;
          SubItems.Add(IntToStr(Busnumber));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsDevice+' '+rsNumber;
          SubItems.Add(IntToStr(Devicenumber));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsFunction+' '+rsNumber;
          SubItems.Add(IntToStr(Functionnumber));
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.SetControls;
begin

end;

procedure Tmdi_msi_Viewer.DisplaySMBIOSMemoryDeviceDetail(AIndex: Integer);
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.Machine.SMBIOS do begin
        with Add do begin
          Caption:=Format('%s',[rsDeviceLocator]);
          SubItems.Add(MemoryDevice[AIndex].DeviceLocator);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=Format('%s',[rsBankLocator]);
          SubItems.Add(MemoryDevice[AIndex].BankLocator);
          ImageIndex:=-3;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsDevice,rsType]);
          SubItems.Add(MemoryDeviceTypes[MemoryDevice[AIndex].Device]);
          ImageIndex:=-3;
        end;with Add do begin
          Caption:=Format('%s %s',[rsType,rsDetails]);
          SubItems.Add(GetMemoryTypeDetailsStr(MemoryDevice[AIndex].TypeDetails));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsFormFactor;
          SubItems.Add(MemoryFormFactors[MemoryDevice[AIndex].FormFactor]);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSize;
          SubItems.Add(Format('%d MB',[MemoryDevice[AIndex].Size]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSpeed;
          SubItems.Add(Format('%d MHz',[MemoryDevice[AIndex].Speed]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsTotal,rsWidth]);
          SubItems.Add(Format('%d b',[MemoryDevice[AIndex].TotalWidth]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsData,rsWidth]);
          SubItems.Add(Format('%d b',[MemoryDevice[AIndex].DataWidth]));
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsManufacturer;
          SubItems.Add(MemoryDevice[AIndex].Manufacturer);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsSerial;
          SubItems.Add(MemoryDevice[AIndex].SerialNumber);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=rsAssetTag;
          SubItems.Add(MemoryDevice[AIndex].AssetTag);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsPart,rsNumber]);
          SubItems.Add(MemoryDevice[AIndex].PartNumber);
          ImageIndex:=-1;
        end;
        with Add do begin
          Caption:=Format('%s %s',[rsMemory,rsTechnology]);
          SubItems.Add(MemoryTechnologies[MemoryDevice[AIndex].MemoryTechnology]);
          ImageIndex:=-1;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.ListDblClick(Sender: TObject);
begin
  if Assigned(List.Selected) and (List.Selected.SubItems.Count>0) then
    Clipboard.AsText:=List.Selected.SubItems[0];
end;

procedure Tmdi_msi_Viewer.LoadFromSIF(AComponent: TMiTeC_Component);
var
  sir: TStorageInfoRecord;
  rh: boolean;
begin
  rh:=True;
  AComponent.LoadStorageInfo(AComponent.StorageFileName,@sir);
  case sir.SIFFormat of
    0: AComponent.LoadFromStorage(AComponent.StorageFileName,rh);
    1: AComponent.LoadFromStorage(AComponent.StorageFileName,rh,DecompressStream);
  end;
end;

procedure Tmdi_msi_Viewer.DisplayCPUCache;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.CPU do begin
        CPUIndex:=AIndex;
        if LiveData then
          RefreshData
        else
          LoadFromSIF(SIC.CPU);
        with Cache.Level1 do
          if Data.SharedWays*Data.Size+Code.SharedWays*Code.Size>0 then begin
            with Add do begin
              Caption:='Level 1';
              with Cache.Level1 do
                SubItems.Add(Format('%d K',[Data.SharedWays*Data.Size+Code.SharedWays*Code.Size]));
              ImageIndex:=-3;
            end;
            if Data.Size>0 then begin
              with Add do begin
                Caption:='Data';
                SubItems.Add(Format('%d K',[Cache.Level1.Data.Size*Cache.Level1.Data.SharedWays]));
                ImageIndex:=-1;
              end;
              with Add do begin
                Caption:='Associativity';
                if Cache.Level1.Data.Associativity<>caNone then
                  SubItems.Add(Format('%s',[cAssociativityDescription[Cache.Level1.Data.Associativity]]))
                else
                  SubItems.Add(Format('%d-way',[Cache.Level1.Data.Ways]));
                ImageIndex:=-1;
              end;
              with Add do begin
                Caption:='Line Size';
                SubItems.Add(Format('%d entries',[Cache.Level1.Data.LineSize]));
                ImageIndex:=-1;
              end;
            end;
            if Code.Size>0 then begin
              with Add do begin
                Caption:='Code';
                SubItems.Add(Format('%d K',[Cache.Level1.Code.Size*Cache.Level1.Code.SharedWays]));
                ImageIndex:=-1;
              end;
              with Add do begin
                Caption:='Associativity';
                if Cache.Level1.Code.Associativity<>caNone then
                  SubItems.Add(Format('%s',[cAssociativityDescription[Cache.Level1.Code.Associativity]]))
                else
                  SubItems.Add(Format('%d-way',[Cache.Level1.Code.Ways]));
                ImageIndex:=-1;
              end;
              with Add do begin
                Caption:='Line Size';
                SubItems.Add(Format('%d entries',[Cache.Level1.Code.LineSize]));
                ImageIndex:=-1;
              end;
            end;
          end;
        if Cache.Level2.Size>0 then begin
          with Add do begin
            Caption:='';
            ImageIndex:=-2;
          end;
          with Add do begin
            Caption:='Level 2';
            SubItems.Add(Format('%d K',[Cache.Level2.Size*Cache.Level2.SharedWays]));
            ImageIndex:=-3;
          end;
          with Add do begin
            Caption:='Associativity';
            if Cache.Level2.Associativity<>caNone then
              SubItems.Add(Format('%s',[cAssociativityDescription[Cache.Level2.Associativity]]))
            else
              SubItems.Add(Format('%d-way',[Cache.Level2.Ways]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:='Line Size';
            SubItems.Add(Format('%d entries',[Cache.Level2.LineSize]));
            ImageIndex:=-1;
          end;
        end;
        if Cache.Level3.Size>0 then begin
          with Add do begin
            Caption:='';
            ImageIndex:=-2;
          end;
          with Add do begin
            Caption:='Level 3';
            SubItems.Add(Format('%d K',[Cache.Level3.Size*Cache.Level3.SharedWays]));
            ImageIndex:=-3;
          end;
          with Add do begin
            Caption:='Associativity';
            if Cache.Level3.Associativity<>caNone then
              SubItems.Add(Format('%s',[cAssociativityDescription[Cache.Level3.Associativity]]))
            else
              SubItems.Add(Format('%d-way',[Cache.Level3.Ways]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:='Line Size';
            SubItems.Add(Format('%d entries',[Cache.Level3.LineSize]));
            ImageIndex:=-1;
          end;
        end;
        if Cache.Trace.Size>0 then begin
          with Add do begin
            Caption:='';
            ImageIndex:=-2;
          end;
          with Add do begin
            Caption:='Trace';
            SubItems.Add(Format('%d K',[Cache.Trace.Size]));
            ImageIndex:=-3;
          end;
          with Add do begin
            Caption:='Associativity';
            SubItems.Add(Format('%s',[cAssociativityDescription[Cache.Trace.Associativity]]));
            ImageIndex:=-1;
          end;
          with Add do begin
            Caption:='Line Size';
            SubItems.Add(Format('%d entries',[Cache.Trace.LineSize]));
            ImageIndex:=-1;
          end;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayCPUFS;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.CPU, Features do begin
        CPUIndex:=AIndex;
        if LiveData then
          RefreshData
        else
          LoadFromSIF(SIC.CPU);
        if Standard1.Count>0 then
          with Add do begin
            Caption:=rsStandard+'-1';
            SubItems.Add(Format('%d',[Standard1.Count]));
            ImageIndex:=-1;
          end;
        if Standard2.Count>0 then
          with Add do begin
            Caption:=rsStandardEx+'-2';
            SubItems.Add(Format('%d',[Standard2.Count]));
            ImageIndex:=-1;
          end;
        if Extended1.Count>0 then
          with Add do begin
            Caption:=rsExtended+'-1';
            SubItems.Add(Format('%d',[Extended1.Count]));
            ImageIndex:=-1;
          end;
        if Extended2.Count>0 then
          with Add do begin
            Caption:=rsExtended+'-2';
            SubItems.Add(Format('%d',[Extended2.Count]));
            ImageIndex:=-1;
          end;
        if PowerManagement.Count>0 then
          with Add do begin
            Caption:=rsAPM;
            SubItems.Add(Format('%d',[Powermanagement.Count]));
            ImageIndex:=-1;
          end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayCPUFSExt1;
var
  i: Integer;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.CPU, Features.Extended1 do begin
        CPUIndex:=AIndex;
        if LiveData then
          RefreshData
        else
          LoadFromSIF(SIC.CPU);
        for i:=0 to Count-1 do
          with Add do begin
            Caption:=Format('%s - %s',[Features[i].Mnemonic,Features[i].Name]);
            SubItems.Add(Format('%d',[Integer(Features[i].Available)]));
            ImageIndex:=-4;
          end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayCPUFSExt2;
var
  i: Integer;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.CPU, Features.Extended2 do begin
        CPUIndex:=AIndex;
        if LiveData then
          RefreshData
        else
          LoadFromSIF(SIC.CPU);
        for i:=0 to Count-1 do
          with Add do begin
            Caption:=Format('%s - %s',[Features[i].Mnemonic,Features[i].Name]);
            SubItems.Add(Format('%d',[Integer(Features[i].Available)]));
            ImageIndex:=-4;
          end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayCPUFSStd1;
var
  i: Integer;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.CPU, Features.Standard1 do begin
        CPUIndex:=AIndex;
        if LiveData then
          RefreshData
        else
          LoadFromSIF(SIC.CPU);
        for i:=0 to Count-1 do
          with Add do begin
            Caption:=Format('%s - %s',[Features[i].Mnemonic,Features[i].Name]);
            SubItems.Add(Format('%d',[Integer(Features[i].Available)]));
            ImageIndex:=-4;
          end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayCPUFSStd2;
var
  i: Integer;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.CPU, Features.Standard2 do begin
        CPUIndex:=AIndex;
        if LiveData then
          RefreshData
        else
          LoadFromSIF(SIC.CPU);
        for i:=0 to Count-1 do
          with Add do begin
            Caption:=Format('%s - %s',[Features[i].Mnemonic,Features[i].Name]);
            SubItems.Add(Format('%d',[Integer(Features[i].Available)]));
            ImageIndex:=-4;
          end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Tmdi_msi_Viewer.DisplayCPUFSAPM;
var
  i: Integer;
begin
  with List, Items do begin
    BeginUpdate;
    try
      Clear;
      with SIC.CPU, Features.PowerManagement do begin
        CPUIndex:=AIndex;
        if LiveData then
          RefreshData
        else
          LoadFromSIF(SIC.CPU);
        for i:=0 to Count-1 do
          with Add do begin
            Caption:=Format('%s - %s',[Features[i].Mnemonic,Features[i].Name]);
            SubItems.Add(Format('%d',[Integer(Features[i].Available)]));
            ImageIndex:=-4;
          end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

end.

