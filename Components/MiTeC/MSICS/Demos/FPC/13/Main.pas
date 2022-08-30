unit Main;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, MSI_Common, MSI_Devices, ImgList;

const
  iiComputer      = 0;
  iiSystem        = 1;
  iiDisplay       = 2;
  iiMonitor       = 3;
  iiVolumes       = 4;
  iiFDD           = 5;
  iiHDD           = 6;
  iiCDROM         = 7;
  iiTape          = 8;
  iiAPM           = 9;
  iiImaging       = 10;
  iiKeyboard      = 11;
  iiMouse         = 12;
  iiModem         = 13;
  iiPort          = 14;
  iiAdapter       = 15;
  iiPackage       = 16;
  iiSCSI          = 17;
  iiDriver        = 18;
  iiSound         = 19;
  iiUSB           = 20;
  iiGame          = 21;
  iiNet           = 22;
  iiProcess       = 23;
  iiPCMCIA        = 24;
  iiChanger       = 25;
  iiHID           = 26;
  iiGPS           = 27;
  iiReader        = 28;
  iiInfrared      = 29;
  iiMIDI          = 30;
  iiWave          = 31;
  iiMixer         = 32;
  iiAUX           = 33;
  iiDirectX       = 34;
  iiPrinter       = 35;
  iiPrinterDef    = 36;
  iiNetPrinter    = 37;
  iiNetPrinterDef = 38;
  iiController    = 39;
  iiMemory        = 40;
  iiCPU           = 41;
  iiBluetooth     = 42;

  cDeviceImageIndex: array[TDeviceClass] of integer =
                      (iiAPM, iiSystem, iiVolumes, iiDisplay, iiCDROM, iiVolumes,
                       iiFDD, iiGPS, iiHID, iiVolumes, iiDriver, iiImaging,
                       iiInfrared, iiKeyboard, iiChanger, iiDriver, iiMouse, iiModem,
                       iiMonitor, iiReader, iiPort, iiAdapter, iiDriver,
                       iiPackage, iiDriver, iiAdapter, iiPort, iiPrinter, iiSCSI,
                       iiReader, iiSound, iiHDD, iiSystem, iiTape, iiController,
                       iiTape, iiUSB, iiCPU, iiBluetooth);

type
  Twnd_db_Main = class(TForm)
    pc: TPageControl;
    tsDevTree: TTabSheet;
    Tree: TTreeView;
    bProps: TButton;
    bRes: TButton;
    tsDevRes: TTabSheet;
    ResList: TListView;
    img: TImageList;
    bRefresh: TButton;
    procedure cmRes(Sender: TObject);
    procedure cmProps(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure cmRefresh(Sender: TObject);
  private
    Devices: TMiTeC_Devices;
  public
    procedure RefreshData;
  end;

var
  wnd_db_Main: Twnd_db_Main;

implementation

uses DetailDlg, ResourcesDlg, MiTeC_Routines, MiTeC_NTDDK;

{$R *.lfm}

procedure Twnd_db_Main.cmRefresh(Sender: TObject);
begin
  Screen.Cursor:=crHourglass;
  try
    RefreshData;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure Twnd_db_Main.cmProps(Sender: TObject);
var
  dr: TDevice;
  i: integer;
begin
  if Assigned(Tree.Selected) and (Tree.Selected.Level=2) then
    with Tdlg_db_Detail.Create(self) do begin
      lv.items.clear;
      i:=PInteger(Tree.Selected.Data)^;
      dr:=Devices.Devices[i];
      with lv.Items.Add do begin
        Caption:='Device Name';
        Subitems.Add(Tree.Selected.Text);
        ImageIndex:=-3;
      end;
      with lv.Items.Add do begin
        Caption:='Class Name';
        Subitems.Add(dr.ClassName);
      end;
      with lv.Items.Add do begin
        Caption:='Class Description';
        Subitems.Add(dr.ClassDesc);
      end;
      with lv.Items.Add do begin
        Caption:='Class GUID';
        Subitems.Add(dr.GUID);
      end;
      with lv.Items.Add do begin
        Caption:='Registry Key';
        Subitems.Add(dr.RegKey);
      end;
      with lv.Items.Add do begin
        Caption:='Manufacturer';
        Subitems.Add(dr.Manufacturer);
      end;
      with lv.Items.Add do begin
        Caption:='Hardware ID';
        Subitems.Add(dr.HardwareID);
      end;
      with lv.Items.Add do begin
        Caption:='SymbolicLink';
        Subitems.Add(dr.SymbolicLink);
      end;
      if (dr.PCINumber=-1) and (dr.DeviceNumber=-1) and (dr.FunctionNumber=-1) then
        with lv.Items.Add do begin
          Caption:='Location';
          Subitems.Add(Format('%s',[dr.Location]));
        end
      else begin
        with lv.Items.Add do begin
          Caption:='PCI Number';
          Subitems.Add(Format('%d',[dr.PCINumber]));
        end;
        with lv.Items.Add do begin
          Caption:='Device Number';
          Subitems.Add(Format('%d',[dr.DeviceNumber]));
        end;
        with lv.Items.Add do begin
          Caption:='Function Number';
          Subitems.Add(Format('%d',[dr.FunctionNumber]));
        end;
      end;
      with lv.Items.Add do begin
        Caption:='Vendor ID';
        Subitems.Add(Format('%4.4x',[dr.VendorID]));
      end;
      with lv.Items.Add do begin
        Caption:='Device ID';
        Subitems.Add(Format('%4.4x',[dr.DeviceID]));
      end;
      with lv.Items.Add do begin
        Caption:='SubSystem';
        Subitems.Add(Format('%8.8x',[dr.SubSysID]));
      end;
      with lv.Items.Add do begin
        Caption:='Revision';
        Subitems.Add(Format('%2.2x',[dr.Revision]));
      end;
      if dr.Timestamp<>0 then
        with lv.Items.Add do begin
          Caption:='Last Init';
          Subitems.Add(DateTimeToStr(dr.Timestamp));
        end;
      with lv.Items.Add do begin
        Caption:='';
        ImageIndex:=-2;
      end;
      with lv.Items.Add do begin
        Caption:='Driver Version';
        SubItems.Add(dr.DriverVersion);
        ImageIndex:=-3;
      end;
      with lv.Items.Add do begin
        Caption:='Driver Date';
        SubItems.Add(dr.DriverDate);
      end;
      with lv.Items.Add do begin
        Caption:='Driver Provider';
        SubItems.Add(dr.DriverProvider);
      end;
      with lv.Items.Add do begin
        Caption:='Image Path';
        SubItems.Add(dr.ImagePath);
      end;
      with lv.Items.Add do begin
        Caption:='';
        ImageIndex:=-2;
      end;
      with lv.Items.Add do begin
        Caption:='Service Name';
        if dr.ServiceName='' then
          SubItems.Add(dr.Service)
        else
          SubItems.Add(dr.ServiceName);
        ImageIndex:=-3;
      end;
      with lv.Items.Add do begin
        Caption:='Service Group';
        SubItems.Add(dr.ServiceGroup);
      end;
      TabSheet1.Caption:='Device Properties';
      showmodal;
      free;
    end;
end;

procedure Twnd_db_Main.cmRes(Sender: TObject);
var
  d: TDevice;
  i: Integer;
  DR: TDeviceResources;
  dn: string;
begin
  if Assigned(Tree.Selected) and (Tree.Selected.Level=2) then begin
    i:=PInteger(Tree.Selected.Data)^;
    d:=Devices.Devices[i];
    if Devices.LiveData and not(Trim(d.ResourceListKey)='') then begin
      Screen.Cursor:=crHourGlass;
      try
        GetDeviceResources(d,DR);
      finally
        Screen.Cursor:=crDefault;
      end;
      dn:=d.Name;
      ShowResourcesDlg(dn,DR);
    end else
      MessageDlg('No resource list available.',mtInformation,[mbOK],0);
  end;
end;

procedure Twnd_db_Main.FormCreate(Sender: TObject);
begin
  pc.ActivePageIndex:=0;
  Devices:=TMiTeC_Devices.Create(Self);
  cmRefresh(nil);
end;

procedure Twnd_db_Main.RefreshData;
var
  i,c,p: integer;
  r,n: TTreeNode;
  cn,dn,s: string;
  pi: PInteger;
  ldc: TDeviceClass;
  RL: TResourceList;
begin
  Devices.RefreshData;
  ldc:=dcUnknown;
  with Devices, Tree,Items do begin
    c:=DeviceCount-1;
    BeginUpdate;
    while Count>0 do begin
      if Assigned(Items[Count-1].Data) then
        FreeMem(Items[Count-1].Data);
      Delete(Items[Count-1]);
    end;
    r:=Add(nil,GetMachine);
    r.ImageIndex:=0;
    r.SelectedIndex:=r.ImageIndex;
    n:=nil;
    for i:=0 to c do begin
      if (Trim(Devices[i].ClassDesc)<>'') then
        cn:=Devices[i].ClassDesc
      else
        cn:=Devices[i].ClassName;
      if not Assigned(n) or (Devices[i].DeviceClass<>ldc) then begin
        ldc:=Devices[i].DeviceClass;
        n:=AddChild(r,cn);
        n.ImageIndex:=cDeviceImageIndex[Devices[i].DeviceClass];
        n.SelectedIndex:=n.ImageIndex;
      end;
      dn:=Devices[i].Name;
      with AddChild(n,dn) do begin
        ImageIndex:=n.ImageIndex;
        SelectedIndex:=ImageIndex;
        new(pi);
        pi^:=i;
        Data:=pi;
      end;
      n.AlphaSort;
    end;
    r.AlphaSort;
    r.Expand(False);
    EndUpdate;
  end;


  tsDevRes.TabVisible:=Devices.LiveData;
  bRes.Visible:=Devices.LiveData;
  if Devices.LiveData then
  with Devices, ResList, Items do begin
    GetResourceList(RL);
    BeginUpdate;
    try
      Clear;
      for i:=0 to High(RL) do
        with Add do begin
          Caption:=RL[i].Resource;
          SubItems.Add(ResourceShareStr(RL[i].Share));
          SubItems.Add(RL[i].Device);
          ImageIndex:=cDeviceImageIndex[RL[i].DeviceClass];
        end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure Twnd_db_Main.TreeChange(Sender: TObject; Node: TTreeNode);
var
  d: TDevice;
  i: Integer;
  DR: TDeviceResources;
  dn: string;
begin
  bRes.Enabled:=False;
  bProps.Enabled:=Assigned(Node) and (Node.Level=2);
  if Assigned(Tree.Selected) and (Tree.Selected.Level=2) then begin
    i:=PInteger(Tree.Selected.Data)^;
    d:=Devices.Devices[i];
    bRes.Enabled:=Devices.LiveData and not(Trim(d.ResourceListKey)='');
  end;
end;

procedure Twnd_db_Main.TreeDblClick(Sender: TObject);
begin
  if bRes.Enabled then
    cmRes(nil)
  else if bProps.Enabled then
    cmProps(nil);
end;

procedure Twnd_db_Main.TreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
    try Dispose(PInteger(Node.Data)) except end;
end;

end.
