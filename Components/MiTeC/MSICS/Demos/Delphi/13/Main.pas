unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, MSI_Common, MSI_Devices, ImgList, MiTeC_CfgMgrSetupApi;

type
  Twnd_db_Main = class(TForm)
    pc: TPageControl;
    tsDevTree: TTabSheet;
    Tree: TTreeView;
    bProps: TButton;
    bRes: TButton;
    tsDevRes: TTabSheet;
    ResList: TListView;
    bRefresh: TButton;
    ilSystem: TImageList;
    procedure cmRes(Sender: TObject);
    procedure cmProps(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure cmRefresh(Sender: TObject);
  private
    Devices: TMiTeC_Devices;
    spid: TSPClassImageListData;
  public
    procedure RefreshData;
  end;

var
  wnd_db_Main: Twnd_db_Main;

implementation

uses DetailDlg, ResourcesDlg, MiTeC_Routines, MiTeC_NTDDK, MiTeC_Datetime;

{$R *.dfm}

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
  i,ii: integer;
  h: HICON;
  g: TGUID;
begin
  if Assigned(Tree.Selected) and (Tree.Selected.Level=2) then
    with Tdlg_db_Detail.Create(self) do begin
      lv.items.clear;
      i:=PInteger(Tree.Selected.Data)^;
      dr:=Devices.Devices[i];
      g:=dr.ClassGUID;
      SetupDiLoadClassIcon(g,h,ii);
      Icon.Picture.Icon.Handle:=h;
      eName.Text:=dr.Name;
      with lv.Items.Add do begin
        Caption:='Class Name';
        Subitems.Add(dr.ClassName);
        ImageIndex:=-3;
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
      with lv.Items.Add do begin
        Caption:='Location';
        Subitems.Add(Format('%s',[dr.Location]));
      end;
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
      with lv.Items.Add do begin
        Caption:='';
        ImageIndex:=-2;
      end;
      with lv.Items.Add do begin
        Caption:='Driver Description';
        SubItems.Add(dr.Driver);
        ImageIndex:=-3;
      end;
      with lv.Items.Add do begin
        Caption:='Driver Version';
        SubItems.Add(dr.DriverVersion);
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
        Caption:='Driver GUID';
        SubItems.Add(dr.DriverKey);
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
      with lv.Items.Add do begin
        Caption:='';
        ImageIndex:=-2;
      end;
      with lv.Items.Add do begin
        Caption:='Install ID';
        SubItems.Add(dr.InstallID);
        ImageIndex:=-3;
      end;
      with lv.Items.Add do begin
        Caption:='Install Date';
        SubItems.Add(DateTimeToStrDef(dr.InstallDate));
      end;
      with lv.Items.Add do begin
        Caption:='First Install Date';
        SubItems.Add(DateTimeToStrDef(dr.FirstInstallDate));
      end;
      with lv.Items.Add do begin
        Caption:='Last Arrival Date';
        SubItems.Add(DateTimeToStrDef(dr.LastArrivalDate));
      end;
      with lv.Items.Add do begin
        Caption:='Last Removal Date';
        SubItems.Add(DateTimeToStrDef(dr.LastRemovalDate));
      end;
      showmodal;
      DestroyIcon(h);
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
  spid.cbSize:=sizeof(spid);
  SetupDiGetClassImageList(spid);
  ilSystem.Handle:=spid.ImageList;
  pc.ActivePageIndex:=0;
  Devices:=TMiTeC_Devices.Create(Self);
  cmRefresh(nil);
end;

procedure Twnd_db_Main.RefreshData;
var
  i,c,ii: integer;
  r,n: TTreeNode;
  lcn,cn,dn: string;
  pi: PInteger;
  RL: TResourceList;
  ilh: THandle;
  g: TGUID;
begin
  Devices.RefreshData;
  lcn:='';
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
      if not Assigned(n) or not SameText(Devices[i].ClassName,lcn) then begin
        lcn:=Devices[i].ClassName;
        n:=AddChild(r,cn);
        g:=Devices[i].ClassGUID;
        SetupDiGetClassImageIndex(spid,g,ii);
        n.ImageIndex:=ii;
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
          g:=RL[i].DeviceClassGUID;
          SetupDiGetClassImageIndex(spid,g,ii);
          ImageIndex:=ii;
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
  if bProps.Enabled then
    cmProps(nil);
end;

procedure Twnd_db_Main.TreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
    try Dispose(PInteger(Node.Data)) except end;
end;

end.
