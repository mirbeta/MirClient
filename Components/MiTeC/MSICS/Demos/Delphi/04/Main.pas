unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ImgList, MiTeC_USB, MSI_USB, ExtCtrls,
  MSI_Common, MSI_DeviceMonitor, MiTeC_CfgMgrSetupApi;

type
  TForm1 = class(TForm)
    Tree: TTreeView;
    sd: TSaveDialog;
    USB: TMiTeC_USB;
    DeviceMonitor: TMiTeC_DeviceMonitor;
    ilSystem: TImageList;
    Panel1: TPanel;
    bRefresh: TButton;
    bSave: TButton;
    bRemove: TButton;
    cbxAuto: TCheckBox;
    bClose: TButton;
    Splitter1: TSplitter;
    List: TListView;
    procedure DeviceMonitorDeviceConnect(Sender: TObject;
      DeviceDesc: TDeviceDesc);
    procedure FormCreate(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure bCloseClick(Sender: TObject);
    procedure cmSave(Sender: TObject);
    procedure cmRemove(Sender: TObject);
    procedure DeviceMonitorVolumeConnect(Sender: TObject; Drive: Char;
      Remote: Boolean);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
  private
    spid: TSPClassImageListData;
    function FindNode(AIndex: Integer): TTreeNode;
    procedure DisplayProps(AIndex: integer);
  public
    procedure RefreshData;
  end;

var
  Form1: TForm1;

implementation

uses MiTeC_Routines;

{$R *.dfm}

procedure TForm1.RefreshData;
var
  ii,i,j: Integer;
  s: string;
  pi: PInteger;
  r,n,c: TTreeNode;
  g: TGUID;
begin
  USB.RefreshData;
  Caption:=Format('USB Devices (%d connected)',[USB.ConnectedDevices]);
  Tree.Items.BeginUpdate;
  try
  Tree.Items.Clear;
  for i:=0 to USB.USBNodeCount-1 do
    with USB.USBNodes[i] do begin
      s:='';
      if ClassGUID.D1=0 then
        g:=GUID_DEVCLASS_USB
      else
        g:=ClassGUID;
      SetupDiGetClassImageIndex(spid,g,ii);
      case USBClass of
        usbHostController: s:=s+Format('%s %d',[ClassNames[Integer(USBClass)],USBDevice.Port]);
        usbHub: s:=s+Format('%s (%s)',[USBDevice.USBClassname,ClassNames[Integer(USBClass)]]);
        else begin
          if USBDevice.ConnectionStatus=1 then begin
            if USBClass=usbExternalHub then
              s:=s+Format('Port[%d]: %s (%s)',[USBDevice.Port,USBDevice.USBClassname,ClassNames[Integer(USBClass)]])
            else begin
              if USBDevice.Product<>'' then
                s:=s+Format('Port[%d]: %s',[USBDevice.Port,USBDevice.Product])
              else
                s:=s+Format('Port[%d]: %s',[USBDevice.Port,USBDevice.USBClassname]);
              if IsEqualGUID(g,GUID_DEVCLASS_USB) and (Length(USBDevice.Registry)>0) then begin
                g:=USBDevice.Registry[0].DeviceClassGUID;
                SetupDiGetClassImageIndex(spid,g,ii);
              end;
            end;
          end else
            s:=s+Format('Port[%d]: %s',[USBDevice.Port,ConnectionStates[USBDevice.ConnectionStatus]]);
        end;
      end;
      r:=FindNode(ParentIndex);
      new(pi);
      pi^:=i;
      n:=Tree.Items.AddChildObject(r,s,pi);
      n.ImageIndex:=ii;
      n.SelectedIndex:=n.ImageIndex;
      if Assigned(r) then
        r.Expand(False);
      r:=n;
      if (USBClass in [usbReserved..usbStorage,usbVendorSpec,usbError]) and (USBDevice.ConnectionStatus=1) then begin
        for j:=0 to High(USBDevice.Registry) do begin
          g:=USBDevice.Registry[j].DeviceClassGUID;
          SetupDiGetClassImageIndex(spid,g,ii);
          new(pi);
          pi^:=MakeWord(j,i+1);
          n:=Tree.Items.AddChildObject(r,USBDevice.Registry[j].DeviceClass,pi);
          n.ImageIndex:=ii;
          n.SelectedIndex:=n.ImageIndex;
          if (USBDevice.Registry[j].Drive<>'') and USBDevice.Registry[j].DriveConnected then begin
            new(pi);
            pi^:=MakeWord(j,i+1);
            c:=Tree.Items.AddChildObject(n,Format('Drive: %s:',[USBDevice.Registry[j].Drive]),pi);
            g:=GUID_DEVCLASS_VOLUME;
            SetupDiGetClassImageIndex(spid,g,ii);
            c.ImageIndex:=ii;
            c.SelectedIndex:=c.ImageIndex;
          end;
        end;
      end;
    end;
  finally
    Tree.Items.EndUpdate;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  spid.cbSize:=sizeof(spid);
  SetupDiGetClassImageList(spid);
  ilSystem.Handle:=spid.ImageList;
  RefreshData;
end;

procedure TForm1.bRefreshClick(Sender: TObject);
begin
  RefreshData;
  Tree.SetFocus;
end;

procedure TForm1.cmRemove(Sender: TObject);
var
  idx: Integer;
  inst: Cardinal;
begin
  if not Assigned(Tree.Selected) or not Assigned(Tree.Selected.Data) then
    Exit;
  idx:=PInteger(Tree.Selected.Data)^;
  inst:=USB.IsEjectable(USB.USBNodes[idx]);
  if (inst>0) then begin
    if (MessageDlg('Do you want to disconnect selected device?',mtConfirmation,[mbYes,mbNo],0)=mrYes) then begin
      if not USB.EjectDevice(inst) then
        MessageDlg('Selected device cannot be stopped right now because it is in use.',mtWarning,[mbOK],0)
      else
        MessageDlg('Selected device can now be safely removed from the system.',mtInformation,[mbOK],0);
      Tree.SetFocus;
    end;
  end else
    MessageDlg('Selected device cannot be stopped.',mtInformation,[mbOK],0);
end;

function TForm1.FindNode(AIndex: Integer): TTreeNode;
var
  n: TTreeNode;
begin
  Result:=nil;
  n:=Tree.Items.GetFirstNode;
  while Assigned(n) do begin
    if Assigned(n.Data) and (PInteger(n.Data)^=AIndex) then begin
      Result:=n;
      Break;
    end;
    n:=n.GetNext;
  end;
end;

procedure TForm1.TreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
    Dispose(PInteger(Node.Data));
end;

procedure TForm1.TreeChange(Sender: TObject; Node: TTreeNode);
begin
  bRemove.Enabled:=Assigned(Tree.Selected) and Assigned(Tree.Selected.Data);
  if Assigned(Tree.Selected) and Assigned(Tree.Selected.Data) then
    DisplayProps(PInteger(Tree.Selected.Data)^)
  else
    List.Items.Clear;
end;

procedure TForm1.TreeCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  n: TUSBNode;
begin
  if Assigned(Node) then begin
    if Assigned(Node.Data) and (PInteger(Node.Data)^<256) then begin
      n:=USB.USBNodes[PInteger(Node.Data)^];
      if n.USBClass in [usbReserved..usbStorage,usbVendorSpec,usbError] then begin
        if n.USBDevice.ConnectionStatus=1 then begin
          Sender.Canvas.Font.Style:=[fsBold];
          if (cdsSelected in State) and not Sender.Focused then
            Sender.Canvas.Font.Color:=clWindowText;
        end else
          Sender.Canvas.Font.Color:=clGray;
      end;
    end;
  end;
end;

procedure TForm1.bCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.cmSave(Sender: TObject);
var
  h: Boolean;
begin
  h:=True;
  if sd.Execute then
    USB.SaveToStorage(sd.FileName,h);
end;

procedure TForm1.DeviceMonitorDeviceConnect(Sender: TObject;
  DeviceDesc: TDeviceDesc);
begin
  if cbxAuto.Checked and SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_USB_DEVICE)) then
    Refreshdata;
end;

procedure TForm1.DeviceMonitorVolumeConnect(Sender: TObject; Drive: Char;
  Remote: Boolean);
begin
  if cbxAuto.Checked then
    Refreshdata;
end;

procedure TForm1.DisplayProps(AIndex: integer);

procedure AddItem(const AProperty,AValue: string);
begin
  with List.Items.Add do begin
    Caption:=AProperty;
    SubItems.Add(AValue);
  end;
end;

var
  s: string;
begin
  List.Items.BeginUpdate;
  try
    List.Items.Clear;
    if AIndex<256 then begin
      with USB.USBNodes[AIndex] do
        if (USBDevice.ConnectionStatus=1) then begin
          if USBDevice.USBClassname='' then
            AddItem('Class',ClassNames[Integer(USBClass)])
          else
            AddItem('Class',USBDevice.USBClassName);
          AddItem('Manufacturer',USBDevice.Manufacturer);
          if (USBClass in [usbReserved..usbStorage,usbVendorSpec,usbError]) then begin
            AddItem('ClassGUID',GUIDToString(ClassGUID));
            AddItem('Connection Name',ConnectionName);
            AddItem('Serial',USBDevice.Serial);
            AddItem('Power consumption',Format('%d mA',[USBDevice.MaxPower]));
            case USB.GetDevicePowerState(DeviceInstanceId,Keyname) of
              PowerDeviceUnspecified: s:='Unspecified';
              PowerDeviceD0: s:='D0';
              PowerDeviceD1: s:='D1';
              PowerDeviceD2: s:='D2';
              PowerDeviceD3: s:='D3';
            end;
            AddItem('Power state',s);
            AddItem('Specification version',Format('%d.%d',[USBDevice.MajorVersion,USBDevice.MinorVersion]));
            AddItem('Driver key',Keyname);
            AddItem('Last init',DateTimeToStr(TimeStamp));
          end;
        end;
    end else
      with USB.USBNodes[Hi(AIndex)-1] do begin
        AddItem('Class',USBDevice.Registry[Lo(AIndex)].DeviceClass);
        AddItem('Name',USBDevice.Registry[Lo(AIndex)].Name);
        AddItem('ClassGUID',GUIDToString(USBDevice.Registry[Lo(AIndex)].DeviceClassGUID));
        AddItem('Last init',DateTimeToStr(USBDevice.Registry[Lo(AIndex)].Timestamp));
      end;
  finally
    List.Items.EndUpdate;
  end;
end;

end.
