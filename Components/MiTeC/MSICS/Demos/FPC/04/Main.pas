unit Main;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ImgList, MiTeC_USB, MSI_USB, ExtCtrls,
  MSI_Common, MSI_DeviceMonitor;

type
  TForm1 = class(TForm)
    bRefresh: TButton;
    Tree: TTreeView;
    ilUSB: TImageList;
    bClose: TButton;
    Bevel1: TBevel;
    bSave: TButton;
    sd: TSaveDialog;
    USB: TMiTeC_USB;
    DeviceMonitor: TMiTeC_DeviceMonitor;
    cbxAuto: TCheckBox;
    bRemove: TButton;
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
    function FindNode(AIndex: Integer): TTreeNode;
  public
    procedure RefreshData;
  end;

var
  Form1: TForm1;

implementation

uses MiTeC_Routines;

{$R *.lfm}

procedure TForm1.RefreshData;
var
  ii,i,j: Integer;
  s: string;
  pi: PInteger;
  r,n,c,d: TTreeNode;
begin
  USB.RefreshData;
  Caption:=Format('USB Devices (%d connected)',[USB.ConnectedDevices]);
  Tree.Items.BeginUpdate;
  try
  Tree.Items.Clear;
  for i:=0 to USB.USBNodeCount-1 do
    with USB.USBNodes[i] do begin
      s:='';
      ii:=Integer(USBClass);
      case USBClass of
        usbHostController: s:=s+Format('%s %d (%s)',[ClassNames[Integer(USBClass)],USBDevice.Port,USB.USBNodes[i].ConnectionName]);
        usbHub: s:=s+Format('%s (%s)',[ClassNames[Integer(USBClass)],USB.USBNodes[i].ConnectionName]);
        else begin
          if USBDevice.ConnectionStatus=1 then begin
            if USBClass=usbExternalHub then
              s:=s+Format('Port[%d]: %s (%s)',[USBDevice.Port,ClassNames[Integer(USBClass)],USB.USBNodes[i].ConnectionName])
            else
              if USBDevice.Product<>'' then
                s:=s+Format('Port[%d]: %s (%s)',[USBDevice.Port,USBDevice.Product,USB.USBNodes[i].ConnectionName])
              else
                s:=s+Format('Port[%d]: %s (%s)',[USBDevice.Port,USBDevice.USBClassname,USB.USBNodes[i].ConnectionName]);
          end else begin
            s:=s+Format('Port[%d]: %s',[USBDevice.Port,ConnectionStates[USBDevice.ConnectionStatus]]);
            ii:=13;
          end;
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
        ii:=15;
        if USBDevice.USBClassname='' then
          n:=Tree.Items.AddChild(r,Format('Class: %s',[ClassNames[Integer(USBClass)]]))
        else
          n:=Tree.Items.AddChild(r,Format('Class: %s',[USBDevice.USBClassName]));
        n.ImageIndex:=ii;
        n.SelectedIndex:=n.ImageIndex;
        n:=Tree.Items.AddChild(r,Format('Manufacturer: %s',[USBDevice.Manufacturer]));
        n.ImageIndex:=ii;
        n.SelectedIndex:=n.ImageIndex;
        n:=Tree.Items.AddChild(r,Format('Serial: %s',[USBDevice.Serial]));
        n.ImageIndex:=ii;
        n.SelectedIndex:=n.ImageIndex;
        n:=Tree.Items.AddChild(r,Format('Power consumption: %d mA',[USBDevice.MaxPower]));
        n.ImageIndex:=ii;
        n.SelectedIndex:=n.ImageIndex;
        n:=Tree.Items.AddChild(r,Format('Specification version: %d.%d',[USBDevice.MajorVersion,USBDevice.MinorVersion]));
        n.ImageIndex:=ii;
        n.SelectedIndex:=n.ImageIndex;
        n:=Tree.Items.AddChild(r,Format('Driver key: %s',[USB.USBNodes[i].Keyname]));
        n.ImageIndex:=ii;
        n.SelectedIndex:=n.ImageIndex;
        n:=Tree.Items.AddChild(r,Format('Last init: %s',[DateTimeToStr(USB.USBNodes[i].TimeStamp)]));
        n.ImageIndex:=ii;
        n.SelectedIndex:=n.ImageIndex;
        if Length(USBdevice.Registry)>0 then begin
          n:=Tree.Items.AddChild(r,'Featured devices');
          n.ImageIndex:=0;
          n.SelectedIndex:=n.ImageIndex;
          for j:=0 to High(USBDevice.Registry) do begin
            c:=Tree.Items.AddChild(n,Format('Class: %s',[USBDevice.Registry[j].DeviceClass]));
            c.ImageIndex:=Integer(USBClass);
            c.SelectedIndex:=c.ImageIndex;
            d:=Tree.Items.AddChild(c,Format('Name: %s',[USBDevice.Registry[j].Name]));
            d.ImageIndex:=ii;
            d.SelectedIndex:=d.ImageIndex;
            d:=Tree.Items.AddChild(c,Format('Last init: %s',[DateTimeToStr(USBDevice.Registry[j].Timestamp)]));
            d.ImageIndex:=ii;
            d.SelectedIndex:=d.ImageIndex;
            if (USBDevice.Registry[j].Drive<>'') and USBDevice.Registry[j].DriveConnected then begin
              d:=Tree.Items.AddChild(c,Format('Drive: %s:',[USBDevice.Registry[j].Drive]));
              d.ImageIndex:=ii;
              d.SelectedIndex:=c.ImageIndex;
            end;
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
end;

procedure TForm1.TreeCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  n: TUSBNode;
begin
  Sender.Canvas.Font.Color:=clWindowText;
  Sender.Canvas.Font.Style:=[];
  if Assigned(Node) then begin
    if Assigned(Node.data) then begin
      n:=USB.USBNodes[PInteger(Node.Data)^];
      if n.USBClass in [usbReserved..usbStorage,usbVendorSpec,usbError] then begin
        if n.USBDevice.ConnectionStatus=1 then begin
          Sender.Canvas.Font.Style:=[fsBold];
          if (cdsSelected in State) and not Sender.Focused then
            Sender.Canvas.Font.Color:=clWindowText;
        end else
          Sender.Canvas.Font.Color:=clGray;
      end;
    end else begin
      if Node.ImageIndex=15 then
        Sender.Canvas.Font.Color:=clNavy;
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

end.
