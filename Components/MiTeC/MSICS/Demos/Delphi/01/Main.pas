unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, MSI_Common, MSI_Monitor, MSI_Defs,
  MSI_USB, MSI_Storage, MSI_Printers, MSI_Media, MSI_Network, MSI_Display,
  MSI_Memory, MSI_CPU, MSI_OS, MSI_Machine, MSI_Devices, MSI_SystemInfo, MSI_APM;

const
  WM_REFRESH = WM_USER+1;

type
  TForm1 = class(TForm)
    Tree: TTreeView;
    od: TOpenDialog;
    sd: TSaveDialog;
    SI: TMiTeC_SystemInfo;
    ButtonPanel: TPanel;
    Button1: TButton;
    Button2: TButton;
    bSave: TButton;
    bLoad: TButton;
    Label1: TLabel;
    procedure bLoadClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmRefresh(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FormShow(Sender: TObject);
  private
  protected
    procedure WMREFRESH(var AMsg: TMessage); message WM_REFRESH;
  public
    procedure RefreshData;
  end;

var
  Form1: TForm1;

implementation

uses MiTeC_USB, MiTeC_Routines;

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption:=SI._About;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  PostMessage(Handle,WM_REFRESH,0,0);
end;

procedure TForm1.cmRefresh(Sender: TObject);
begin
  Screen.Cursor:=crHourglass;
  try
    Tree.Hide;
    ButtonPanel.Hide;
    Update;
    SI.OS.DetectUpdatesAndHotfixes:=False;
    SI.RefreshData;
    RefreshData;
  finally
    ButtonPanel.Show;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.bLoadClick(Sender: TObject);
var
  h: Boolean;
begin
  if not od.execute then
    Exit;
  h:=True;
  SI.LoadFromStorage(od.FileName,h);
  RefreshData;
end;

procedure TForm1.bSaveClick(Sender: TObject);
var
  h: Boolean;
begin
  if not sd.execute then
    Exit;
  h:=True;
  SI.SaveToStorage(sd.FileName,h);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.RefreshData;
var
  i,j: Integer;
  r,n,c: TTreeNode;
  di: TDiskInfo;
  s: string;
begin
  Screen.Cursor:=crHourglass;
  Tree.Items.BeginUpdate;
  Update;
  try
    Tree.Items.Clear;

    r:=Tree.Items.AddChild(nil,'Computer');
    n:=Tree.Items.AddChild(r,Format('%s - %s',[SI.Machine.MachineName,SI.Machine.User]));
    c:=Tree.Items.AddChild(n,Format('System: %s',[SI.Machine.Computer]));
    c:=Tree.Items.AddChild(n,Format('Model: %s',[Trim(Format('%s %s',[Trim(SI.Machine.SMBIOS.SystemManufacturer),
                                                                   Trim(SI.Machine.SMBIOS.SystemModel)]))]));
    r:=Tree.Items.AddChild(nil,'Operating system');
    s:=Format('%s %s',[SI.OS.OSName,SI.OS.OSEdition]);
    n:=Tree.Items.AddChild(r,s);

    r:=Tree.Items.AddChild(nil,'CPU');
    for i:=0 to SI.CPU.CPUPhysicalCount-1 do begin
      n:=Tree.Items.AddChild(r,Format('%s %s - %d MHz',[cVendorNames[SI.CPU.Vendor].Prefix,SI.CPU.CPUName,SI.CPU.Frequency]));
      with SI.CPU.Cache do
        c:=Tree.Items.AddChild(n,Format('Cache: %d x %d KB L1 + %d x %d KB L2 + %d x %d KB L3',[Level1.Code.SharedWays,Level1.Code.Size+Level1.Data.Size,Level2.SharedWays,Level2.Size,Level3.SharedWays,Level3.Size]));
      c:=Tree.Items.AddChild(n,Format('Core number: %d',[SI.CPU.CoreCount]));
      c:=Tree.Items.AddChild(n,Format('Thread number: %d',[SI.CPU.ThreadCount]));
    end;

    r:=Tree.Items.AddChild(nil,'Memory');
    n:=Tree.Items.AddChild(r,Format('%d MB',[SI.Memory.PhysicalTotal shr 20]));

    r:=Tree.Items.AddChild(nil,'Storage');
    for i:=0 to SI.Storage.PhysicalCount-1 do begin
      n:=Tree.Items.AddChild(r,Format('%s - %d MB',[SI.Storage.Physical[i].Model,SI.Storage.Physical[i].Size shr 20]));
      for j:=0 to SI.Storage.LogicalCount-1 do
        if SI.Storage.Logical[j].PhysicalIndex=i then begin
          di:=GetDiskInfo(SI.Storage.Logical[j].Drive+':');
          c:=Tree.Items.AddChild(n,Format('%s: [%s] - %s (%d MB)',[SI.Storage.Logical[j].Drive,di.Volumelabel,di.FileSystem,di.Capacity shr 20]));
        end;
    end;

    r:=Tree.Items.AddChild(nil,'USB');
    for i:=0 to SI.USB.USBNodeCount-1 do begin
      if (SI.USB.USBNodes[i].USBDevice.ConnectionStatus=1) and
         not(SI.USB.USBNodes[i].USBClass in [usbHostController,usbHub,usbExternalHub]) then begin
        n:=Tree.Items.AddChild(r,Trim(Format('%s %s (%d.%d) - %s (Port %d)',[SI.USB.USBNodes[i].USBDevice.Manufacturer,
                                                                             SI.USB.USBNodes[i].USBDevice.Product,
                                                                             SI.USB.USBNodes[i].USBDevice.MajorVersion,
                                                                             SI.USB.USBNodes[i].USBDevice.MinorVersion,
                                                                             ClassNames[Integer(SI.USB.USBNodes[i].USBClass)],
                                                                             SI.USB.USBNodes[i].USBDevice.Port])));
      end;
    end;

    r:=Tree.Items.AddChild(nil,'Graphics');
    for i:=0 to SI.Display.AdapterCount-1 do begin
      n:=Tree.Items.AddChild(r,Format('%s - %d MB',[SI.Display.Adapter[i].Name,SI.Display.Adapter[i].Memory shr 20]));
    end;


    r:=Tree.Items.AddChild(nil,'Monitor');
    for i:=0 to SI.Monitor.Count-1 do begin
      s:=SI.Monitor.Monitors[i].EDID.Name;
      if s='' then
        s:=SI.Monitor.Monitors[i].DeviceDescription;
      n:=Tree.Items.AddChild(r,Format('%s',[s]));
      c:=Tree.Items.AddChild(n,Format('(%dx%d)cm',[SI.Monitor.Monitors[i].EDID.Width,SI.Monitor.Monitors[i].EDID.Height]));
    end;

    r:=Tree.Items.AddChild(nil,'Audio');
    if SI.Media.SoundCardName<>'' then
      n:=Tree.Items.AddChild(r,SI.Media.SoundCardName)
    else
      with SI.Media do
        if (WAVEOut.Count>0) then
          n:=Tree.Items.AddChild(r,WaveOut[0]);

    r:=Tree.Items.AddChild(nil,'Network');
    if SI.Network.TCPIP.AdapterCount>0 then
      for i:=0 to SI.Network.TCPIP.AdapterCount-1 do begin
        n:=Tree.Items.AddChild(r,Format('%s - %s (%s)',[SI.Network.TCPIP.Adapter[i].Name,SI.Network.TCPIP.Adapter[i].Address,SI.Network.TCPIP.Adapter[i].IPAddress.CommaText]));
      end
    else
      for i:=0 to SI.Network.PhysicalAdapters.Count-1 do begin
        n:=Tree.Items.AddChild(r,Format('%s - %s (%s)',[SI.Network.PhysicalAdapters[i],SI.Network.MACAddresses[i],SI.Network.IPAddresses[i]]));
      end;

    r:=Tree.Items.AddChild(nil,'Printers');
    for i:=0 to SI.Printers.PrinterCount-1 do begin
      n:=Tree.Items.AddChild(r,Format('%s - %s',[SI.Printers.Driver[i],SI.Printers.Port[i]]));
    end;

    Tree.FullExpand;

    Tree.Items.GetFirstNode.MakeVisible;
  finally
    Tree.Items.EndUpdate;
    Tree.Show;
    Screen.Cursor:=crdefault;
  end;
end;

procedure TForm1.TreeCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Assigned(Node) then begin
    case Node.Level of
      0: Sender.Canvas.Font.Style:=[fsBold,fsUnderline];
      2: Sender.Canvas.Font.Color:=clGray;
    end;
  end;
end;

procedure TForm1.WMREFRESH(var AMsg: TMessage);
begin
  cmRefresh(nil);
end;

end.
