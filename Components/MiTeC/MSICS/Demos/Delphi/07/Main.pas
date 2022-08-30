unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, MSI_Defs,
  StdCtrls, ExtCtrls, ComCtrls, MSI_Common, MiTeC_WbemScripting_TLB, MiTeC_WMI;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Tree: TTreeView;
    Button2: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    eMachine: TEdit;
    eUser: TEdit;
    ePwd: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure cmRefresh(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  private
  public
    procedure RefreshData;
  end;

var
  Form1: TForm1;

implementation

uses MiTeC_StrUtils;

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption:='WMI Demo - '+cCompname+' '+cVersion;
  cmRefresh(nil);
end;

procedure TForm1.cmRefresh(Sender: TObject);
begin
  Screen.Cursor:=crHourglass;
  try
    RefreshData;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.RefreshData;
var
  i,j,k: Integer;
  r,n,c: TTreeNode;
  wmiServices: ISWbemServices;
  wmi,wmi1,wmi2,wmi3: TInstances;
  s,s1,s2: string;
  v: Int64;
begin
  Screen.Cursor:=crHourglass;
  Tree.Hide;
  Tree.Items.BeginUpdate;
  Update;

  try
    Tree.Items.Clear;
    if not WMIConnect(eMachine.Text,eUser.Text,ePwd.Text,Rootnamespace,wmiServices) then
      Exit;

    WMICommand(wmiServices,'Win32_ComputerSystem',wmi1);

    r:=Tree.Items.AddChild(nil,'Computer');
    n:=Tree.Items.AddChild(r,Format('%s - %s',[GetInstancePropertyValue(wmi1,'Name'),
                                               GetInstancePropertyValue(wmi1,'UserName')]));

    WMICommand(wmiServices,'Win32_OperatingSystem',wmi);

    r:=Tree.Items.AddChild(nil,'Operaèní systém');
    n:=Tree.Items.AddChild(r,GetInstancePropertyValue(wmi,'Name'));

    WMICommand(wmiServices,'Win32_Processor',wmi);

    r:=Tree.Items.AddChild(nil,'CPU');
    for i:=0 to High(wmi) do begin
      n:=Tree.Items.AddChild(r,Format('%s - %s MHz',[GetInstancePropertyValue(wmi,'Name'),GetInstancePropertyValue(wmi,'CurrentClockSpeed')]));
      c:=Tree.Items.AddChild(n,GetInstancePropertyValue(wmi,'Description'));
    end;

    r:=Tree.Items.AddChild(nil,'Memory');
    try v:=StrToInt64(GetInstancePropertyValue(wmi1,'TotalPhysicalMemory')) except v:=0 end;
    n:=Tree.Items.AddChild(r,Format('%d MB',[v shr 20]));

    WMICommand(wmiServices,'Win32_DiskDrive',wmi);

    r:=Tree.Items.AddChild(nil,'Storage');
    for i:=0 to High(wmi) do begin
      try v:=StrToInt64(GetInstancePropertyValue(wmi,'Size',i)) except v:=0 end;
      n:=Tree.Items.AddChild(r,Format('%s - %d MB',[GetInstancePropertyValue(wmi,'Model',i),
                                                    v shr 20]));
      WMICommand(wmiServices,Format('select * from Win32_DiskPartition where DiskIndex=%s',[GetInstancePropertyValue(wmi,'Index',i)]),wmi1);
      WMICommand(wmiServices,'Win32_LogicalDiskToPartition',wmi2);
      for j:=0 to High(wmi1) do begin
        for k:=0 to High(wmi2) do begin
          s1:=GetInstancePropertyValue(wmi1,'DeviceID',j);
          s2:=GetInstancePropertyValue(wmi2,'Antecedent',k);
          if Pos(s1,s2)>0 then begin
            s:=GetInstancePropertyValue(wmi2,'Dependent',k);
            s:=Copy(s,Pos(':"',s)-1,2);
            WMICommand(wmiServices,Format('select * from Win32_LogicalDisk where DeviceId=''%s''',[s]),wmi3);
            try v:=StrToInt64(GetInstancePropertyValue(wmi,'Size',i)) except v:=0 end;
            c:=Tree.Items.AddChild(n,Format('%s [%s] - %d MB - %s',[s,GetInstancePropertyValue(wmi3,'VolumeName',0),v shr 20,GetInstancePropertyValue(wmi3,'FileSystem',0)]));
          end;
        end;
      end;
    end;

    WMICommand(wmiServices,'Win32_CDROMDrive',wmi);

    for i:=0 to High(wmi) do begin
      n:=Tree.Items.AddChild(r,GetInstancePropertyValue(wmi,'Name',i));
      c:=Tree.Items.AddChild(n,GetInstancePropertyValue(wmi,'Id',i));
    end;

    WMICommand(wmiServices,'Win32_TapeDrive',wmi);

    for i:=0 to High(wmi) do begin
      n:=Tree.Items.AddChild(r,GetInstancePropertyValue(wmi,'Name',i));
      c:=Tree.Items.AddChild(n,GetInstancePropertyValue(wmi,'Id',i));
    end;

    WMICommand(wmiServices,'Win32_VideoController',wmi);

    r:=Tree.Items.AddChild(nil,'Graphics');
    try v:=StrToInt(GetInstancePropertyValue(wmi,'AdapterRAM')) except v:=0 end;
    n:=Tree.Items.AddChild(r,Format('%s - %d MB',[GetInstancePropertyValue(wmi,'Name'),v shr 20]));

    WMICommand(wmiServices,'Win32_DesktopMonitor',wmi);

    r:=Tree.Items.AddChild(nil,'Monitor');
    for i:=0 to High(wmi) do begin
      n:=Tree.Items.AddChild(r,Format('%s - (%s x %s) px',[GetInstancePropertyValue(wmi,'Name'),
                                                           GetInstancePropertyValue(wmi,'ScreenWidth'),
                                                           GetInstancePropertyValue(wmi,'ScreenHeight')]));
    end;

    WMICommand(wmiServices,'Win32_SoundDevice',wmi);

    r:=Tree.Items.AddChild(nil,'Audio');
    for i:=0 to High(wmi) do begin
      n:=Tree.Items.AddChild(r,GetInstancePropertyValue(wmi,'Name',i));
    end;

    WMICommand(wmiServices,'Win32_NetworkAdapter',wmi);

    r:=Tree.Items.AddChild(nil,'Network');
    for i:=0 to High(wmi) do begin
      WMICommand(wmiServices,Format('select * from Win32_NetworkAdapterConfiguration where Caption=''%s''',[GetInstancePropertyValue(wmi,'Caption',i)]),wmi1);
      s1:=GetInstancePropertyValue(wmi,'MACAddress',i);
      s2:=GetInstancePropertyValue(wmi1,'IPAddress',0);
      if not SameText(s1,'NULL') and not SameText(s2,'NULL') then begin
        n:=Tree.Items.AddChild(r,GetInstancePropertyValue(wmi,'Name',i));
        c:=Tree.Items.AddChild(n,s1);
        c:=Tree.Items.AddChild(n,s2);
      end;
    end;

    WMICommand(wmiServices,'Win32_Printer',wmi);

    r:=Tree.Items.AddChild(nil,'Printers');
    for i:=0 to High(wmi) do begin
      n:=Tree.Items.AddChild(r,GetInstancePropertyValue(wmi,'Name',i));
    end;

    Tree.FullExpand;

    Tree.Items.GetFirstNode.MakeVisible;
  finally
    WMIDisconnect(wmiServices);
    Finalize(wmi);
    Finalize(wmi1);
    Finalize(wmi2);
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

end.
