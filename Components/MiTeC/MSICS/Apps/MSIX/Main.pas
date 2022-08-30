{$INCLUDE ..\..\Compilers.inc}

unit Main;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.Messages, VCL.Graphics, VCL.Controls,
     VCL.Forms, VCL.StdCtrls, VCL.Dialogs, VCL.Menus, VCL.ExtCtrls, VCL.ComCtrls, VCL.ImgList, VCL.ButtonGroup,
     Vcl.Samples.Gauges,
     {$IFDEF RAD17PLUS}System.ImageList,System.UITypes,{$ENDIF}
     {$ELSE}
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
     Dialogs, Menus, ImgList, ComCtrls, ExtCtrls, StdCtrls, Gauges,
     {$ENDIF}
     MSI_Memory, MSI_SysMon, PrefsDlg, MSI_Defs, MSI_WIFI, System.ImageList;

const
  WM_OPENFILE = WM_USER + 1000;
  WM_MDICHILDCLOSED = WM_USER + 2000;

type
  Twnd_msi_Main = class(TForm)
    ToolbarImages: TImageList;
    sb: TStatusBar;
    Panel: TPanel;
    ClientPanel: TPanel;
    Label1: TLabel;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Viewer1: TMenuItem;
    mmWindows: TMenuItem;
    acOpen: TMenuItem;
    acLocal: TMenuItem;
    N1: TMenuItem;
    acSave: TMenuItem;
    acCloseChild: TMenuItem;
    N2: TMenuItem;
    acRefresh: TMenuItem;
    N3: TMenuItem;
    acReport: TMenuItem;
    N4: TMenuItem;
    acRun: TMenuItem;
    N6: TMenuItem;
    acPrefs: TMenuItem;
    N7: TMenuItem;
    acExit: TMenuItem;
    acOverview: TMenuItem;
    N8: TMenuItem;
    acFullExpand: TMenuItem;
    acFullCollapse: TMenuItem;
    N9: TMenuItem;
    acTextOverview: TMenuItem;
    acCascade: TMenuItem;
    acTileHor: TMenuItem;
    acTileVer: TMenuItem;
    od: TOpenDialog;
    sd: TSaveDialog;
    Label2: TLabel;
    Bevel1: TBevel;
    InfoPanel: TPanel;
    lCopy: TLabel;
    lProd: TLabel;
    Bevel3: TBevel;
    GreenPanel: TPanel;
    bScan: TButton;
    bBrowse: TButton;
    Icon: TImage;
    lMachine: TLabel;
    MemGauge: TGauge;
    lWin: TLabel;
    lSes: TLabel;
    lUser: TLabel;
    Bevel2: TBevel;
    Image1: TImage;
    Image2: TImage;
    Label3: TLabel;
    lv: TListView;
    Bevel4: TBevel;
    ilWIFI: TImageList;
    Image4: TImage;
    Label4: TLabel;
    EPPanel: TPanel;
    Label5: TLabel;
    bRunAsAdmin: TButton;
    cbxWU: TCheckBox;
    cbxEL: TCheckBox;
    cbxAD: TCheckBox;
    AppIcon: TImage;
    Image3: TImage;
    MemPanel: TPanel;
    Panel1: TPanel;
    CPUPanel: TPanel;
    CPUGauge: TGauge;
    Panel3: TPanel;
    lMSICS: TLabel;
    Logo: TImage;
    procedure acCloseChildExecute(Sender: TObject);
    procedure acLocalExecute(Sender: TObject);
    procedure acReportExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acRefreshExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure MruManItemClick(Sender: TObject; MenuItem: TMenuItem;
      const FileName: String; const Data: WideString;
      var CanDelete: Boolean);
    procedure FormShow(Sender: TObject);
    procedure SysMonInterval(Sender: TSysMonThread);
    procedure acRunExecute(Sender: TObject);
    procedure acPrefsExecute(Sender: TObject);
    procedure acOverviewExecute(Sender: TObject);
    procedure acFullExpandExecute(Sender: TObject);
    procedure acFullCollapseExecute(Sender: TObject);
    procedure acTextOverviewExecute(Sender: TObject);
    procedure AppInstancesCmdLineReceived(Sender: TObject; CmdLine: TStrings);
    procedure cbxELClick(Sender: TObject);
    procedure cbxADClick(Sender: TObject);
    procedure mdibgMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure acOpenExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure sdTypeChange(Sender: TObject);
    procedure acCascadeExecute(Sender: TObject);
    procedure acTileHorExecute(Sender: TObject);
    procedure acTileVerExecute(Sender: TObject);
    procedure mmWindowsClick(Sender: TObject);
    procedure IconClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure Viewer1Click(Sender: TObject);
    procedure bRunAsAdminClick(Sender: TObject);
    procedure cbxWUClick(Sender: TObject);
    procedure LogoClick(Sender: TObject);
    procedure Image3Click(Sender: TObject);
  private
    SysMon: TSysMonThread;
    WIFI: TMiTeC_WIFI;
  protected
    procedure WMOPENFILE(var Msg: TMessage); message WM_OPENFILE;
    procedure WMMDICHILDCLOSED(var Msg: TMessage); message WM_MDICHILDCLOSED;
    procedure WMCopyData(var Message: TWMCopyData); message WM_COPYDATA;
  public
    procedure SetControls;

    function ShowChild(AChild :TFormClass; ChildCaption: string; ImageIdx: Integer = -1; AHint: string = ''; ABringToFront: Boolean = True; AMultiInstance: boolean = False) :TForm;
    function FindChild(AChild :TFormClass; ChildCaption: string = '') :TForm;

    procedure OpenFile(AFilename: string); overload;
    procedure OpenFile(AFiles: TStrings); overload;
    procedure OpenLocal;
  end;

  {
  procedure ExpandHID(VEN,DEV,SUBSYS,REV: DWORD; var SVEN,SDEV: string);
  procedure ExpandUSB(VEN,DEV: WORD; var SDEV: string);
  procedure ExpandMON(SIG: string; var SSIG: string);
  procedure ExpandPCM(SIG: string; var SSIG: string);
  }

var
  wnd_msi_Main: Twnd_msi_Main;
  Prefs: TPrefs;

{const
  fnPCIDEVS = 'PCIDEVS.TXT';
  fnUSBDEVS = 'USBDEVS.TXT';
  fnMONDEVS = 'MONDEVS.TXT';
  fnPCMDEVS = 'PCMDEVS.TXT';}


implementation

uses {$IFDEF RAD9PLUS}
     WinAPI.ShellAPI, WinApi.ActiveX,
     {$ELSE}
     ShellAPI, ActiveX,
     {$ENDIF}
     MiTeC_Dialogs, Viewer, MiTeC_Routines, Summary, MiTeC_IPC, Codecs,
     MiTeC_SIF, MSI_SystemInfo, MSI_XML_Reports, MiTeC_SysUtils, MiTeC_CmdLine,
     UserPictureWnd, MiTeC_Windows;

{$R *.dfm}

function MDIChildProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  C: TWinControl;
begin
  Result:=0;
  C:=FindControl(hwnd);
  if Assigned(C) and (C is TForm) then begin
    case uMsg of
      WM_DESTROY: begin
        SetWindowLong(hwnd,GWL_WNDPROC,C.Tag);
        C.Tag:=0;
        PostMessage(Application.MainForm.Handle,WM_MDICHILDCLOSED,0,0);
      end;
    end;
    Result:=CallWindowProc(Pointer(C.Tag),hwnd,uMsg,wParam,lParam);
  end;
end;

function Twnd_msi_Main.FindChild(AChild: TFormClass;
  ChildCaption: string): TForm;
var
  i :integer;
begin
  Result:=nil;
  for i:=0 to mdichildcount-1 do
    if (AChild=MDIChildren[i].ClassType) and
       ((Trim(ChildCaption)='') or ((Trim(ChildCaption)<>'') and SameText(ChildCaption,MDIChildren[i].Caption))) then begin
      Result:=MDIChildren[i];
      Break;
    end;
end;

function Twnd_msi_Main.ShowChild;
var
  i :integer;
  icon: TIcon;
begin
  Icon:=TIcon.Create;
  Result:=nil;
  if not AMultiInstance then
    for i:=0 to mdichildcount-1 do
      if (AChild=MDIChildren[i].ClassType) and SameText(ChildCaption,MDIChildren[i].Caption) then begin
        result:=MDIChildren[i];
        break;
      end;
  if assigned(result) then
    result.show
  else begin
    Result:=achild.create(self);
    Result.Tag:=SetWindowLong(Result.Handle,GWL_WNDPROC,Integer(@MDIChildProc));
    Result.Caption:=ChildCaption;
    Result.Icon:=icon;
  end;
  icon.Free;
end;

procedure Twnd_msi_Main.WMCopyData(var Message: TWMCopyData);
var
  Msg: TIPCMessage;
  i: integer;
begin
  ReadIPCMessage(Message,Msg);
  if (Msg.Typ=ipcParams) then begin
    with TCommandLine.Create(Msg.StrData) do
      try
        for i:=0 to ParamList.Count-1 do
          if FileExists(ParamList[i]) then
            OpenFile(ParamList[i]);
      finally
        Free;
      end;
  end;
end;

procedure Twnd_msi_Main.WMMDICHILDCLOSED(var Msg: TMessage);
begin
  Panel.Visible:=MDIChildCount=0;
  sb.Visible:=not Panel.Visible;
end;

procedure Twnd_msi_Main.WMOPENFILE(var Msg: TMessage);
begin
  OpenFile(PChar(Msg.wParam));
end;

procedure Twnd_msi_Main.SysMonInterval(Sender: TSysMonThread);
var
  i: Integer;
  n: TListItem;
  s: string;
begin
  sb.Panels[0].Text:=Format('CPU: %d %%',[Round(Sender.CPUUsage)]);
  sb.Panels[1].Text:=Format('Mem: %d %%',[Sender.MemoryLoad]);

  if MDIChildCount=0 then begin
    CPUGauge.Progress:=Round(Sender.CPUUsage);
    MemGauge.Progress:=Sender.MemoryLoad;
    WIFI.RefreshData;
    WIFI.RefreshData;
    lv.Items.BeginUpdate;
    try
      for i:=0 to lv.Items.Count-1 do
        lv.Items[i].SubItems[0]:='0%';
      for i:=0 to WIFI.NetworkCount-1 do begin
        s:=WIFI.Networks[i].SSID;
        if (s='') then
          s:=WIFI.Networks[i].Profile;
        n:=lv.FindData(0,Pointer(WIFI.Networks[i].ID),True,True);
        if not Assigned(n) then begin
          n:=lv.Items.Add;
          n.Caption:=s;
          n.SubItems.Add(Format('%d%%',[WIFI.Networks[i].SignalQuality]));
          n.Data:=Pointer(WIFI.Networks[i].ID);
        end else begin
          n.SubItems[0]:=Format('%d%%',[WIFI.Networks[i].SignalQuality]);
        end;
        n.ImageIndex:=Integer(WIFI.Networks[i].SecurityEnabled);
        if WIFI.Networks[i].Connected then
          n.ImageIndex:=n.ImageIndex+2;
      end;
    finally
      lv.Items.EndUpdate;
    end;
  end;
end;

{
procedure ExpandHID;
var
  s: string;
  i,j: Integer;
begin
  SVEN:='';
  SDEV:='';
  with PCIDEVS do begin
    if Count=0 then
      Exit;
    if VEN<>0 then begin
      s:=Format('%4.4x=',[VEN]);
      for i:=0 to Count-1 do
        if Pos(s,Strings[i])=1 then begin
          SVEN:=ValueFromIndex[i];
          j:=i;
          Break;
        end;
    end;
    if DEV<>0 then begin
      s:=Format('%4.4x:%4.4x',[VEN,DEV]);
      for i:=j to Count-1 do
        if (Pos(s,Strings[i])=1) and (ValueFromIndex[i]<>'=') then begin
          SDEV:=ValueFromIndex[i];
          j:=i;
          Break;
        end;
    end;
    if SUBSYS<>0 then begin
      s:=Format('%4.4x:%4.4x:%4.4x:%4.4x',[VEN,DEV,LoWord(SUBSYS),HiWord(SUBSYS)]);
      for i:=j to Count-1 do
        if (Pos(s,Strings[i])=1) and (ValueFromIndex[i]<>'=') then begin
          SDEV:=ValueFromIndex[i];
          Break;
        end;
    end;
    if REV<>0 then begin
      s:=Format('%4.4x:%4.4x:%2.2x',[VEN,DEV,REV]);
      for i:=j to Count-1 do
        if (Pos(s,Strings[i])=1) and (ValueFromIndex[i]<>'=') then begin
          SDEV:=ValueFromIndex[i];
          Break;
        end;
    end;
    if REV<>0 then begin
      s:=Format('%4.4x:%4.4x:%4.4x:%4.4x:%2.2x',[VEN,DEV,LoWord(SUBSYS),HiWord(SUBSYS),REV]);
      for i:=j to Count-1 do
        if (Pos(s,Strings[i])=1) and (ValueFromIndex[i]<>'=') then begin
          SDEV:=ValueFromIndex[i];
          Break;
        end;
    end;
  end;
end;

procedure ExpandMON(SIG: string; var SSIG: string);
var
  s: string;
  i: Integer;
begin
  SSIG:='';
  s:=Uppercase(SIG);
  with MONDEVS do begin
    if Count=0 then
      Exit;
    for i:=0 to Count-1 do
      if SameText(s,Names[i]) then begin
        SSIG:=Values[Names[i]];
        Break;
      end;
  end;
end;

procedure ExpandPCM(SIG: string; var SSIG: string);
var
  s: string;
  i: Integer;
begin
  SSIG:='';
  s:=Uppercase(SIG);
  with PCMDEVS do begin
    if Count=0 then
      Exit;
    for i:=0 to Count-1 do
      if SameText(s,Names[i]) then begin
        SSIG:=Values[Names[i]];
        Break;
      end;
  end;
end;

procedure ExpandUSB(VEN, DEV: WORD; var SDEV: string);
var
  s: string;
  i: Integer;
begin
  SDEV:='';
  with USBDEVS do begin
    if Count=0 then
      Exit;
    if VEN<>0 then
      s:=Format('%4.4x',[VEN]);
    for i:=0 to Count-1 do
      if SameText(s,Names[i]) then begin
        SDEV:=Values[Names[i]];
        Break;
      end;
    if DEV<>0 then
      s:=s+Format(':%4.4x',[DEV]);
    for i:=0 to Count-1 do
      if SameText(s,Names[i]) then begin
        SDEV:=Values[Names[i]];
        Break;
      end;
  end;
end;
}

procedure Twnd_msi_Main.SetControls;
begin
  if ActiveMDIChild is Tmdi_MSI_Viewer then
    (ActiveMDIChild as Tmdi_MSI_Viewer).SetControls;
  acCloseChild.Enabled:=MDIChildCount>0;
  acSave.Enabled:=MDIChildCount>0;
  acReport.Enabled:=MDIChildCount>0;

  acOverview.Enabled:=MDIChildCount>0;
  acTextOverview.Enabled:=MDIChildCount>0;
  acFullExpand.Enabled:=MDIChildCount>0;
  acFullCollapse.Enabled:=MDIChildCount>0;
end;

procedure Twnd_msi_Main.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
begin
  for i:=MDIChildCount-1 downto 0 do
    MDIChildren[I].Close;
end;

procedure Twnd_msi_Main.FormCreate(Sender: TObject);
{var
  upp: array[0..MAX_PATH] of char;
  n: HRESULT;}
begin
  AppIcon.Picture.Icon.Handle:=Application.Icon.Handle;
  lProd.Caption:=ModuleInfo.ProductName+' '+ModuleInfo.FileVersion;
  lCopy.Caption:=ModuleInfo.Copyright;
  lMachine.Caption:=MachineName;
  if WindowsLiveID<>'' then
    lUser.Caption:=WindowsLiveID
  else
    lUser.Caption:=WindowsUser;
  lWin.Caption:=TrueWindowsName;
  lSes.Caption:=GetSessionStr(Session);
  lMSICS.Caption:=cCompName+' '+cVersion;

  {$IFDEF THEMESUPPORT}
  Panel.ParentBackground:=False;
  GreenPanel.ParentBackground:=False;
  InfoPanel.ParentBackground:=False;
  ClientPanel.ParentBackground:=False;
  {$ENDIF}

  {$IFDEF WIN64}
  Caption:=Caption+' (64-bit)';
  {$ENDIF}

  EnablePrivilege(SE_DEBUG_NAME);
  EnablePrivilege(SE_SECURITY_NAME);

  EPPanel.Visible:=((OS>=osVista) and not IsElevated) or not IsAdmin;

  WIFI:=TMiTeC_WIFI.Create(Self);
  SysMon:=TSysMonThread.Create;
  SysMon.OnInterval:=SysMonInterval;
  SysMon.Interval:=1000;
  SysMon.Suspended:=False;

  if WindowsLiveID<>'' then
    sb.panels[2].Text:=WindowsLiveID
  else
    sb.panels[2].Text:=WindowsUser;
  sb.panels[3].Text:=GetSessionStr(GetSession);
  sb.panels[4].Text:=TrueWindowsName;
  sd.InitialDir:=ExtractFilePath(Application.EXEName);

  {if Assigned(SHGetUserPicturePath) or Assigned(xpSHGetUserPicturePath) then begin
    ResetMemory(upp,SizeOf(upp));
    CoInitialize(nil);
    if (OSVIX.dwMajorVersion<6) then
      n:=xpSHGetUserPicturePath(nil,0,@upp)
    else
      n:=SHGetUserPicturePath(nil,0,@upp,MAX_PATH);
    if (n=S_OK) and FileExists(upp) then
      Icon.Picture.LoadFromFile(upp);
    CoUnInitialize;
  end;}

  if not Assigned(Icon.Picture.Graphic) and not RetrieveUserAccountPicture(Icon.Picture.Bitmap) then
    Icon.Picture.Icon.Handle:=Application.Icon.Handle
  else
    Icon.Tag:=1;
end;

procedure Twnd_msi_Main.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure Twnd_msi_Main.acFullCollapseExecute(Sender: TObject);
begin
  Tmdi_msi_Viewer(ActiveMDIChild).acFullCollapseExecute(Sender);
end;

procedure Twnd_msi_Main.acFullExpandExecute(Sender: TObject);
begin
  Tmdi_msi_Viewer(ActiveMDIChild).acFullExpandExecute(Sender);
end;

procedure Twnd_msi_Main.acRefreshExecute(Sender: TObject);
begin
  if MDIChildCount=0 then
    OpenLocal
  else
    Tmdi_MSI_Viewer(ActiveMDIChild).RefreshData;
end;

procedure Twnd_msi_Main.acReportExecute(Sender: TObject);
begin
  ShowSummaryReport;
end;

procedure Twnd_msi_Main.acRunExecute(Sender: TObject);
begin
  RunDlg(Handle,Application.Icon.Handle,'','');
end;

procedure Twnd_msi_Main.acSaveExecute(Sender: TObject);
var
  sl: TStringList;
  et: comp;
  wh: Boolean;
begin
  with Tmdi_MSI_Viewer(ActiveMDIChild) do
    if SIC.StorageFilename='' then
      sd.Filename:=MachineName+cSIFExt
    else
      sd.FileName:=SIC.StorageFilename;
  if not sd.Execute then
    Exit;
  with Tmdi_MSI_Viewer(ActiveMDIChild) do begin
    et:=GetTickCount64;
    Screen.Cursor:=crHourGlass;
    wh:=True;
    try
      case sd.FilterIndex of
        1: SIC.SaveToStorage(ChangeFileext(sd.FileName,cSIFExt),wh,1,'',CompressStream);
        2: begin
          sl:=TStringList.Create;
          try
            SystemInfo_XML_Report(SIC,sl);
            {$IFDEF UNICODE}
            sl.SaveToFile(ChangeFileext(sd.FileName,'.xml'),TEncoding.UTF8);
            {$ELSE}
            sl.SaveToFile(ChangeFileext(sd.FileName,'.xml'));
           {$ENDIF}
            SaveXSLTemplate(IncludeTrailingPathDelimiter(ExtractFilePath(sd.FileName))+XSLName);
          finally
            sl.Free;
          end;
        end;
      end;
    finally
      Screen.Cursor:=crDefault;
    end;
    Info(Format('File has been saved: %1.2f s',[(GetTickCount64-et)/1000]));
  end;
end;

procedure Twnd_msi_Main.acCascadeExecute(Sender: TObject);
begin
  Cascade;
end;

procedure Twnd_msi_Main.acCloseChildExecute(Sender: TObject);
begin
  ACtiveMDIChild.Close;
end;

procedure Twnd_msi_Main.acLocalExecute(Sender: TObject);
begin
  OpenLocal;
end;

procedure Twnd_msi_Main.OpenFile(AFilename: string);
var
  Child: TForm;
begin
  Panel.Hide;sb.Show;
  if FileExists(AFileName) then begin
    Child:=ShowChild(Tmdi_MSI_Viewer,ExtractFileName(AFileName),6,AFilename,True,True);
    with Tmdi_MSI_Viewer(Child) do begin
      try
        OpenFile(AFilename);
      except on e: exception do begin
        Error(Format('Cannot open file %s: %s',[AFilename,e.Message]));
        Close;
      end end;
    end;
  end else
    Error(Format('File %s does not exist.',[AFilename]));
end;

procedure Twnd_msi_Main.acOpenExecute(Sender: TObject);
begin
  if not od.Execute then
    Exit;
  OpenFile(od.Files);
end;

procedure Twnd_msi_Main.acOverviewExecute(Sender: TObject);
begin
  Tmdi_msi_Viewer(ActiveMDIChild).acOverviewExecute(Sender);
end;

procedure Twnd_msi_Main.acPrefsExecute(Sender: TObject);
begin
  DisplayPrefsDlg(Prefs);
  cbxWU.Checked:=Prefs.Updates;
  cbxAD.Checked:=Prefs.ActiveDirectory;
  cbxEL.Checked:=Prefs.EventLog;
end;

procedure Twnd_msi_Main.MruManItemClick(Sender: TObject;
  MenuItem: TMenuItem; const FileName: String; const Data: WideString;
  var CanDelete: Boolean);
begin
  OpenFile(FileName);
end;

procedure Twnd_msi_Main.FormShow(Sender: TObject);
var
  par :string;
  slpar :tstringlist;
  i :integer;
begin
  par:='';
  slpar:=tstringlist.create;
  for i:=1 to paramcount do begin
    par:=par+paramstr(i)+' ';
    if fileexists(par) then begin
      slpar.add(trim(par));
      par:='';
    end;
  end;
  openfile(slpar);
  slpar.free;

  {if ParamCount=0 then
    OpenLocal;}
end;

procedure Twnd_msi_Main.IconClick(Sender: TObject);
var
  p: TPoint;
begin
  if Icon.Tag=0 then
    ShellExecute(Handle,'open',cWWW,nil,nil,SW_SHOWDEFAULT)
  else begin
    p.X:=Icon.Left;
    p.Y:=Icon.Top;
    p:=GreenPanel.ClientToScreen(p);
    DisplayUserPicture(p.X,p.Y,Icon.Picture.Bitmap);
  end;
end;

procedure Twnd_msi_Main.Image3Click(Sender: TObject);
begin
  ShellExecute(Handle,'open','http://www.mitec.cz/msics.html',nil,nil,SW_SHOWDEFAULT);
end;

procedure Twnd_msi_Main.LogoClick(Sender: TObject);
begin
  ShellExecute(Handle,'open',cWWW,nil,nil,SW_SHOWDEFAULT);
end;

procedure Twnd_msi_Main.mdibgMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Abort;
end;

procedure Twnd_msi_Main.mmWindowsClick(Sender: TObject);
begin
  acCascade.Enabled:=MDIChildCount>0;
  acTileHor.Enabled:=MDIChildCount>0;
  acTileVer.Enabled:=MDIChildCount>0;
end;

procedure Twnd_msi_Main.cbxELClick(Sender: TObject);
begin
  Prefs.EventLog:=TCheckbox(Sender).Checked;
end;

procedure Twnd_msi_Main.cbxWUClick(Sender: TObject);
begin
  Prefs.Updates:=TCheckbox(Sender).Checked;
end;

procedure Twnd_msi_Main.cbxADClick(Sender: TObject);
begin
  Prefs.ActiveDirectory:=TCheckbox(Sender).Checked;
end;

procedure Twnd_msi_Main.OpenFile(AFiles: TStrings);
var
  Child: TForm;
  i: Integer;
begin
  for i:=0 to AFiles.Count-1 do
  if FileExists(AFiles[i]) then begin
    Panel.Hide;sb.Show;
    Child:=ShowChild(Tmdi_MSI_Viewer,ChangeFileExt(ExtractFileName(AFiles[i]),''),6,AFiles[i],True,True);
    with Tmdi_MSI_Viewer(Child) do begin
      try
        OpenFile(AFiles[i]);
      except on e: exception do begin
        Error(Format('Cannot open file %s.',[AFiles[i]]));
        Close;
      end end;
    end;
  end else
    Error(Format('File %s does not exist.',[AFiles[i]]));
end;

procedure Twnd_msi_Main.OpenLocal;
var
  Child: TForm;
begin
  Panel.Hide;sb.Show;
  Child:=ShowChild(Tmdi_MSI_Viewer,MachineName,5,MachineName,True,True);
  with Tmdi_MSI_Viewer(Child) do
    OpenLocal;
end;

procedure Twnd_msi_Main.sdTypeChange(Sender: TObject);
var
  s: string;
begin
  case sd.FilterIndex of
    1: s:=ChangeFileExt(sd.FileName,cSIFExt);
    2: s:=ChangeFileExt(sd.FileName,'.xml');
  end;
  {SendMessage(Windows.GetParent(sd.Handle),
               CDM_SETCONTROLTEXT,
               1152,
               Integer(PChar(s)));}
end;

procedure Twnd_msi_Main.acTextOverviewExecute(Sender: TObject);
begin
  Tmdi_msi_Viewer(ActiveMDIChild).acTextOverviewExecute(Sender);
end;

procedure Twnd_msi_Main.acTileHorExecute(Sender: TObject);
begin
  TileMode:=tbHorizontal;
  Tile;
end;

procedure Twnd_msi_Main.acTileVerExecute(Sender: TObject);
begin
  TileMode:=tbVertical;
  Tile;
end;

procedure Twnd_msi_Main.AppInstancesCmdLineReceived(Sender: TObject;
  CmdLine: TStrings);
begin
  OpenFile(CmdLine);
end;

procedure Twnd_msi_Main.bRunAsAdminClick(Sender: TObject);
begin
  if RunAsAdmin(Handle,Application.ExeName,'') then
    Close;
end;

procedure Twnd_msi_Main.FormActivate(Sender: TObject);
begin
  Panel.Visible:=MDIChildCount=0;
  sb.Visible:=not Panel.Visible;
end;

procedure Twnd_msi_Main.File1Click(Sender: TObject);
begin
  SetControls;
end;

procedure Twnd_msi_Main.Viewer1Click(Sender: TObject);
begin
  SetControls;
end;

end.
