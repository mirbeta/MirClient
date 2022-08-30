{.$DEFINE DLM}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, Menus,
  MSI_ProcListMon, MSI_ProcMon, MSI_SvcListMon, MSI_SysModListMon;

type
  Twnd_Main = class(TForm)
    PrcList: TListView;
    pc: TPageControl;
    tsPrc: TTabSheet;
    tsDrv: TTabSheet;
    Button2: TButton;
    DrvList: TListView;
    tsSvc: TTabSheet;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    mmRefresh: TMenuItem;
    N1: TMenuItem;
    mmExit: TMenuItem;
    Bevel1: TBevel;
    mmProcess: TMenuItem;
    mmPrcDetails: TMenuItem;
    mmPrcProps: TMenuItem;
    N3: TMenuItem;
    mmPrcKill: TMenuItem;
    mmDriver: TMenuItem;
    mmDrvProps: TMenuItem;
    mmService: TMenuItem;
    mmSvcProps: TMenuItem;
    mSvcDetails: TMenuItem;
    N4: TMenuItem;
    mmSvcStart: TMenuItem;
    mmSvcStop: TMenuItem;
    mmSvcPause: TMenuItem;
    mmSvcResume: TMenuItem;
    mmAbout: TMenuItem;
    SvcList: TListView;
    Button5: TButton;
    Button6: TButton;
    cbSvc: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure ListCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure ListAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure cmRefresh(Sender: TObject);
    procedure cmExit(Sender: TObject);
    procedure cmPrcDetails(Sender: TObject);
    procedure pcChange(Sender: TObject);
    procedure mmAboutClick(Sender: TObject);
    procedure cmPrcKill(Sender: TObject);
    procedure cmSvcDetails(Sender: TObject);
    procedure cmSvcStart(Sender: TObject);
    procedure cmSvcStop(Sender: TObject);
    procedure cmSvcPause(Sender: TObject);
    procedure cmSvcResume(Sender: TObject);
    procedure cmDrvDetails(Sender: TObject);
    procedure ProcMonThreadInterval(Sender: TProcMonThread);
    procedure ProcListMonThreadInterval(Sender: TProcListMonThread);
    procedure SvcListMonThreadInterval(Sender: TSvcListMonThread);
    procedure SysModListMonThreadInterval(Sender: TSysModListMonThread);
    procedure cbSvcChange(Sender: TObject);
  private
    sl: TStringList;
    FPLM: TProcListMonThread;
    FSLM: TSvcListMonThread;
    FDLM: TSysModListMonThread;
    FPM: TProcMonThread;
  public
    procedure RefreshProcList(Sender: TProcListMonThread);
    procedure RefreshSvcList(Sender: TSvcListMonThread);
    procedure RefreshDrvList(Sender: TSysModListMonThread);
  end;

var
  wnd_Main: Twnd_Main;

implementation

uses ShellAPI, MiTeC_CtrlRtns, MiTeC_Datetime, MiTeC_Dialogs, PrcDetails,
  MiTeC_Routines, MiTeC_AdvAPI, SvcDetails, SMDetails, WinSvc, MSI_Defs, MiTeC_Windows;

{$R *.dfm}

procedure Twnd_Main.FormCreate(Sender: TObject);
begin
  EnablePrivilege(SE_DEBUG_NAME);
  EnablePrivilege(SE_SECURITY_NAME);
  sl:=TStringList.Create;
  FPLM:=TProcListMonThread.Create;
  FPLM.OnInterval:=ProcListMonThreadInterval;
  FPLM.AutoSuspend:=True;
  FSLM:=TSvcListMonThread.Create;
  FSLM.Types:=stAll;
  FSLM.AutoSuspend:=True;
  FSLM.OnInterval:=SvcListMonThreadInterval;
  {$IFDEF DLM}
  FDLM:=TSysModListMonThread.Create;
  FDLM.OnInterval:=SysModListMonThreadInterval;
  tsDrv.TabVisible:=True;
  {$ENDIF}
  FPM:=TProcMonThread.Create(GetCurrentProcessId);
  FPM.OnInterval:=ProcMonThreadInterval;
  cmRefresh(nil);
  pc.ActivePageIndex:=0;
end;

procedure Twnd_Main.RefreshDrvList(Sender: TSysModListMonThread);
var
  i: Integer;
  r: TSystemModuleRecord;
begin
  DrvList.Items.BeginUpdate;
  try
    DrvList.Items.Clear;
    for i:=0 to Sender.RecordCount-1 do begin
      Sender.GetRecord(i,r);
      with DrvList.Items.Add do begin
        Caption:=ExtractFilename(r.Name);
        SubItems.Add(Format('0x%x',[r.Base]));
        SubItems.Add(Format('%d',[r.LoadCount]));
        SubItems.Add(r.VersionInfo.Description);
        SubItems.Add(r.Name);
      end;
    end;
    if DrvList.Tag<>0 then
      DrvList.AlphaSort;
  finally
    DrvList.Items.EndUpdate;
  end;
end;

procedure Twnd_Main.RefreshProcList(Sender: TProcListMonThread);
var
  i: Integer;
  r: TProcessRecord;
begin
  PrcList.Items.BeginUpdate;
  try
    PrcList.Items.Clear;
    for i:=0 to Sender.RecordCount-1 do begin
      Sender.GetRecord(i,r);
      with PrcList.Items.Add do begin
        Caption:=r.Name;
        if Is64 and (r.Bits<>64) then
          Caption:=Caption+'*32';
        SubItems.Add(Format('%d',[r.PID]));
        SubItems.Add(FormatTicks(r.CPUTimes.UserTime.QuadPart+r.CPUTimes.KernelTime.QuadPart,False));
        SubItems.Add(Format('%d KB',[r.VMCounters.WorkingSetSize div 1024]));
        SubItems.Add(Format('%s',[r.CommandLine]));
      end;
    end;
    if PrcList.Tag<>0 then
      PrcList.AlphaSort;
  finally
    PrcList.Items.EndUpdate;
  end;
end;

procedure Twnd_Main.RefreshSvcList(Sender: TSvcListMonThread);
var
  i: Integer;
  r: TServiceRecord;
begin
  SvcList.Items.BeginUpdate;
  try
    SvcList.Items.Clear;
    for i:=0 to Sender.RecordCount-1 do begin
      Sender.GetRecord(i,r);
      with SvcList.Items.Add do begin
        Caption:=r.DisplayName;
        if r.Typ=svcUnknown then
          SubItems.Add(Format('%s (%d)',[cSvcType[r.Typ],r._Typ]))
        else
          SubItems.Add(cSvcType[r.Typ]);
        SubItems.Add(cSvcStatus[r.Status]);
        SubItems.Add(cSvcStartup[r.StartUp]);
        SubItems.Add(r.ObjectName);
        SubItems.Add(r.Name);
      end;
    end;
    if SvcList.Tag<>0 then
      SvcList.AlphaSort;
  finally
    SvcList.Items.EndUpdate;
  end;
end;

procedure Twnd_Main.SvcListMonThreadInterval(Sender: TSvcListMonThread);
begin
  tsSvc.Caption:=Format('Services (%d)',[Sender.RecordCount]);
  Update;
  Screen.Cursor:=crHourGlass;
  try
    RefreshSvcList(Sender);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure Twnd_Main.ListCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
  Compare:=ListView_CustomSort(Item1,Item2,abs(TListView(Sender).Tag)-1);
  if TListView(Sender).Tag<0 then
    Compare:=-Compare;
end;

procedure Twnd_Main.ListColumnClick(Sender: TObject; Column: TListColumn);
begin
  TListView(Sender).SortType:=stNone;
  if Column.Index+1<>abs(TListView(Sender).Tag) then
    TListView(Sender).Tag:=Column.Index+1
  else
    TListView(Sender).Tag:=-TListView(Sender).Tag;
  TListView(Sender).SortType:=stText;
end;

procedure Twnd_Main.ListAdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  if Abs(TListView(Sender).Tag)=1 then
    Sender.Canvas.Brush.Color:=clInfoBk
  else
    Sender.Canvas.Brush.Color:=clWhite
end;

procedure Twnd_Main.ListAdvancedCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if Abs(TListView(Sender).Tag)=SubItem+1 then
    Sender.Canvas.Brush.Color:=clInfoBk
  else
    Sender.Canvas.Brush.Color:=clWhite
end;

procedure Twnd_Main.cmRefresh(Sender: TObject);
begin
  FPLM.Suspended:=False;
  FSLM.Suspended:=False;
  {$IFDEF DLM}
  FDLM.Suspended:=False;
  {$ENDIF}
end;

procedure Twnd_Main.cmExit(Sender: TObject);
begin
  Close;
end;

procedure Twnd_Main.cmPrcDetails(Sender: TObject);
begin
  if not Assigned(PrcList.Selected) then
    Exit;

  Screen.Cursor:=crHourglass;
  FPM.ProcessID:=StrToInt(PrcList.Selected.SubItems[0]);
  FPM.Suspended:=False;
end;

procedure Twnd_Main.pcChange(Sender: TObject);
begin
  mmProcess.Visible:=pc.ActivePage.pageIndex=0;
  mmService.Visible:=pc.ActivePage.pageIndex=1;
  mmDriver.Visible:=pc.ActivePage.pageIndex=2;
end;

procedure Twnd_Main.ProcListMonThreadInterval(Sender: TProcListMonThread);
begin
  tsPrc.Caption:=Format('Processes (%d)',[Sender.RecordCount]);
  Update;
  Screen.Cursor:=crHourGlass;
  try
    RefreshProcList(Sender);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure Twnd_Main.ProcMonThreadInterval(Sender: TProcMonThread);
var
  r: TProcessRecord;
begin
  Sender.Suspended:=True;
  FPLM.GetChildProcesses(Sender.ProcessID,sl);
  FPLM.GetRecordByPID(Sender.ProcessID,r);
  ShowPrcDetails(r,Sender,sl);
end;

procedure Twnd_Main.mmAboutClick(Sender: TObject);
begin
  ShellAbout(Handle,PChar(Application.Title+' '+cVersion),PChar(Format('%s (%s)',[cCopyright,cWWW]))  ,Application.Icon.Handle);
end;

procedure Twnd_Main.cmPrcKill(Sender: TObject);
var
  pid: Cardinal;
begin
  if not Assigned(PrcList.Selected) then
    Exit;

  pid:=StrToInt(PrcList.Selected.Caption);
  if (pid>9) then
    if WarnYesNo('WARNING: Terminating a process can cause undesired '#13#10+
                  'results including loss of data and system instability. The '#13#10+
                  'process will not be given the chance to save its state or '#13#10+
                  'data before it is terminated. Are you sure you want to '#13#10+
                  'terminate the process?') then begin
      if KillProcess(pid)=tsError then
        Warn('Cannot terminate this process.')
      else
        FPLM.Suspended:=False;
    end;
end;

procedure Twnd_Main.cmSvcDetails(Sender: TObject);
var
  r: TServiceRecord;
begin
  if not Assigned(SvcList.Selected) then
    Exit;

  FSLM.GetRecordByName(SvcList.Selected.SubItems[SvcList.Selected.SubItems.Count-1],r);
  ShowSvcDetails(r,FSLM);
end;

procedure Twnd_Main.cmSvcStart(Sender: TObject);
var
  r: TServiceRecord;
  s: string;
begin
  if not Assigned(SvcList.Selected) then
    Exit;

  FSLM.GetRecordByName(SvcList.Selected.SubItems[SvcList.Selected.SubItems.Count-1],r);
  if (r.Status=SERVICE_STOPPED) and
     YesNo(Format('Start %s service?',[r.DisplayName])) and
     InputQuery(r.DisplayName,'Parameters',s) then begin
    if ServiceStart(r.Name,s) then
      FSLM.Suspended:=False
    else
      Warn('Cannot start service.');
  end;
end;

procedure Twnd_Main.cmSvcStop(Sender: TObject);
var
  r: TServiceRecord;
begin
  if not Assigned(SvcList.Selected) then
    Exit;

  FSLM.GetRecordByName(SvcList.Selected.SubItems[SvcList.Selected.SubItems.Count-1],r);

  if (r.ControlsAccepted and SERVICE_ACCEPT_STOP>0) and (r.Status in [SERVICE_RUNNING,SERVICE_PAUSED]) and YesNo(Format('Stop %s service?',[r.DisplayName])) then begin
    ServiceGetDependants('',r.Name,True,False,sl);
    if ServiceStop(r.Name,sl,True) then
      FSLM.Suspended:=False
    else
      Warn('Cannot stop service.');
  end;
end;

procedure Twnd_Main.SysModListMonThreadInterval(Sender: TSysModListMonThread);
begin
  Sender.Suspended:=True;
  tsDrv.Caption:=Format('Drivers (%d)',[Sender.RecordCount]);
  Update;
  Screen.Cursor:=crHourGlass;
  try
    RefreshDrvList(Sender);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure Twnd_Main.cmSvcPause(Sender: TObject);
var
  r: TServiceRecord;
begin
  if not Assigned(SvcList.Selected) then
    Exit;

  FSLM.GetRecordByName(SvcList.Selected.SubItems[SvcList.Selected.SubItems.Count-1],r);
  if (r.Status=SERVICE_RUNNING) and YesNo(Format('Pause %s service?',[r.DisplayName])) then begin
    if ServicePause(r.Name) then
      FSLM.Suspended:=False
    else
      Warn('Cannot pause service.');
  end;
end;

procedure Twnd_Main.cmSvcResume(Sender: TObject);
var
  r: TServiceRecord;
begin
  if not Assigned(SvcList.Selected) then
    Exit;

  FSLM.GetRecordByName(SvcList.Selected.SubItems[SvcList.Selected.SubItems.Count-1],r);
  if (r.Status=SERVICE_PAUSED) and YesNo(Format('Resume %s service?',[r.DisplayName])) then begin
    if ServiceContinue(r.Name) then
      FSLM.Suspended:=False
    else
      Warn('Cannot resume service.');
  end;
end;

procedure Twnd_Main.cbSvcChange(Sender: TObject);
begin
  case cbSvc.ItemIndex of
    0: FSLM.Types:=stServices;
    1: FSLM.Types:=stDrivers;
    2: FSLM.Types:=stAll;
  end;
  FSLM.Suspended:=False;
end;

procedure Twnd_Main.cmDrvDetails(Sender: TObject);
var
  r: TSystemModuleRecord;
begin
  if not Assigned(DrvList.Selected) then
    Exit;

  FDLM.GetRecordByName(DrvList.Selected.SubItems[DrvList.Selected.SubItems.Count-1],r);
  ShowDrvDetails(r);
end;

end.
