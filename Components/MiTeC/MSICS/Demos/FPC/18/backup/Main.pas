{$mode delphi}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, MiTeC_NativeDefs, MSI_ProcListMon, MSI_ProcMon;

type

  { TwndMain }

  TwndMain = class(TForm)
    HandleList: TListView;
    InfoPanel: TPanel;
    lCPU: TLabel;
    lCT: TLabel;
    lIORead: TLabel;
    lIOWrite: TLabel;
    List: TListView;
    lMem: TLabel;
    lPri: TLabel;
    lTitle: TLabel;
    ModList: TListView;
    Panel2: TPanel;
    pc: TPageControl;
    sb: TStatusBar;
    ListPanel: TPanel;
    eSearch: TEdit;
    ThreadList: TListView;
    tsHandles: TTabSheet;
    tsModules: TTabSheet;
    tsThreads: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure eSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ProcMonThreadInterval(Sender: TProcMonThread);
    procedure ProcListMonThreadInterval(Sender: TProcListMonThread);
  private
    FPLM: TProcListMonThread;
    FPM: TProcMonThread;
    FHL: TList;
    procedure RefreshProcList;
    procedure FindProcess(const AText: string);
    procedure RefreshThreadList;
    procedure RefreshModList;
    procedure RefreshHandleList;
  public
  end;

var
  wndMain: TwndMain;

implementation

uses MiTeC_StrUtils, MiTeC_Routines, MiTeC_NativeAPI, MiTeC_Datetime, Math;

{$R *.lfm}

{ TwndMain }

procedure TwndMain.eSearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift=[]) and (Key=vk_return) then
    FindProcess(eSearch.Text);
end;

procedure TwndMain.FindProcess(const AText: string);
var
  n: TListItem;
begin
  if Trim(AText)='' then
    Exit;
  for n in List.Items do
    if PosText(AText,n.SubItems[2])>0 then begin
      List.Selected:=n;
      List.Selected.MakeVisible(False);
      Break;
    end;
end;

procedure TwndMain.FormCreate(Sender: TObject);
begin
  EnablePrivilege('SeDebugPrivilege');
  FHL:=TList.Create;
  FPLM:=TProcListMonThread.Create;
  FPLM.OnInterval:=ProcListMonThreadInterval;
  FPM:=TProcMonThread.Create(GetCurrentProcessId);
  FPM.OnInterval:=ProcMonThreadInterval;

  FPLM.Suspended:=False;
  pc.ActivePageIndex:=0;
end;

procedure TwndMain.ListSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if not Selected or not Assigned(Item) then
    Exit;
  FPM.ProcessID:=StrToInt(Item.Caption);
  FPM.Suspended:=False;
  InfoPanel.Show;
end;

procedure TwndMain.ProcListMonThreadInterval(Sender: TProcListMonThread);
var
  p: TProcessRecord;
begin
  RefreshProcList;
  Sender.GetRecordByPID(Sender.MaxCPUUsageProcessID,p);
  sb.Panels[1].Text:=Format('Max CPU usage: %s (%d)',[p.Name,p.PID]);
end;

procedure TwndMain.ProcMonThreadInterval(Sender: TProcMonThread);
begin
  lCPU.Caption:=Format('CPU Usage: %1.2f %%',[Sender.CPUUsage]);
  lTitle.Caption:=Format('[%d] %s (%d-bit)',[Sender.ProcessID,Sender.ImageName,Sender.Bits]);
  lCT.Caption:=Format('Created: %s',[DateTimeToStr(Sender.CreateTime)]);
  lPri.Caption:=Format('Priority: %d',[Sender.Priority]);
  lMem.Caption:=Format('Memory: %s',[NormalizeDataValue(Sender.WorkingSetSize/1)]);
  lIORead.Caption:=Format('IO Read Rate: %s/s (Total: %s)',[NormalizeDataValue(Sender.CurrentIORead),NormalizeDataValue(Sender.TotalIORead/1)]);
  lIOWrite.Caption:=Format('IO Write Rate: %s/s (Total: %s)',[NormalizeDataValue(Sender.CurrentIOWrite),NormalizeDataValue(Sender.TotalIOWrite/1)]);
  tsHandles.Caption:=Format('Handles (%d/%d)',[HandleList.Items.Count, Sender.HandleCount]);
  tsModules.Caption:=Format('Modules (%d)',[Sender.ModuleCount]);
  RefreshThreadList;
  if tsModules.TabVisible and ((pc.ActivePage=tsModules) or (ModList.Items.Count=0)) then
    RefreshModList;
  if (pc.ActivePage=tsHandles) or (HandleList.Items.Count=0) then begin
    Sender.RefreshHandles(Sender.ProcessID,Sender.ProcessHandle,FHL);
    RefreshHandleList;
  end;
end;

procedure TwndMain.RefreshHandleList;
var
  hid: THandle;
  c,vp,i: Integer;
  SInfo: TScrollInfo;
  r: TRect;
  h: THandleRecord;
begin
  hid:=0;
  if Assigned(HandleList.Selected) then
    hid:=StrToInt('$'+Copy(HandleList.Selected.Caption,3,Length(HandleList.Selected.Caption)));
  SInfo.cbSize:=SizeOf(SInfo);
  SInfo.fMask:=SIF_ALL;
  GetScrollInfo(HandleList.Handle,SB_VERT,SInfo);
  vp:=SInfo.nPos;
  HandleList.Items.BeginUpdate;
  try
    HandleList.Items.Clear;
    c:=FHL.Count;
    for i:=0 to c-1 do begin
      h:=PHandleRecord(FHL[i])^;
      with HandleList.Items.Add do begin
        Caption:=Format('0x%x',[h.Handle]);
        SubItems.Add(h.TypeName);
        Subitems.Add(h.Name);
      end;
    end;
  finally
    HandleList.Items.EndUpdate;
  end;
  if HandleList.Items.Count>0 then begin
    HandleList.Items.BeginUpdate;
    try
      r:=HandleList.Items[0].DisplayRect(drBounds);
      HandleList.ScrollBy_WS(0,vp*(r.Bottom-r.Top));
      if hid<>0 then
        HandleList.Selected:=HandleList.FindCaption(0,Format('0x%x',[hid]),False,True,True);
    finally
      HandleList.Items.EndUpdate;
    end;
  end;
end;

procedure TwndMain.RefreshModList;
var
  mid: string;
  c,vp,i: Integer;
  SInfo: TScrollInfo;
  r: TRect;
  m: TModuleRecord;
begin
  mid:='';
  if Assigned(ModList.Selected) then
    mid:=ModList.Selected.Caption;
  SInfo.cbSize:=SizeOf(SInfo);
  SInfo.fMask:=SIF_ALL;
  GetScrollInfo(ModList.Handle,SB_VERT,SInfo);
  vp:=SInfo.nPos;
  ModList.Items.BeginUpdate;
  try
    ModList.Items.Clear;
    c:=FPM.ModuleCount;
    for i:=0 to c-1 do begin
      FPM.GetModuleRecord(i,m);
      with ModList.Items.Add do begin
        Caption:=m.Name;
        SubItems.Add(Format('0x%8.8x',[m.BaseAddress]));
        Subitems.Add(Format('%d KB',[m.ImageSize shr 10]));
        SubItems.Add(m.VersionInfo.Description);
      end;
    end;
  finally
    ModList.Items.EndUpdate;
  end;
  if ModList.Items.Count>0 then begin
    ModList.Items.BeginUpdate;
    try
      r:=ModList.Items[0].DisplayRect(drBounds);
      ModList.ScrollBy_WS(0,vp*(r.Bottom-r.Top));
      if mid<>'' then
        ModList.Selected:=ModList.FindCaption(0,mid,False,True,True);
    finally
      ModList.Items.EndUpdate;
    end;
  end;
end;

procedure TwndMain.RefreshProcList;
var
  i,c: Integer;
  vp: Integer;
  SInfo: TScrollInfo;
  r: TRect;
  p: TProcessRecord;
begin
  SInfo.cbSize:=SizeOf(SInfo);
  SInfo.fMask:=SIF_ALL;
  GetScrollInfo(List.Handle,SB_VERT,SInfo);
  vp:=SInfo.nPos;
  List.Items.BeginUpdate;
  try
    List.Items.Clear;
    c:=FPLM.RecordCount;
    for i:=0 to c-1 do begin
      FPLM.GetRecord(i,p);
      with List.Items.Add do begin
        Caption:=IntToStr(p.PID);
        SubItems.Add(IntToStr(p.SessionID));
        SubItems.Add(IntToStr(p.Bits));
        if CompareValue(p.Performance.CPUUsage,0,0.01)>0 then
          SubItems.Add(Format('%1.2f',[p.Performance.CPUUsage]))
        else
          SubItems.Add('');
        SubItems.Add(NormalizeDataValue(p.VMCounters.WorkingSetSize/1));
        SubItems.Add(p.Name);
        if p.DomainName<>'' then
          SubItems.Add(p.DomainName+'\'+p.UserName)
        else
          SubItems.Add(p.UserName);
      end;
    end;
  finally
    List.Items.EndUpdate;
  end;
  if List.Items.Count>0 then begin
    List.Items.BeginUpdate;
    try
      r:=List.Items[0].DisplayRect(drBounds);
      List.ScrollBy_WS(0,vp*(r.Bottom-r.Top));
      if not FPM.Suspended then begin
        List.OnSelectItem:=nil;
        try
          List.Selected:=List.FindCaption(0,IntToStr(FPM.ProcessID),False,True,True);
        finally
          List.OnSelectItem:=ListSelectItem;
        end;
      end;
    finally
      List.Items.EndUpdate;
    end;
  end;
  sb.Panels[0].Text:=Format('%d item(s)',[List.Items.Count]);
end;

procedure TwndMain.RefreshThreadList;
var
  tid: int64;
  c,vp,i: Integer;
  SInfo: TScrollInfo;
  r: TRect;
  t: TThreadRecord;
begin
  tid:=-1;
  if Assigned(ThreadList.Selected) then
    tid:=StrToInt(ThreadList.Selected.Caption);
  SInfo.cbSize:=SizeOf(SInfo);
  SInfo.fMask:=SIF_ALL;
  GetScrollInfo(ThreadList.Handle,SB_VERT,SInfo);
  vp:=SInfo.nPos;
  ThreadList.Items.BeginUpdate;
  try
    ThreadList.Items.Clear;
    c:=FPM.ThreadCount;
    for i:=0 to c-1 do begin
      FPM.GetThreadRecord(i,t);
      with ThreadList.Items.Add do begin
        Caption:=IntToStr(t.ID);
        SubItems.Add(Format('%s:%s',[cThreadState[TThreadState(t.State)],cKWaitReason[TKWaitReason(t.WaitReason)]]));
        if CompareValue(t.CPUUsage,0,0.01)>0 then
          SubItems.Add(Format('%1.2f',[t.CPUUsage]))
        else
          Subitems.Add('');
        SubItems.Add(Format('%d / %d',[t.Priority,t.BasePriority]));
        SubItems.Add(t.StartAddressString);
      end;
    end;
  finally
    ThreadList.Items.EndUpdate;
  end;
  if ThreadList.Items.Count>0 then begin
    ThreadList.Items.BeginUpdate;
    try
      r:=ThreadList.Items[0].DisplayRect(drBounds);
      ThreadList.ScrollBy_WS(0,vp*(r.Bottom-r.Top));
      if tid<>-1 then
        ThreadList.Selected:=ThreadList.FindCaption(0,IntToStr(tid),False,True,True);
    finally
      ThreadList.Items.EndUpdate;
    end;
  end;
  tsThreads.Caption:=Format('Threads (%d)',[ThreadList.Items.Count]);
end;

end.
