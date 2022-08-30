unit Main;

{$MODE Delphi}

interface

uses Windows, SysUtils, Classes, Forms, Controls, ComCtrls, StdCtrls,
  MSI_SysMon, MSI_ProcMon, ExtCtrls;


type
  TScanThread = class(TThread)
  private
    FList: TStringList;
    FPath: string;
    FStart: Int64;
    procedure DoSync;
  protected
    procedure Execute; override;
  public
    constructor Create(const APath: string);
    destructor Destroy; override;

    property Path: string read FPath;
  end;

  TwndMain = class(TForm)
    Memo: TMemo;
    bStart: TButton;
    sb: TStatusBar;
    List: TListView;
    CPUPanel: TPanel;
    CPUGauge: TProgressbar;
    CPUPanelTitle: TPanel;
    Panel3: TPanel;
    MemGauge: TProgressbar;
    Panel4: TPanel;
    CoreList: TListView;
    procedure FormCreate(Sender: TObject);
    procedure SysMonThreadInterval(Sender: TSysMonThread);
    procedure ProcMonThreadInterval(Sender: TProcMonThread);
    procedure bStartClick(Sender: TObject);
  private
    FSysMon: TSysMonThread;
    FProcMon: TProcMonThread;
    t1, t2, t3: TScanThread;
    procedure RefreshThreadList;
  public
  end;

var
  wndMain: TwndMain;

implementation

uses MiTeC_Routines, MiTeC_NativeDefs, MiTeC_NativeAPI;

{$R *.lfm}

function GetFileVersion(const AFileName: string): string;
var
  h,c,n: Cardinal;
  Buf: pointer;
  FixedFileInfo: PVSFixedFileInfo;
begin
  n:=GetFileVersioninfoSize(PChar(AFileName),h);
  Result:='';
  if n<>0 then begin
    GetMem(Buf,n);
    try
      if GetFileVersionInfo(PChar(AFileName),h,n,Buf) then begin
        VerQueryValue(Buf,'\',Pointer(FixedFileInfo),c);
        Result:=IntToStr(FixedFileInfo.dwFileVersionMS div $10000)+'.'+
                IntToStr(FixedFileInfo.dwFileVersionMS and $0FFFF)+'.'+
                IntToStr(FixedFileInfo.dwFileVersionLS div $10000)+'.'+
                IntToStr(FixedFileInfo.dwFileVersionLS and $0FFFF);
      end;
    finally
      FreeMem(Buf);
    end;
  end;
end;

procedure GetFiles(Directory: string; Filenames: TStringlist);
var
  h: THandle;
  FindData: TWin32FindData;
  s: string;
begin
  Directory:=IncludeTrailingPathDelimiter(Directory);
  h:=FindFirstFile(PChar(Directory+'*.*'),FindData);
  if h<>INVALID_HANDLE_VALUE then begin
    repeat
      if (StrComp(FindData.cFileName,'.')<>0) and (StrComp(FindData.cFileName, '..')<>0) then begin
        if FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY=0 then begin
          if (Pos(ExtractFileExt(LowerCase(FindData.cFileName)),'.exe.dll')>0) then
             s:=GetFileVersion(Directory+FindData.cFileName)
           else
             s:='';
          Filenames.Add(Directory+FindData.cFileName+'='+s);
        end else
          GetFiles(Directory+FindData.cFileName,Filenames);
      end;
    until not FindNextFile(h,FindData);
    Windows.FindClose(h);
  end;
end;

procedure TwndMain.bStartClick(Sender: TObject);
begin
  bStart.Enabled:=False;

  t1:=TScanThread.Create(GetSysDir);
  FProcMon.SetThreadText(t1.ThreadID,TScanThread.ClassName+': '+t1.Path);
  t2:=TScanThread.Create('c:\program files');
  FProcMon.SetThreadText(t2.ThreadID,TScanThread.ClassName+': '+t2.Path);
  t3:=TScanThread.Create(GetTempDir);
  FProcMon.SetThreadText(t3.ThreadID,TScanThread.ClassName+': '+t3.Path);

  while (WaitForSingleObject(t1.Handle,0)=WAIT_TIMEOUT) or
        (WaitForSingleObject(t2.Handle,0)=WAIT_TIMEOUT) or
        (WaitForSingleObject(t3.Handle,0)=WAIT_TIMEOUT) do
    Application.ProcessMessages;

  Memo.Lines.Add('');
  bStart.Enabled:=True;
end;

procedure TwndMain.SysMonThreadInterval(Sender: TSysMonThread);
var
  i: Byte;
begin
  sb.Panels[0].Text:=Format('CPU: %d%%',[Round(Sender.CPUUsage)]);
  sb.Panels[1].Text:=Format('Mem: %d%%',[Sender.MemoryLoad]);
  CPUGauge.Position:=Round(Sender.CPUUsage);
  MemGauge.Position:=Sender.MemoryLoad;
  for i:=0 to FSysMon.CPUCoreCount-1 do begin
    CoreList.Items[i].SubItems[0]:=Format('%d MHz',[FSysMon.CPUCoreMaxFreq[i]]);
    CoreList.Items[i].SubItems[1]:=Format('%d MHz',[FSysMon.CPUCoreFreq[i]]);
  end;
end;

procedure TwndMain.FormCreate(Sender: TObject);
var
  i: byte;
begin
  FSysMon:=TSysMonThread.Create;
  FSysMon.Interval:=500;
  FSysMon.OnInterval:=SysMonThreadInterval;
  FSysMon.Suspended:=False;

  FProcMon:=TProcMonThread.Create(GetCurrentProcessID);
  FProcMon.Interval:=1000;
  FProcMon.OnInterval:=ProcMonThreadInterval;
  FProcMon.Suspended:=False;

  for i:=0 to FSysMon.CPUCoreCount-1 do
    with CoreList.Items.Add do begin
      Caption:=Format('#%d',[i]);
      SubItems.Add(Format('%d MHz',[FSysMon.CPUCoreMaxFreq[i]]));
      SubItems.Add(Format('%d MHz',[FSysMon.CPUCoreFreq[i]]));
    end;

  FProcMon.SetThreadText(MainThreadID,'Main thread');
end;

procedure TwndMain.ProcMonThreadInterval(Sender: TProcMonThread);
begin
  sb.Panels[2].Text:=Format('Proc CPU: %1.2f%%',[Sender.CPUUsage]);
  sb.Panels[3].Text:=Format('Proc Mem: %d MB',[Sender.WorkingSetSize shr 20]);
  RefreshThreadList;
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
  if Assigned(List.Selected) then
    tid:=StrToInt(List.Selected.Caption);
  SInfo.cbSize:=SizeOf(SInfo);
  SInfo.fMask:=SIF_ALL;
  GetScrollInfo(List.Handle,SB_VERT,SInfo);
  vp:=SInfo.nPos;
  List.Items.BeginUpdate;
  try
    List.Items.Clear;
    c:=FProcMon.ThreadCount;
    for i:=0 to c-1 do begin
      FProcMon.GetThreadRecord(i,t);
      with List.Items.Add do begin
        Caption:=IntToStr(t.ID);
        SubItems.Add(DateTimeToStr(t.CreateTime));
        SubItems.Add(Format('%s:%s',[cThreadState[TThreadState(t.State)],cKWaitReason[TKWaitReason(t.WaitReason)]]));
        if t.CPUUsage>0 then
          SubItems.Add(Format('%1.2f',[t.CPUUsage]))
        else
          SubItems.Add('');
        SubItems.Add(Format('%d / %d',[t.Priority,t.BasePriority]));
        SubItems.Add(t.StartAddressString);
        SubItems.Add(t.Text);
      end;
    end;
  finally
    List.Items.EndUpdate;
  end;
  if List.Items.Count>0 then begin
    List.Items.BeginUpdate;
    try
      r:=List.Items[0].DisplayRect(drBounds);
      List.ScrollBy(0,vp*(r.Bottom-r.Top));
      if tid<>-1 then
        List.Selected:=List.FindCaption(0,IntToStr(tid),False,True,True);
    finally
      List.Items.EndUpdate;
    end;
  end;
  sb.Panels[4].Text:=Format('%d thread(s)',[List.Items.Count]);
end;

{ TScanThread }

constructor TScanThread.Create(const APath: string);
begin
  inherited Create(False);
  FList:=TStringList.Create;
  FPath:=APath;
  FreeOnTerminate:=True;
end;

destructor TScanThread.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TScanThread.DoSync;
begin
  wndMain.Memo.Lines.Add(Format('[%s]: %d ms - %d files',[FPath,GetTickCount64-FStart,FList.Count]));
end;

procedure TScanThread.Execute;
begin
  FStart:=GetTickCount64;
  if DirectoryExists(FPath) then
    GetFiles(FPath,FList);
  Synchronize(DoSync);
  FList.Clear;
end;

end.
