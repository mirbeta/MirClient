unit PrcDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ImgList, MSI_ProcMon, MSI_ProcListMon;

type
  Tdlg_PrcDetails = class(TForm)
    Bevel1: TBevel;
    bClose: TButton;
    pc: TPageControl;
    TabSheet2: TTabSheet;
    ModList: TListView;
    TabSheet1: TTabSheet;
    GenList: TListView;
    imgIcon: TImage;
    TabSheet3: TTabSheet;
    CPList: TListView;
    TabSheet4: TTabSheet;
    CntList: TListView;
    TabSheet5: TTabSheet;
    ThdList: TListView;
    TabSheet6: TTabSheet;
    HList: TListView;
    TabSheet7: TTabSheet;
    WinTree: TTreeView;
    ImageList: TImageList;
    Button5: TButton;
    eName: TEdit;
    TabSheet8: TTabSheet;
    SecList: TListView;
    tsEnv: TTabSheet;
    EnvBox: TListBox;
    procedure ListAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure ListAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure ListColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure GenListAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure GenListAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure WinTreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure cmWinDetails(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMon: TProcMonThread;
    FRecord: TProcessRecord;
    FCP: TStringList;
    FML,FHL,FWL: TList;
  public
    procedure RefreshData;
  end;

procedure ShowPrcDetails(ARecord: TProcessRecord; AMonitor: TProcMonThread; AChildProcs: TStringlist);

var
  dlg_PrcDetails: Tdlg_PrcDetails;

implementation

uses
  MiTeC_CtrlRtns, MiTeC_Datetime, MiTeC_Routines, MiTeC_Dialogs, MiTeC_NativeDefs,
  MiTeC_NativeAPI, WinDetails, MiTeC_SysUtils;

{$R *.dfm}

procedure ShowPrcDetails;
begin
  with Tdlg_PrcDetails.Create(Application.Mainform) do
    try
      FMon:=AMonitor;
      FRecord:=ARecord;
      FCP:=AChildProcs;

      FMon.RefreshModules(FMon.ProcessID,FML);
      FMon.RefreshHandles(FMon.ProcessID,FMon.ProcessHandle,FHL);
      FMon.RefreshWindows(FMon.ProcessID,False,FWL);

      RefreshData;

      Screen.Cursor:=crDefault;
      ShowModal;
    finally
      Free;
    end;
end;

procedure Tdlg_PrcDetails.RefreshData;
var
  i,j,c: Integer;
  r,n: TTreeNode;
  VersionInfo: TVersionInfo;
  m: TModuleRecord;
  h: THandleRecord;
  t: TThreadRecord;
  w: TWindowRecord;
begin
  eName.Text:=FRecord.Name;
  imgIcon.Picture.Icon.Handle:=GetFileIcon(FRecord.ImageName);
  GetFileVerInfo(FRecord.ImageName,VersionInfo);

  GenList.Items.Clear;
  with GenList.Items.Add do begin
    Caption:='Description';
    SubItems.Add(VersionInfo.Description);
    ImageIndex:=-3;
  end;
  with GenList.Items.Add do begin
    Caption:='Version';
    SubItems.Add(VersionInfo.FileVersion);
  end;
  with GenList.Items.Add do begin
    Caption:='Product Name';
    SubItems.Add(VersionInfo.ProductName);
  end;
  with GenList.Items.Add do begin
    Caption:='Company Name';
    SubItems.Add(VersionInfo.CompanyName);
  end;
  with GenList.Items.Add do begin
    Caption:='';
    ImageIndex:=-2;
  end;
  with GenList.Items.Add do begin
    Caption:='PID';
    SubItems.Add(Format('%d',[FRecord.PID]));
    ImageIndex:=-3;
  end;
  with GenList.Items.Add do begin
    Caption:='Parent PID';
    SubItems.Add(Format('%d',[FRecord.ParentPID]))
  end;
  with GenList.Items.Add do begin
    Caption:='Image name';
    SubItems.Add(FRecord.ImageName);
  end;
  with GenList.Items.Add do begin
    Caption:='Command line';
    SubItems.Add(FRecord.CommandLine);
  end;
  with GenList.Items.Add do begin
    Caption:='';
    ImageIndex:=-2;
  end;
  if Win32Platform=VER_PLATFORM_WIN32_NT then
    with GenList.Items.Add do begin
      Caption:='User Name';
      SubItems.Add(FRecord.UserName);
      ImageIndex:=-3;
    end;
  with GenList.Items.Add do begin
    Caption:='Priority';
    SubItems.Add(Format('%d',[FRecord.Priority]));
    ImageIndex:=-3;
  end;
  with GenList.Items.Add do begin
    Caption:='Thread Count';
    SubItems.Add(Format('%d',[FRecord.ThreadCount]));
  end;
  with GenList.Items.Add do begin
    Caption:='Handle Count';
    SubItems.Add(Format('%d',[FRecord.HandleCount]));
  end;

  c:=FML.Count;
  TabSheet2.Caption:=Format(' Modules (%d) ',[c]);
  ModList.Items.Clear;
  for i:=0 to c-1 do begin
    m:=PModuleRecord(FML[i])^;
    with ModList.Items.Add do begin
      Caption:=m.Name;
      SubItems.Add(Format('%d',[m.ImageSize]));
      SubItems.Add(Format('0x%x',[m.BaseAddress]));
      SubItems.Add(m.VersionInfo.Description);
    end;
  end;

  c:=FMon.ThreadCount;
  TabSheet5.Caption:=Format(' Threads (%d) ',[c]);
  ThdList.Items.Clear;
  for i:=0 to c-1 do begin
    FMon.GetThreadRecord(i,t);
    with ThdList.Items.Add do begin
      Caption:=Format('%d',[t.ID]);
      Subitems.Add(Format('0x%x',[t.StartAddress]));
      Subitems.Add(Format('%d',[t.BasePriority]));
      Subitems.Add(Format('%d',[t.Priority]));
      if TThreadState(t.State)<>StateWait then
        SubItems.Add(cThreadState[TThreadState(t.State)])
      else
        SubItems.Add(Format('%s:%s',[cThreadState[TThreadState(t.State)],cKWaitReason[TKWaitReason(t.WaitReason)]]));
      Subitems.Add(Format('%d',[t.ContextSwitchCount]));
      SubItems.Add(FormatTicks(t.KernelTime));
      SubItems.Add(FormatTicks(t.UserTime));
    end;
  end;

  TabSheet3.Caption:=Format(' Child Procs (%d) ',[FCP.Count]);
  CPList.Items.Clear;
  for i:=0 to FCP.Count-1 do
    with CPList.Items.Add do begin
      Caption:=FCP.Names[i];
      SubItems.Add(FCP.ValueFromIndex[i]);
    end;

  CntList.Items.Clear;
  with CntList.Items.Add do begin
    Caption:='CPU Times';
    ImageIndex:=-3;
  end;
  with CntList.Items.Add do begin
    Caption:='Kernel Time';
    SubItems.Add(FormatTicks(FRecord.CPUTimes.KernelTime.QuadPart));
    SubItems.Add('-');
  end;
  with CntList.Items.Add do begin
    Caption:='User Time';
    SubItems.Add(FormatTicks(FRecord.CPUTimes.UserTime.QuadPart));
    SubItems.Add('-');
  end;
  with CntList.Items.Add do begin
    Caption:='Total Time';
    SubItems.Add(FormatTicks(FRecord.CPUTimes.UserTime.QuadPart+FRecord.CPUTimes.KernelTime.QuadPart));
    SubItems.Add('-');
  end;
  with CntList.Items.Add do begin
    Caption:='';
    ImageIndex:=-2;
  end;
  with CntList.Items.Add do begin
    Caption:='I/O Counters';
    ImageIndex:=-3;
  end;
  with CntList.Items.Add do begin
    Caption:='Read Operation Count';
    SubItems.Add(Format('%d',[FRecord.IOCounters.ReadOperationCount.QuadPart]));
    SubItems.Add('-');
  end;
  with CntList.Items.Add do begin
    Caption:='Write Operation Count';
    SubItems.Add(Format('%d',[FRecord.IOCounters.WriteOperationCount.QuadPart]));
    SubItems.Add('-');
  end;
  with CntList.Items.Add do begin
    Caption:='Other Operation Count';
    SubItems.Add(Format('%d',[FRecord.IOCounters.OtherOperationCount.QuadPart]));
    SubItems.Add('-');
  end;
  with CntList.Items.Add do begin
    Caption:='Read Transfer Count';
    SubItems.Add(Format('%d',[FRecord.IOCounters.ReadTransferCount.QuadPart]));
    SubItems.Add('-');
  end;
  with CntList.Items.Add do begin
    Caption:='Write Transfer Count';
    SubItems.Add(Format('%d',[FRecord.IOCounters.WriteTransferCount.QuadPart]));
    SubItems.Add('-');
  end;
  with CntList.Items.Add do begin
    Caption:='Other Transfer Count';
    SubItems.Add(Format('%d',[FRecord.IOCounters.OtherTransferCount.QuadPart]));
    SubItems.Add('-');
  end;
  with CntList.Items.Add do begin
    Caption:='';
    ImageIndex:=-2;
  end;
  with CntList.Items.Add do begin
    Caption:='Virtual Memory Counters';
    ImageIndex:=-3;
  end;
  with CntList.Items.Add do begin
    Caption:='Page Fault Count';
    SubItems.Add(Format('%d',[FRecord.VMCounters.PageFaultCount]));
    SubItems.Add('-');
  end;
  with CntList.Items.Add do begin
    Caption:='Virtual Size';
    SubItems.Add(Format('%d B',[FRecord.VMCounters.VirtualSize]));
    SubItems.Add(Format('%d B',[FRecord.VMCounters.PeakVirtualSize]));
  end;
  with CntList.Items.Add do begin
    Caption:='Working Set Size';
    SubItems.Add(Format('%d B',[FRecord.VMCounters.WorkingSetSize]));
    SubItems.Add(Format('%d B',[FRecord.VMCounters.PeakWorkingSetSize]));
  end;
  with CntList.Items.Add do begin
    Caption:='Quota Peak Paged Pool Usage';
    SubItems.Add(Format('%d B',[FRecord.VMCounters.QuotaPagedPoolUsage]));
    SubItems.Add(Format('%d B',[FRecord.VMCounters.QuotaPeakPagedPoolUsage]));
  end;
  with CntList.Items.Add do begin
    Caption:='Quota Peak Non Paged Pool Usage';
    SubItems.Add(Format('%d B',[FRecord.VMCounters.QuotaNonPagedPoolUsage]));
    SubItems.Add(Format('%d B',[FRecord.VMCounters.QuotaPeakNonPagedPoolUsage]));
  end;
  with CntList.Items.Add do begin
    Caption:='Pagefile Usage';
    SubItems.Add(Format('%d B',[FRecord.VMCounters.PagefileUsage]));
    SubItems.Add(Format('%d B',[FRecord.VMCounters.PeakPagefileUsage]));
  end;

  SecList.Items.Clear;
  with SecList.Items.Add do begin
    Caption:='Groups';
    ImageIndex:=-3;
  end;
  for i:=0 to High(FRecord.Groups) do
    with SecList.Items.Add do begin
      if FRecord.Groups[i].Name='' then
        Caption:=FRecord.Groups[i].SID
      else
        if FRecord.Groups[i].Domain<>'' then
          Caption:=Format('%s\%s',[FRecord.Groups[i].Domain,FRecord.Groups[i].Name])
        else
          Caption:=FRecord.Groups[i].Name;
      SubItems.Add(Format('0x%x',[FRecord.Groups[i].Flags]));
    end;
  with SecList.Items.Add do begin
   Caption:='';
   ImageIndex:=-2;
  end;
  with SecList.Items.Add do begin
    Caption:='Privileges';
    ImageIndex:=-3;
  end;
  for i:=0 to High(FRecord.Privileges) do
    with SecList.Items.Add do begin
      Caption:=FRecord.Privileges[i].Name;
      SubItems.Add(IntToStr(Integer(FRecord.Privileges[i].Flags and SE_PRIVILEGE_ENABLED=SE_PRIVILEGE_ENABLED)));
      ImageIndex:=-4;
    end;

  c:=FHL.Count;
  TabSheet6.Caption:=Format(' Handles (%d) ',[c]);
  HList.Items.Clear;
  for i:=0 to c-1 do begin
    h:=PHandleRecord(FHL[i])^;
    with HList.Items.Add do begin
      Caption:=Format('0x%x',[h.Handle]);
      SubItems.Add(h.TypeName);
      SubItems.Add(Format('0x%x',[h.Access]));
      SubItems.Add(h.Name);
    end;
  end;

  r:=nil;
  c:=FWL.Count;
  TabSheet7.Caption:=Format(' Windows (%d) ',[c]);
  WinTree.Items.Clear;
  for i:=0 to c-1 do begin
    w:=PWindowRecord(FWL[i])^;
    for j:=0 to WinTree.Items.Count-1 do
      if PHandle(WinTree.Items[j].Data)^=w.ParentWin then begin
        r:=WinTree.Items[j];
        Break;
      end;
    n:=WinTree.Items.AddChild(r,Format('(0x%x) "%s": %s',[w.Handle,w.Text,w.ClassName]));
    n.ImageIndex:=Integer(w.Visible);
    n.Data:=Allocmem(SizeOf(THandle));
    n.SelectedIndex:=n.ImageIndex;
    PHandle(n.Data)^:=w.Handle;
  end;
  if WinTree.Items.Count>0 then begin
    WinTree.FullExpand;
    WinTree.Items[0].MakeVisible;
  end;

  EnvBox.Items.Text:=FRecord.Environment;
  tsEnv.Caption:=Format('Environment (%d)',[EnvBox.Items.Count]);

end;

procedure Tdlg_PrcDetails.ListAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if Abs(TListView(Sender).Tag)=1 then
    Sender.Canvas.Brush.Color:=clInfoBk
  else
    Sender.Canvas.Brush.Color:=clWhite
end;

procedure Tdlg_PrcDetails.ListAdvancedCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  if Abs(TListView(Sender).Tag)=SubItem+1 then
    Sender.Canvas.Brush.Color:=clInfoBk
  else
    Sender.Canvas.Brush.Color:=clWhite
end;

procedure Tdlg_PrcDetails.ListColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  TListView(Sender).SortType:=stNone;
  if Column.Index+1<>abs(TListView(Sender).Tag) then
    TListView(Sender).Tag:=Column.Index+1
  else
    TListView(Sender).Tag:=-TListView(Sender).Tag;
  TListView(Sender).SortType:=stText;
end;

procedure Tdlg_PrcDetails.ListCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare:=ListView_CustomSort(Item1,Item2,abs(TListView(Sender).Tag)-1);
  if TListView(Sender).Tag<0 then
    Compare:=-Compare;
end;

procedure Tdlg_PrcDetails.GenListAdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  DefaultDraw:=True;
  with TListView(Sender) do begin
    Canvas.Font.Style:=[];
    if Item.ImageIndex=-3 then
      Canvas.Font.Style:=[fsBold];
    Canvas.Font.Color:=clBlack;
    if cdsHot in State then 
      Canvas.Font.Style:=Canvas.Font.Style+[fsUnderline]
    else
      Canvas.Font.Style:=Canvas.Font.Style-[fsUnderline];
    if Item.ImageIndex=-2 then
      ListView_DrawLine(Sender,Item,State,DefaultDraw,clGray);
  end;
end;

procedure Tdlg_PrcDetails.GenListAdvancedCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  with TListView(Sender) do begin
    Canvas.Font.Style:=[];
    if Item.ImageIndex=-3 then
      Canvas.Font.Style:=[fsBold];
    Canvas.Font.Color:=clBlack;
    if Item.ImageIndex=-4 then
      ListView_DrawCheckBox(Sender,Item,SubItem,State,DefaultDraw,'1')
    else
      Canvas.Brush.Color:=Color;
    if cdsHot in State then
      Canvas.Font.Style:=Canvas.Font.Style+[fsUnderline]
    else
      Canvas.Font.Style:=Canvas.Font.Style-[fsUnderline];
  end;
end;

procedure Tdlg_PrcDetails.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FML.Clear;
  FML.Free;
  FHL.Clear;
  FHL.Free;
  FWL.Clear;
  FWL.Free;
end;

procedure Tdlg_PrcDetails.FormCreate(Sender: TObject);
begin
  pc.ActivePage:=TabSheet1;
  FML:=TList.Create;
  FHL:=TList.Create;
  FWL:=TList.Create;
end;

procedure Tdlg_PrcDetails.WinTreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  Dispose(PInteger(Node.Data));
end;

procedure Tdlg_PrcDetails.cmWinDetails(Sender: TObject);
var
  w: PWindowRecord;
begin
  if not Assigned(WinTree.Selected) then
    Exit;
  for w in FWL do
    if w.Handle=PHandle(WinTree.Selected.Data)^ then begin
      ShowWinDetails(w^);
      Break;
    end;
end;

end.
