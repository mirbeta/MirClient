unit Details;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, MSI_Processes, ImgList;

type
  Tdlg_Details = class(TForm)
    Bevel1: TBevel;
    bClose: TButton;
    pc: TPageControl;
    TabSheet2: TTabSheet;
    ModList: TListView;
    TabSheet1: TTabSheet;
    GenList: TListView;
    imgIcon: TImage;
    lName: TLabel;
    TabSheet3: TTabSheet;
    CPList: TListView;
    Button1: TButton;
    Button2: TButton;
    TabSheet4: TTabSheet;
    CntList: TListView;
    Button3: TButton;
    Button4: TButton;
    bRefresh: TButton;
    TabSheet5: TTabSheet;
    ThdList: TListView;
    TabSheet6: TTabSheet;
    HList: TListView;
    TabSheet7: TTabSheet;
    WinTree: TTreeView;
    ImageList: TImageList;
    Button5: TButton;
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
    procedure GenListDblClick(Sender: TObject);
    procedure CPListDblClick(Sender: TObject);
    procedure ModListDblClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure WinTreeDeletion(Sender: TObject; Node: TTreeNode);
  private
    FPL: TProcessList;
    FIndex: DWORD;
  public
    procedure RefreshData;
  end;

procedure ShowDetails(var PL: TProcessList; Index: DWORD);

var
  dlg_Details: Tdlg_Details;

implementation

uses
  MiTeC_CtrlRtns, MiTeC_Datetime, MiTeC_Routines, MiTeC_Dialogs, MiTeC_Native;

{$R *.dfm}

procedure ShowDetails;
begin
  with Tdlg_Details.Create(Application.Mainform) do
    try
      FPL:=PL;
      FIndex:=Index;

      RefreshData;

      ShowModal;

      PL:=FPL;
    finally
      Free;
    end;
end;

procedure Tdlg_Details.RefreshData;
var
  i,j: Integer;
  VI: TVersionInfo;
  FPI: TProcessInfo;
  r,n: TTreeNode;
begin
  TabSheet4.TabVisible:=Win32Platform=VER_PLATFORM_WIN32_NT;
  TabSheet6.TabVisible:=Win32Platform=VER_PLATFORM_WIN32_NT;

  FPI:=FPL.Processes[FIndex];

  lName.Caption:=FPI.Name;
  if FileExists(FPI.ImageName) then
    imgIcon.Picture.Icon.Handle:=GetFileIcon(FPI.ImageName);
  GetFileVerInfo(FPI.ImageName,VI);

  GenList.Items.Clear;
  with GenList.Items.Add do begin
    Caption:='Description';
    SubItems.Add(VI.Description);
    ImageIndex:=-3;
  end;
  with GenList.Items.Add do begin
    Caption:='Version';
    SubItems.Add(VI.Version);
  end;
  with GenList.Items.Add do begin
    Caption:='Product Name';
    SubItems.Add(VI.ProductName);
  end;
  with GenList.Items.Add do begin
    Caption:='Company Name';
    SubItems.Add(VI.CompanyName);
  end;
  with GenList.Items.Add do begin
    Caption:='';
    ImageIndex:=-2;
  end;
  with GenList.Items.Add do begin
    Caption:='PID';
    if Win32Platform=VER_PLATFORM_WIN32_NT then
      SubItems.Add(Format('%d',[FPI.PID]))
    else
      SubItems.Add(Format('%x',[FPI.PID]));
    ImageIndex:=-3;
  end;
  with GenList.Items.Add do begin
    Caption:='Parent PID';
    if Win32Platform=VER_PLATFORM_WIN32_NT then
      SubItems.Add(Format('%d',[FPI.ParentPID]))
    else
      SubItems.Add(Format('%x',[FPI.ParentPID]));
  end;
  with GenList.Items.Add do begin
    Caption:='Image name';
    SubItems.Add(FPI.ImageName);
  end;
  with GenList.Items.Add do begin
    Caption:='';
    ImageIndex:=-2;
  end;
  with GenList.Items.Add do begin
    Caption:='Priority';
    SubItems.Add(Format('%d',[FPI.Priority]));
    ImageIndex:=-3;
  end;
  with GenList.Items.Add do begin
    Caption:='Thread Count';
    SubItems.Add(Format('%d',[FPI.ThreadCount]));
  end;
  if Win32Platform<>VER_PLATFORM_WIN32_NT then begin
    with GenList.Items.Add do begin
      Caption:='Process Usage';
      SubItems.Add(Format('%d',[FPI.Usage]));
    end;
  end else begin
    with GenList.Items.Add do begin
      Caption:='Handle Count';
      SubItems.Add(Format('%d',[FPI.HandleCount]));
    end;
  end;

  TabSheet2.Caption:=Format(' Modules (%d) ',[Length(FPI.Modules)]);
  ModList.Items.Clear;
  for i:=0 to High(FPI.Modules) do
    with ModList.Items.Add do begin
      Caption:=FPI.Modules[i].Name;
      SubItems.Add(Format('%d',[FPI.Modules[i].ImageSize]));
      SubItems.Add(Format('0x%x',[FPI.Modules[i].BaseAddress]));
      SubItems.Add(Format('0x%x',[FPI.Modules[i].EntryPoint]));
      SubItems.Add(FPI.Modules[i].ImageName);
    end;

  TabSheet5.Caption:=Format(' Threads (%d) ',[FPI.ThreadCount]);
  ThdList.Items.Clear;
  if Win32Platform<>VER_PLATFORM_WIN32_NT then begin
    ThdList.Columns.Items[1].Width:=0;
    ThdList.Columns.Items[4].Width:=0;
    ThdList.Columns.Items[5].Width:=0;
    ThdList.Columns.Items[6].Width:=0;
    ThdList.Columns.Items[7].Width:=0;
  end;
  for i:=0 to High(FPI.Threads) do
    with ThdList.Items.Add do begin
      Caption:=Format('%d',[FPI.Threads[i].ClientID.UniqueThread]);
      Subitems.Add(Format('0x%p',[FPI.Threads[i].StartAddress]));
      Subitems.Add(Format('%d',[FPI.Threads[i].BasePriority]));
      Subitems.Add(Format('%d',[FPI.Threads[i].Priority]));
      if TThreadState(FPI.Threads[i].State)<>StateWait then
        SubItems.Add(cThreadState[TThreadState(FPI.Threads[i].State)])
      else
        SubItems.Add(Format('%s:%s',[cThreadState[TThreadState(FPI.Threads[i].State)],cKWaitReason[TKWaitReason(FPI.Threads[i].WaitReason)]]));
      Subitems.Add(Format('%d',[FPI.Threads[i].ContextSwitchCount]));
      SubItems.Add(FormatSeconds(FPI.Threads[i].KernelTime.QuadPart/10000000,True,False,True));
      SubItems.Add(FormatSeconds(FPI.Threads[i].UserTime.QuadPart/10000000,True,False,True));
    end;

  CPList.Items.Clear;
  for i:=0 to FPL.ProcessCount-1 do
    if FPL.Processes[i].ParentPID=FPI.PID then
      with CPList.Items.Add do begin
        if Win32Platform=VER_PLATFORM_WIN32_NT then
          Caption:=Format('%d',[FPL.Processes[i].PID])
        else
          Caption:=Format('%x',[FPL.Processes[i].PID]);
        SubItems.Add(FPL.Processes[i].Name);
      end;
  TabSheet3.Caption:=Format(' Child Procs(%d) ',[CPList.Items.Count]);

  if Win32Platform=VER_PLATFORM_WIN32_NT then begin
    CntList.Items.Clear;
    with CntList.Items.Add do begin
      Caption:='CPU Times';
      ImageIndex:=-3;
    end;
    with CntList.Items.Add do begin
      Caption:='Kernel Time';
      SubItems.Add(FormatSeconds(FPI.KernelTime.QuadPart/10000000,True,False,True));
      SubItems.Add('-');
    end;
    with CntList.Items.Add do begin
      Caption:='User Time';
      SubItems.Add(FormatSeconds(FPI.UserTime.QuadPart/10000000,True,False,True));
      SubItems.Add('-');
    end;
    with CntList.Items.Add do begin
      Caption:='Total Time';
      SubItems.Add(FormatSeconds((FPI.UserTime.QuadPart+FPI.KernelTime.QuadPart)/10000000,True,False,True));
      SubItems.Add('-');
    end;
    with CntList.Items.Add do begin
      Caption:='';
      ImageIndex:=-2;
    end;
    with CntList.Items.Add do begin
      Caption:='';
    end;
    with CntList.Items.Add do begin
      Caption:='I/O Counters';
      ImageIndex:=-3;
    end;
    with CntList.Items.Add do begin
      Caption:='Read Operation Count';
      SubItems.Add(Format('%d',[FPI.IOCounters.ReadOperationCount.QuadPart]));
      SubItems.Add('-');
    end;
    with CntList.Items.Add do begin
      Caption:='Write Operation Count';
      SubItems.Add(Format('%d',[FPI.IOCounters.WriteOperationCount.QuadPart]));
      SubItems.Add('-');
    end;
    with CntList.Items.Add do begin
      Caption:='Other Operation Count';
      SubItems.Add(Format('%d',[FPI.IOCounters.OtherOperationCount.QuadPart]));
      SubItems.Add('-');
    end;
    with CntList.Items.Add do begin
      Caption:='Read Transfer Count';
      SubItems.Add(Format('%d',[FPI.IOCounters.ReadTransferCount.QuadPart]));
      SubItems.Add('-');
    end;
    with CntList.Items.Add do begin
      Caption:='Write Transfer Count';
      SubItems.Add(Format('%d',[FPI.IOCounters.WriteTransferCount.QuadPart]));
      SubItems.Add('-');
    end;
    with CntList.Items.Add do begin
      Caption:='Other Transfer Count';
      SubItems.Add(Format('%d',[FPI.IOCounters.OtherTransferCount.QuadPart]));
      SubItems.Add('-');
    end;
    with CntList.Items.Add do begin
      Caption:='';
      ImageIndex:=-2;
    end;
    with CntList.Items.Add do begin
      Caption:='';
    end;
    with CntList.Items.Add do begin
      Caption:='Virtual Memory Counters';
      ImageIndex:=-3;
    end;
    with CntList.Items.Add do begin
      Caption:='Page Fault Count';
      SubItems.Add(Format('%d',[FPI.VMCounters.PageFaultCount]));
      SubItems.Add('-');
    end;
    with CntList.Items.Add do begin
      Caption:='Virtual Size';
      SubItems.Add(Format('%d B',[FPI.VMCounters.VirtualSize]));
      SubItems.Add(Format('%d B',[FPI.VMCounters.PeakVirtualSize]));
    end;
    with CntList.Items.Add do begin
      Caption:='Working Set Size';
      SubItems.Add(Format('%d B',[FPI.VMCounters.WorkingSetSize]));
      SubItems.Add(Format('%d B',[FPI.VMCounters.PeakWorkingSetSize]));
    end;
    with CntList.Items.Add do begin
      Caption:='Quota Peak Paged Pool Usage';
      SubItems.Add(Format('%d B',[FPI.VMCounters.QuotaPagedPoolUsage]));
      SubItems.Add(Format('%d B',[FPI.VMCounters.QuotaPeakPagedPoolUsage]));
    end;
    with CntList.Items.Add do begin
      Caption:='Quota Peak Non Paged Pool Usage';
      SubItems.Add(Format('%d B',[FPI.VMCounters.QuotaNonPagedPoolUsage]));
      SubItems.Add(Format('%d B',[FPI.VMCounters.QuotaPeakNonPagedPoolUsage]));
    end;
    with CntList.Items.Add do begin
      Caption:='Pagefile Usage';
      SubItems.Add(Format('%d B',[FPI.VMCounters.PagefileUsage]));
      SubItems.Add(Format('%d B',[FPI.VMCounters.PeakPagefileUsage]));
    end;
  end;

  HList.Items.Clear;
  for i:=0 to FPL.HandleCount-1 do
    if FPL.Handles[i].PID=FPI.PID then
      with HList.Items.Add do begin
        Caption:=Format('0x%x',[FPL.Handles[i].Handle]);
        SubItems.Add(cSystemHandleType[TSystemHandleType(FPL.Handles[i].Typ)]);
        SubItems.Add(Format('0x%x',[FPL.Handles[i].Access]));
        SubItems.Add(FPL.Handles[i].Name);
      end;
  TabSheet6.Caption:=Format(' Handles (%d) ',[HList.Items.Count]);


  WinTree.Items.Clear;
  for i:=0 to FPL.WindowCount-1 do begin
    if FPL.Windows[i].Process<>FPI.PID then
      Continue;
    r:=nil;
    for j:=0 to WinTree.Items.Count-1 do
      if PInteger(WinTree.Items[j].Data)^=FPL.Windows[i].ParentWin then begin
        r:=WinTree.Items[j];
        Break;
      end;
    n:=WinTree.Items.AddChild(r,Format('(0x%x) "%s": %s',[FPL.Windows[i].Handle,FPL.Windows[i].Text,FPL.Windows[i].ClassName]));
    n.ImageIndex:=Integer(FPL.Windows[i].Visible);
    n.Data:=Allocmem(SizeOf(Integer));
    PInteger(n.Data)^:=FPL.Windows[i].Handle;
  end;
  if WinTree.Items.Count>0 then begin
    WinTree.FullExpand;
    WinTree.Items[0].MakeVisible;
  end;
  TabSheet7.Caption:=Format(' Windows (%d) ',[WinTree.Items.Count]);
end;

procedure Tdlg_Details.ListAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if ListView_SortColumn=0 then
    Sender.Canvas.Brush.Color:=clInfoBk
  else
    Sender.Canvas.Brush.Color:=clWhite
end;

procedure Tdlg_Details.ListAdvancedCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  if ListView_SortColumn=SubItem then
    Sender.Canvas.Brush.Color:=clInfoBk
  else
    Sender.Canvas.Brush.Color:=clWhite
end;

procedure Tdlg_Details.ListColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  TListView(Sender).SortType:=stNone;
  if Column.Index<>ListView_SortColumn then begin
    ListView_SortColumn:=Column.Index;
    ListView_SortDescending:=False;
  end else
    ListView_SortDescending:=not ListView_SortDescending;
  TListView(Sender).SortType:=stText;
end;

procedure Tdlg_Details.ListCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare:=ListView_CustomSort(Item1,Item2,ListView_SortColumn);
  if ListView_SortDescending then
    Compare:=-Compare;
end;

procedure Tdlg_Details.GenListAdvancedCustomDrawItem(Sender: TCustomListView;
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

procedure Tdlg_Details.GenListAdvancedCustomDrawSubItem(
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

procedure Tdlg_Details.GenListDblClick(Sender: TObject);
var
  idx: Integer;
begin
  idx:=FPL.FindProcess(FPL.Processes[FIndex].ParentPID);
  if idx>=0 then begin
    FIndex:=idx;
    RefreshData;
  end;
end;

procedure Tdlg_Details.CPListDblClick(Sender: TObject);
var
  idx: Integer;
begin
  if Assigned(CPList.Selected) then begin
    try
      idx:=FPL.FindProcess(StrToInt(CPList.Selected.Caption));
    except
      idx:=FPL.FindProcess(StrToInt('$'+CPList.Selected.Caption));
    end;
    if idx>=0 then begin
      FIndex:=idx;
      RefreshData;
      pc.ActivePage:=TabSheet1;
    end;
  end;
end;

procedure Tdlg_Details.ModListDblClick(Sender: TObject);
begin
  if Assigned(ModList.Selected) then
    ShellPropDlg(Handle,ModList.Selected.SubItems[0]);
end;

procedure Tdlg_Details.Button4Click(Sender: TObject);
begin
  if FPL.Processes[FIndex].PID>9 then
    ShellPropDlg(Handle,FPL.Processes[FIndex].ImageName);
end;

procedure Tdlg_Details.bRefreshClick(Sender: TObject);
var
  PID: DWORD;
  idx: Integer;
begin
  Screen.Cursor:=crHourglass;
  try
    PID:=FPL.Processes[FIndex].PID;
    FPL.GetInfo;
    idx:=FPL.FindProcess(PID);
    if idx>=0 then begin
      FIndex:=idx;
      RefreshData;
    end else
      Close;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure Tdlg_Details.FormCreate(Sender: TObject);
begin
  pc.ActivePage:=TabSheet1;
end;

procedure Tdlg_Details.WinTreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  Dispose(PInteger(Node.Data));
end;

end.
