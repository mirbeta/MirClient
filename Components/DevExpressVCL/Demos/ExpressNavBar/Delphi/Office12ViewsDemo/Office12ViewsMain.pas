unit Office12ViewsMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ToolWin, ShlObj, ImgList, Menus, ExtCtrls,
  dxNavBarBase, dxNavBarCollns, dxNavBar, Grids, cxClasses, cxControls,
  cxGraphics, ActnList, dxNavBarGroupItems, cxLookAndFeels,
  cxLookAndFeelPainters, dxNavBarOfficeNavigationBar, cxContainer, cxEdit,
  cxTextEdit, cxGroupBox;

type
  PShellItem = ^TShellItem;
  TShellItem = record
    FullID,
    ID: PItemIDList;
    ParentID: PItemIDList;
    ShellFolder: IShellFolder;
    Empty: Boolean;
    DisplayName,
    TypeName: string;
    ImageIndex,
    Size,
    Attributes: Integer;
    ModDate: string;
  end;

  TfmMain = class(TForm)
    lvMain: TListView;
    nbMain: TdxNavBar;
    bgSearch: TdxNavBarGroup;
    bgSearchControl: TdxNavBarGroupControl;
    btnSearch: TButton;
    StatusBar1: TStatusBar;
    nbMainDesktop: TdxNavBarItem;
    nbMainMyDocuments: TdxNavBarItem;
    nbMainNetwork: TdxNavBarItem;
    bgFavorites: TdxNavBarGroup;
    bgMyComputer: TdxNavBarGroup;
    bgMyComputerControl: TdxNavBarGroupControl;
    bgFavoritesControl: TdxNavBarGroupControl;
    lvMyFavorites: TListView;
    Splitter1: TSplitter;
    ilSmall: TcxImageList;
    bgColorScheme: TdxNavBarGroup;
    nbMyComputer: TdxNavBar;
    nbMyComputerGroup1: TdxNavBarGroup;
    nbMyComputerGroup2: TdxNavBarGroup;
    nbMyComputerGroup2Control: TdxNavBarGroupControl;
    ilMainSmall: TImageList;
    ilMainLarge: TImageList;
    tvMyComputer: TTreeView;
    nbMainBlue: TdxNavBarItem;
    nbMainBlack: TdxNavBarItem;
    nbMainSilver: TdxNavBarItem;
    ActionList1: TActionList;
    bgOptions: TdxNavBarGroup;
    bgOptionsControl: TdxNavBarGroupControl;
    nbOptions: TdxNavBar;
    nbOptionsListOptions: TdxNavBarGroup;
    nbOptionsNavBarOptions: TdxNavBarGroup;
    nbOptionsLargeIcons: TdxNavBarItem;
    nbOptionsSmallIcons: TdxNavBarItem;
    nbOptionsList: TdxNavBarItem;
    nbOptionsReport: TdxNavBarItem;
    actLargeIcons: TAction;
    actSmallIcons: TAction;
    actList: TAction;
    actReport: TAction;
    nbMainLargeIcons: TdxNavBarItem;
    nbMainSmallIcons: TdxNavBarItem;
    nbMainList: TdxNavBarItem;
    nbMainReport: TdxNavBarItem;
    nbOptionsAdjustWidthByPopup: TdxNavBarItem;
    nbOptionsCollapsible: TdxNavBarItem;
    nbOptionsAllowCustomize: TdxNavBarItem;
    nbMyComputerDesktop: TdxNavBarItem;
    nbMyComputerMyDocuments: TdxNavBarItem;
    nbMyComputerMyNetworkPlaces: TdxNavBarItem;
    actDesktop: TAction;
    actMyDocuments: TAction;
    actMyNetworkPlaces: TAction;
    nbMyComputerUp: TdxNavBarItem;
    actUp: TAction;
    nbMainUp: TdxNavBarItem;
    ilLarge: TcxImageList;
    nbOptionsTabStop: TdxNavBarItem;
    Label9: TLabel;
    nbMyComputerSeparator1: TdxNavBarSeparator;
    dxNavBarOfficeNavigationBar1: TdxNavBarOfficeNavigationBar;
    MainMenu1: TMainMenu;
    Options1: TMenuItem;
    ShowOfficeNavigationBar1: TMenuItem;
    actAdjustWidthByPopup: TAction;
    actAllowCustomize: TAction;
    actOptionsCollapsible: TAction;
    actOptionsTabStop: TAction;
    actSearch: TAction;
    dxNavBar1: TdxNavBar;
    dxNavBarGroup1: TdxNavBarGroup;
    dxNavBarGroup2: TdxNavBarGroup;
    dxNavBarItem1: TdxNavBarItem;
    dxNavBarItem2: TdxNavBarItem;
    dxNavBarItem3: TdxNavBarItem;
    dxNavBarItem4: TdxNavBarItem;
    dxNavBarItem5: TdxNavBarItem;
    dxNavBarItem6: TdxNavBarItem;
    dxNavBarItem7: TdxNavBarItem;
    dxNavBarItem8: TdxNavBarItem;
    cxGroupBox2: TcxGroupBox;
    cxTextEdit1: TcxTextEdit;
    Button1: TButton;
    dxNavBar2: TdxNavBar;
    dxNavBarGroup3: TdxNavBarGroup;
    dxNavBarItem9: TdxNavBarItem;
    dxNavBarItem10: TdxNavBarItem;
    dxNavBarItem11: TdxNavBarItem;
    actBlue: TAction;
    actBlack: TAction;
    actSilver: TAction;
    edSearch: TcxTextEdit;
    procedure FormCreate(Sender: TObject);
    procedure lvMainData(Sender: TObject; Item: TListItem);
    procedure btnLargeIconsClick(Sender: TObject);
    procedure lvMainDblClick(Sender: TObject);
    procedure lvMainDataHint(Sender: TObject; StartIndex,
      EndIndex: Integer);
    procedure lvMainKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvMainDataFind(Sender: TObject; Find: TItemFind;
      const FindString: String; const FindPosition: TPoint;
      FindData: Pointer; StartIndex: Integer; Direction: TSearchDirection;
      Wrap: Boolean; var Index: Integer);
    procedure lvMainCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvMainCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure btnBackClick(Sender: TObject);
    procedure Form1Close(Sender: TObject; var Action: TCloseAction);
    procedure btnSearchClick(Sender: TObject);
    procedure nbMainDesktopClick(Sender: TObject);
    procedure nbMainMyDocumentsClick(Sender: TObject);
    procedure nbMainNetworkClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvMyComputerClick(Sender: TObject);
    procedure lvMyFavoritesClick(Sender: TObject);
    procedure actSchemeExecute(Sender: TObject);
    procedure actLargeIconsExecute(Sender: TObject);
    procedure nbOptionsAdjustWidthByPopupClick(Sender: TObject);
    procedure nbOptionsAllowCustomizeClick(Sender: TObject);
    procedure nbOptionsCollapsibleClick(Sender: TObject);
    procedure nbOptionsTabStopClick(Sender: TObject);
    procedure tvMyComputerAdvancedCustomDraw(Sender: TCustomTreeView;
      const ARect: TRect; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure tvMyComputerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tvMyComputerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ShowOfficeNavigationBar1Click(Sender: TObject);
    procedure dxNavBarOfficeNavigationBar1QueryPeekFormContent(ASender: TObject;
      ANavigationItem: IdxNavigationItem; var AControl: TWinControl);
    procedure cxTextEdit1PropertiesChange(Sender: TObject);
    procedure edSearchPropertiesChange(Sender: TObject);
  private
    FLockSearchTextChange: Boolean;
    FIDList: TList;
    FSearchShellID: PItemIDList;
    FShellID: PItemIDList;
    FDesktopFolder: IShellFolder;
    FSearching: Boolean;

    function GetShellFolder: IShellFolder;
    function GetShellItemCount: Integer;
    function GetShellItem(Index: Integer): PShellItem;

    procedure ClearIDList;

    function SwitchOption(Sender: TObject; AValue: Boolean): Boolean;
    procedure CloseNavBarPopup;
  protected
    function GetIDByPath(APath: string): PItemIDList;
    function GetIDBySpetialFolder(ASpetialFolder: Integer): PItemIDList;
    function GetShellFolderByID(AID: PItemIDList): IShellFolder;
    function GetEnumIDListByFolder(AFolder: IShellFolder): IEnumIDList;

    function CompareNames(Path, Pattern: string): Boolean;
    procedure SetSearch(AID: PItemIDList; const Pattern: string);
    procedure SetPath(const Value: string); overload;
    procedure SetPath(ID: PItemIDList); overload;
    procedure PopulateIDList(AID: PItemIDList);
    procedure PopulateSearchIDList(ASearchID: PItemIDList; Pattern: string);
    procedure PopulateMyFavoritesList(AID: PItemIDList);
    procedure PopulateMyComputerTree(AID: PItemIDList);
    procedure CheckShellItems(StartIndex, EndIndex: Integer);
  public
    property DesktopFolder: IShellFolder read FDesktopFolder;
    property SearchShellID: PItemIDList read FSearchShellID;
    property ShellFolder: IShellFolder read GetShellFolder;
    property ShellID: PItemIDList read FShellID;

    property ShellItems[Index: Integer]: PShellItem read GetShellItem;
    property ShellItemCount: Integer read GetShellItemCount;
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses
  Types, ShellAPI, ActiveX, ComObj, CommCtrl, cxGeometry,
  dxNavBarOffice11Views, dxNavBarSkinBasedViews;

procedure DisposePIDL(ID: PItemIDList);
var
  Malloc: IMalloc;
begin
  if ID = nil then Exit;
  OLECheck(SHGetMalloc(Malloc));
  Malloc.Free(ID);
end;

function NextPIDL(IDList: PItemIDList): PItemIDList;
begin
  Result := IDList;
  Inc(PAnsiChar(Result), IDList^.mkid.cb);
end;

function GetPIDLSize(IDList: PItemIDList): Integer;
begin
  Result := 0;
  if Assigned(IDList) then
  begin
    Result := SizeOf(IDList^.mkid.cb);
    while IDList^.mkid.cb <> 0 do
    begin
      Result := Result + IDList^.mkid.cb;
      IDList := NextPIDL(IDList);
    end;
  end;
end;

procedure StripLastID(IDList: PItemIDList);
var
  MarkerID: PItemIDList;
begin
  MarkerID := IDList;
  if Assigned(IDList) then
  begin
     while IDList.mkid.cb <> 0 do
    begin
      MarkerID := IDList;
      IDList := NextPIDL(IDList);
    end;
    MarkerID.mkid.cb := 0;
  end;
end;

function CreatePIDL(Size: Integer): PItemIDList;
var
  Malloc: IMalloc;
  HR: HResult;
begin
  Result := nil;

  HR := SHGetMalloc(Malloc);
  if Failed(HR) then
    Exit;

  try
    Result := Malloc.Alloc(Size);
    if Assigned(Result) then
      FillChar(Result^, Size, 0);
  finally
  end;
end;

function CopyPIDL(IDList: PItemIDList): PItemIDList;
var
  Size: Integer;
begin
  Size := GetPIDLSize(IDList);
  Result := CreatePIDL(Size);
  if Assigned(Result) then
    CopyMemory(Result, IDList, Size);
end;

function ConcatPIDLs(IDList1, IDList2: PItemIDList): PItemIDList;
var
  cb1, cb2: Integer;
begin
  if Assigned(IDList1) then
    cb1 := GetPIDLSize(IDList1) - SizeOf(IDList1^.mkid.cb)
  else cb1 := 0;
  cb2 := GetPIDLSize(IDList2);

  Result := CreatePIDL(cb1 + cb2);
  if Assigned(Result) then
  begin
    if Assigned(IDList1) then
      CopyMemory(Result, IDList1, cb1);
    CopyMemory(PAnsiChar(Result) + cb1, IDList2, cb2);
  end;
end;

function GetDisplayName(ShellFolder: IShellFolder; PIDL: PItemIDList;
                        ForParsing: Boolean): string;
var
  StrRet: TStrRet;
  P: PAnsiChar;
  Flags: Integer;
begin
  Result := '';
  if ForParsing then
    Flags := SHGDN_FORPARSING
  else Flags := SHGDN_NORMAL;

  ShellFolder.GetDisplayNameOf(PIDL, Flags, StrRet);
  case StrRet.uType of
    STRRET_CSTR:
      SetString(Result, StrRet.cStr, lStrLenA(StrRet.cStr));
    STRRET_OFFSET:
      begin
        P := @PIDL.mkid.abID[StrRet.uOffset - SizeOf(PIDL.mkid.cb)];
        SetString(Result, P, PIDL.mkid.cb - StrRet.uOffset);
      end;
    STRRET_WSTR:
      Result := StrRet.pOleStr;
  end;
end;

function GetShellImage(PIDL: PItemIDList; Large, Open: Boolean): Integer;
var
  FileInfo: TSHFileInfo;
  Flags: Integer;
begin
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  Flags := SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_ICON;
  if Open then Flags := Flags or SHGFI_OPENICON;
  if Large then Flags := Flags or SHGFI_LARGEICON
  else Flags := Flags or SHGFI_SMALLICON;
  SHGetFileInfo(PChar(PIDL), 0, FileInfo, SizeOf(FileInfo), Flags);
  Result := FileInfo.iIcon;
end;

function IsFolder(ShellFolder: IShellFolder; ID: PItemIDList): Boolean;
var
  Flags: UINT;
begin
  Flags := SFGAO_FOLDER;
  ShellFolder.GetAttributesOf(1, ID, Flags);
  Result := SFGAO_FOLDER and Flags <> 0;
end;


function ListSortFunc(Item1, Item2: Pointer): Integer;
begin
  Result := SmallInt(fmMain.ShellFolder.CompareIDs(0,
    PShellItem(Item1).ID, PShellItem(Item2).ID));
end;

{TForm1}

procedure TfmMain.FormCreate(Sender: TObject);
var
  FileInfo: TSHFileInfo;
  NewPIDL: PItemIDList;
begin
  cxGroupBox2.Parent := nil;
  dxNavBar1.Parent := nil;
  dxNavBar2.Parent := nil;

  OLECheck(SHGetDesktopFolder(FDesktopFolder));
  FIDList := TList.Create;

  ilMainSmall.Handle := SHGetFileInfo('C:\', 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  ilMainLarge.Handle := SHGetFileInfo('C:\', 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or SHGFI_LARGEICON);

  OLECheck(SHGetSpecialFolderLocation(Application.Handle, CSIDL_DRIVES, NewPIDL));
  FShellID := NewPIDL;
  SetPath(NewPIDL);
  PopulateMyFavoritesList(GetIDBySpetialFolder(CSIDL_FAVORITES));
  PopulateMyComputerTree(NewPIDL);
end;

procedure TfmMain.btnLargeIconsClick(Sender: TObject);
begin
  lvMain.ViewStyle := TViewStyle((Sender as TComponent).Tag);
end;

procedure TfmMain.lvMainDblClick(Sender: TObject);
var
  AShellFolder: IShellFolder;
  AParentID, AID: PItemIDList;
begin
  if lvMain.Selected <> nil then
  begin
    AID := ShellItems[lvMain.Selected.Index].ID;
    AParentID := ShellItems[lvMain.Selected.Index].ParentID;
    if FSearchShellID = nil then
    begin
      AShellFolder := ShellItems[lvMain.Selected.Index].ShellFolder;
      if IsFolder(AShellFolder, AID) then
        SetPath(ConcatPIDLs(AParentID, AID));
    end
    else SetPath(AParentID);
  end;
end;

function TfmMain.GetShellFolder: IShellFolder;
begin
  Result := GetShellFolderByID(FShellID);
end;

function TfmMain.GetShellItemCount: Integer;
begin
  Result := FIDList.Count;
end;

function TfmMain.GetShellItem(Index: Integer): PShellItem;
begin
  Result := PShellItem(FIDList[Index]);
end;

procedure TfmMain.lvMainKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      lvMainDblClick(Sender);
    VK_BACK:
      btnBackClick(Sender);  
  end;
end;

procedure TfmMain.ClearIDList;
var
  I: Integer;
begin
  for I := 0 to ShellItemCount - 1 do
  begin
    DisposePIDL(ShellItems[I].ID);
    Dispose(ShellItems[I]);
  end;
  FIDList.Clear;
end;

function TfmMain.SwitchOption(Sender: TObject; AValue: Boolean): Boolean;
const
  UncheckImage = -1;
  CheckImage = 14;
  AImageIndex: array [Boolean] of Integer = (UncheckImage, CheckImage);
begin
  Result := not AValue;
  (Sender as TAction).ImageIndex := AImageIndex[Result];
end;

procedure TfmMain.CloseNavBarPopup;
begin
  TdxNavBarOffice11NavPanePainter(nbMain.Painter).Controller.ClosePopupControl;
end;

procedure TfmMain.PopulateIDList(AID: PItemIDList);
var
  ID: PItemIDList;
  AShellFolder: IShellFolder;
  EnumList: IEnumIDList;
  NumIDs: LongWord;
  SaveCursor: TCursor;
  ShellItem: PShellItem;
begin
  AShellFolder := GetShellFolderByID(AID);
  SaveCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    ClearIDList;
    EnumList := GetEnumIDListByFolder(AShellFolder);

    FShellID := AID;
    FSearchShellID := nil;
    while EnumList.Next(1, ID, NumIDs) = S_OK do
    begin
      ShellItem := New(PShellItem);
      ShellItem.ID := ID;
      ShellItem.ParentID := AID;
      ShellItem.ShellFolder := ShellFolder;
      ShellItem.DisplayName := GetDisplayName(AShellFolder, ID, False);
      ShellItem.Empty := True;
      FIDList.Add(ShellItem);
    end;
    FIDList.Sort(ListSortFunc);
  finally
    lvMain.Items.Count := ShellItemCount;
    lvMain.Repaint;
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TfmMain.PopulateSearchIDList(ASearchID: PItemIDList; Pattern: string);

  procedure CheckFolder(AID: PItemIDList);
  var
    AFolder: IShellFolder;
    ID: PItemIDList;
    EnumList: IEnumIDList;
    NumIDs: LongWord;
    ShellItem: PShellItem;
  begin
    AFolder := GetShellFolderByID(AID);
    EnumList := GetEnumIDListByFolder(AFolder);
    while EnumList.Next(1, ID, NumIDs) = S_OK do
    begin
      if CompareNames(GetDisplayName(AFolder, ID, True), Pattern) then
      begin
        ShellItem := New(PShellItem);
        ShellItem.ID := ID;
        ShellItem.ParentID := AID;
        ShellItem.ShellFolder := AFolder;
        ShellItem.DisplayName := GetDisplayName(AFolder, ID, False);
        ShellItem.Empty := True;
        FIDList.Add(ShellItem);
      end;
      Application.ProcessMessages;
      if not FSearching then exit;

      if IsFolder(AFolder, ID) then
      begin
        StatusBar1.SimpleText := Format('Search in %s ...', [GetDisplayName(AFolder, ID, False)]);
        CheckFolder(ConcatPIDLs(AID, ID));
      end;
    end;
    lvMain.Items.Count := ShellItemCount;
    lvMain.Repaint;
  end;

begin
  FSearchShellID := ASearchID;
  ClearIDList;
  CheckFolder(ASearchID);
end;

procedure TfmMain.PopulateMyFavoritesList(AID: PItemIDList);
var
  AItem: TListItem;
  ID, FullID: PItemIDList;
  AShellFolder: IShellFolder;
  EnumList: IEnumIDList;
  NumIDs: LongWord;
  SaveCursor: TCursor;
  FileInfo: TSHFileInfo;
begin
  AShellFolder := GetShellFolderByID(AID);
  lvMyFavorites.Items.BeginUpdate;
  SaveCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    EnumList := GetEnumIDListByFolder(AShellFolder);
    while EnumList.Next(1, ID, NumIDs) = S_OK do
    begin
      AItem := lvMyFavorites.Items.Add;
      FullID := ConcatPIDLs(AID, ID);
      AItem.Caption := GetDisplayName(AShellFolder, ID, False);
      AItem.ImageIndex := GetShellImage(FullID, False, False);

      SHGetFileInfo(PChar(FullID), 0, FileInfo, SizeOf(FileInfo), SHGFI_TYPENAME or SHGFI_PIDL);
      AItem.SubItems.Add(FileInfo.szTypeName);
      if IsFolder(AShellFolder, ID) then
        AItem.Data := FullID
      else AItem.Data := nil;
    end;
  finally
    lvMyFavorites.Repaint;
    lvMyFavorites.Items.EndUpdate;
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TfmMain.PopulateMyComputerTree(AID: PItemIDList);
var
  ANode, AItemNode: TTreeNode;
  ID, FullID: PItemIDList;
  AShellFolder: IShellFolder;
  EnumList: IEnumIDList;
  NumIDs: LongWord;
  SaveCursor: TCursor;
begin
  ANode := tvMyComputer.Items.Add(nil, 'MyComputer');
  ANode.ImageIndex := GetShellImage(AID, False, False);
  ANode.SelectedIndex := GetShellImage(AID, False, False);
  ANode.Data := AID;
  AShellFolder := GetShellFolderByID(AID);
  tvMyComputer.Items.BeginUpdate;
  SaveCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    EnumList := GetEnumIDListByFolder(AShellFolder);
    while EnumList.Next(1, ID, NumIDs) = S_OK do
    begin
      AItemNode := tvMyComputer.Items.AddChild(ANode, GetDisplayName(AShellFolder, ID, False));
      FullID := ConcatPIDLs(AID, ID);
      AItemNode.ImageIndex := GetShellImage(FullID, False, False);
      AItemNode.SelectedIndex := GetShellImage(FullID, False, False);
      if IsFolder(AShellFolder, ID) then
        AItemNode.Data := FullID
      else AItemNode.Data := nil;
    end;
  finally
    tvMyComputer.SortType := stText;
    tvMyComputer.Items.EndUpdate;
    Screen.Cursor := SaveCursor;
  end;
  tvMyComputer.FullExpand;
end;

function TfmMain.GetIDByPath(APath: string): PItemIDList;
var
  P: PWideChar;
  Flags,
  NumChars: LongWord;
begin
  NumChars := Length(APath);
  Flags := 0;
  P := StringToOleStr(APath);
  OLECheck(DesktopFolder.ParseDisplayName(Application.Handle, nil, P,
    NumChars, Result, Flags));
end;

function TfmMain.GetIDBySpetialFolder(ASpetialFolder: Integer): PItemIDList;
begin
  OLECheck(SHGetSpecialFolderLocation(Application.Handle, ASpetialFolder, Result));
end;

function TfmMain.GetShellFolderByID(AID: PItemIDList): IShellFolder;
begin
   if AID <> nil then
     OLECheck(DesktopFolder.BindToObject(AID, nil, IID_IShellFolder, Pointer(Result)))
   else Result := nil;
end;

function TfmMain.GetEnumIDListByFolder(AFolder: IShellFolder): IEnumIDList;
const
  Flags = SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN;
begin
  if AFolder <> nil then
    OleCheck(AFolder.EnumObjects(Application.Handle, Flags, Result))
  else Result := nil;
end;

function TfmMain.CompareNames(Path, Pattern: string): Boolean;
var
  APos: Integer;
  S, Name, Extention, PatName, PatExt: string;
begin
  S := Path;
  repeat
    APos := Pos('\', S);
    if APos > 0 then S := Copy(S, APos + 1, Length(S) - APos + 1);
  until APos = 0;
  APos := Pos('.', S);
  if APos > 0 then
  begin
    Name := UpperCase(Copy(S, 1, APos - 1));
    Extention := UpperCase(Copy(S, APos + 1, Length(S) - APos + 1));
  end
  else
  begin
    Name := UpperCase(S);
    Extention := '';
  end;
  Pattern := UpperCase(Pattern);
  APos := Pos('.', Pattern);
  if APos > 0 then
  begin
    PatName := Copy(Pattern, 1, APos - 1);
    PatExt := Copy(Pattern, APos + 1, Length(Pattern) - APos + 1);
  end
  else
  begin
    PatName := Pattern;
    PatExt := '';
  end;
  Result := (((Name = PatName) or (PatName = '*')) and
    ((Extention = PatExt) or (PatExt = '*') or (PatExt = ''))) or
    ((PatExt = '') and (PatName <> '') and (Pos(PatName, Name) > 0));
end;

procedure TfmMain.cxTextEdit1PropertiesChange(Sender: TObject);
begin
  if FLockSearchTextChange then
    Exit;
  FLockSearchTextChange := True;
  edSearch.Text := cxTextEdit1.Text;
  FLockSearchTextChange := False;
end;

procedure TfmMain.dxNavBarOfficeNavigationBar1QueryPeekFormContent(
  ASender: TObject; ANavigationItem: IdxNavigationItem;
  var AControl: TWinControl);
begin
  if FSearching then
    Exit;
  if ANavigationItem.Text = '&Color Scheme' then
    AControl := dxNavBar2
  else
    if ANavigationItem.Text = '&Options' then
      AControl := dxNavBar1
    else
      if ANavigationItem.Text = '&Search' then
        AControl := cxGroupBox2;
end;

procedure TfmMain.edSearchPropertiesChange(Sender: TObject);
begin
  if FLockSearchTextChange then
    Exit;
  FLockSearchTextChange := True;
  cxTextEdit1.Text := edSearch.Text;
  FLockSearchTextChange := False;
end;

procedure TfmMain.SetSearch(AID: PItemIDList; const Pattern: string);
begin
  lvMain.Items.BeginUpdate;
  try
    FSearching := True;
    actSearch.Caption := 'Stop';
    try
      PopulateSearchIDList(AID, Pattern);
      if lvMain.Items.Count > 0 then
      begin
        lvMain.Selected := lvMain.Items[0];
        lvMain.Selected.Focused := True;
        lvMain.Selected.MakeVisible(False);
      end;
    finally
      StatusBar1.SimpleText := '';
      actSearch.Caption := 'Search';
      FSearching := False;
    end;
  finally
    lvMain.Items.EndUpdate;
  end;
end;

procedure TfmMain.ShowOfficeNavigationBar1Click(Sender: TObject);
begin
  if ShowOfficeNavigationBar1.Checked then
  begin
    dxNavBarOfficeNavigationBar1.ItemProvider := nbMain;
    dxNavBarOfficeNavigationBar1.Visible := True;
  end
  else
  begin
    dxNavBarOfficeNavigationBar1.Visible := False;
    dxNavBarOfficeNavigationBar1.ItemProvider := nil;
  end;
end;

procedure TfmMain.SetPath(const Value: string);
var
  NewPIDL: PItemIDList;
begin
  NewPIDL := GetIDByPath(Value);
  SetPath(NewPIDL);
end;

procedure TfmMain.SetPath(ID: PItemIDList);
begin
  lvMain.Items.BeginUpdate;
  try
    PopulateIDList(ID);
    if lvMain.Items.Count > 0 then
    begin
      lvMain.Selected := lvMain.Items[0];
      lvMain.Selected.Focused := True;
      lvMain.Selected.MakeVisible(False);
    end;
  finally
    lvMain.Items.EndUpdate;
  end;
end;

procedure TfmMain.CheckShellItems(StartIndex, EndIndex: Integer);

 function ValidFileTime(FileTime: TFileTime): Boolean;
 begin
   Result := (FileTime.dwLowDateTime <> 0) or (FileTime.dwHighDateTime <> 0);
 end;

var
  FileData: TWin32FindData;
  FileInfo: TSHFileInfo;
  SysTime: TSystemTime;
  I: Integer;
  LocalFileTime: TFILETIME;
begin
  for I := StartIndex to EndIndex do
  begin
    if ShellItems[I]^.Empty then
    with ShellItems[I]^ do
    begin
      FullID := ConcatPIDLs(ParentID, ID);
      ImageIndex := GetShellImage(FullID, lvMain.ViewStyle = vsIcon, False);

      SHGetFileInfo(PChar(FullID), 0, FileInfo, SizeOf(FileInfo), SHGFI_TYPENAME or SHGFI_PIDL);
      TypeName := FileInfo.szTypeName;

      FillChar(FileData, SizeOf(FileData), #0);
      SHGetDataFromIDList(ShellFolder, ID, SHGDFIL_FINDDATA, @FileData, SizeOf(FileData));

      Size := (FileData.nFileSizeLow + 1023 ) div 1024;
      if Size = 0 then Size := 1;

      FillChar(LocalFileTime, SizeOf(TFileTime), #0);
      with FileData do
        if ValidFileTime(ftLastWriteTime)
        and FileTimeToLocalFileTime(ftLastWriteTime, LocalFileTime)
        and FileTimeToSystemTime(LocalFileTime, SysTime) then
        try
          ModDate := DateTimeToStr(SystemTimeToDateTime(SysTime))
        except
          on EConvertError do ModDate := '';
        end
        else ModDate := '';

      Attributes := FileData.dwFileAttributes;
      Empty := False;
    end;
  end;
end;

procedure TfmMain.lvMainDataHint(Sender: TObject; StartIndex,
  EndIndex: Integer);
begin
  if (StartIndex > ShellItemCount) or (EndIndex > ShellItemCount) then Exit;
  CheckShellItems(StartIndex, EndIndex);
end;

procedure TfmMain.lvMainData(Sender: TObject; Item: TListItem);
var
  Attrs: string;
begin
  if (Item.Index > ShellItemCount) then Exit;
  with ShellItems[Item.Index]^ do
  begin
    Item.Caption := DisplayName;
    Item.ImageIndex := ImageIndex;

    if lvMain.ViewStyle <> vsReport then Exit;

    if not IsFolder(ShellFolder, ID) then
      Item.SubItems.Add(Format('%dKB', [Size]))
    else Item.SubItems.Add('');
    Item.SubItems.Add(TypeName);
    try
      Item.SubItems.Add(ModDate);
    except
    end;

    if Bool(Attributes and FILE_ATTRIBUTE_READONLY) then Attrs := Attrs + 'R';
    if Bool(Attributes and FILE_ATTRIBUTE_HIDDEN) then Attrs := Attrs + 'H';
    if Bool(Attributes and FILE_ATTRIBUTE_SYSTEM) then Attrs := Attrs + 'S';
    if Bool(Attributes and FILE_ATTRIBUTE_ARCHIVE) then Attrs := Attrs + 'A';
  end;
  Item.SubItems.Add(Attrs);
end;

procedure TfmMain.lvMainDataFind(Sender: TObject; Find: TItemFind;
  const FindString: String; const FindPosition: TPoint; FindData: Pointer;
  StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean;
  var Index: Integer);
var
  I: Integer;
  Found: Boolean;
begin
  I := StartIndex;
  if (Find = ifExactString) or (Find = ifPartialString) then
  begin
    repeat
      if (I = ShellItemCount - 1) then
        if Wrap then I := 0 else Exit;
      Found := Pos(UpperCase(FindString), UpperCase(ShellItems[I]^.DisplayName)) = 1;
      Inc(I);
    until Found or (I = StartIndex);
    if Found then Index := I-1;
  end;
end;

procedure TfmMain.lvMainCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Attrs: Integer;
begin
  if Item = nil then Exit;
  Attrs := ShellItems[Item.Index].Attributes;
  if Bool(Attrs and FILE_ATTRIBUTE_READONLY) then
    lvMain.Canvas.Font.Color := clGrayText;
  if Bool(Attrs and FILE_ATTRIBUTE_HIDDEN) then
    lvMain.Canvas.Font.Style :=
       lvMain.Canvas.Font.Style + [fsStrikeOut];
  if Bool(Attrs and FILE_ATTRIBUTE_SYSTEM) then
    lvMain.Canvas.Font.Color := clHighlight;
end;

procedure TfmMain.lvMainCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if SubItem = 0 then Exit;
  lvMain.Canvas.Font.Color := GetSysColor(COLOR_WINDOWTEXT);
end;

procedure TfmMain.btnBackClick(Sender: TObject);
var
  Temp: PItemIDList;
begin
  if FSearchShellID = nil then
  begin
    Temp := CopyPIDL(FShellID);
    if Assigned(Temp) then
      StripLastID(Temp);
    if Temp.mkid.cb <> 0 then
      SetPath(Temp)
    else Beep;
  end
  else SetPath(FSearchShellID);
  CloseNavBarPopup;
end;

procedure TfmMain.Form1Close(Sender: TObject; var Action: TCloseAction);
begin
  FSearching := False;
end;

procedure TfmMain.btnSearchClick(Sender: TObject);
begin
  if not FSearching then
    SetSearch(FShellID, edSearch.Text)
  else FSearching := False;
  CloseNavBarPopup;
end;

procedure TfmMain.nbMainDesktopClick(Sender: TObject);
begin
  SetPath(GetIDBySpetialFolder(CSIDL_DESKTOPDIRECTORY));
  CloseNavBarPopup;
end;

procedure TfmMain.nbMainMyDocumentsClick(Sender: TObject);
begin
  SetPath(GetIDBySpetialFolder(CSIDL_PERSONAL));
  CloseNavBarPopup;
end;

procedure TfmMain.nbMainNetworkClick(Sender: TObject);
begin
  SetPath(GetIDBySpetialFolder(CSIDL_NETWORK));
  CloseNavBarPopup;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  ClearIDList;
  FIDList.Free;
end;

procedure TfmMain.tvMyComputerClick(Sender: TObject);
begin
   if (tvMyComputer.Selected <> nil) and (tvMyComputer.Selected.Data <> nil) then
   begin
     SetPath(PItemIDList(tvMyComputer.Selected.Data));
     CloseNavBarPopup;
   end;
end;

procedure TfmMain.lvMyFavoritesClick(Sender: TObject);
begin
  if (lvMyFavorites.Selected <> nil) and (lvMyFavorites.Selected.Data <> nil) then
  begin
     SetPath(PItemIDList(lvMyFavorites.Selected.Data));
     CloseNavBarPopup;
  end;
end;

procedure TfmMain.actSchemeExecute(Sender: TObject);
var
  AColorScheme: string;
  AColorSchemeList: TStringList;
begin
  bgColorScheme.SelectedLinkIndex := TComponent(Sender).Tag;
  AColorSchemeList := TStringList.Create;
  try
    (nbMain.ViewStyle as IdxNavBarColorSchemes).PopulateNames(AColorSchemeList);
    AColorScheme := AColorSchemeList[TComponent(Sender).Tag];
  finally
    AColorSchemeList.Free;
  end;

  (nbMain.ViewStyle as IdxNavBarColorSchemes).SetName(AColorScheme);
  (nbMyComputer.ViewStyle as IdxNavBarColorSchemes).SetName(AColorScheme);
  (nbOptions.ViewStyle as IdxNavBarColorSchemes).SetName(AColorScheme);
end;

procedure TfmMain.actLargeIconsExecute(Sender: TObject);
begin
  lvMain.ViewStyle := TViewStyle((Sender as TComponent).Tag);
  CloseNavBarPopup;
end;

procedure TfmMain.nbOptionsAdjustWidthByPopupClick(
  Sender: TObject);
begin
  nbMain.OptionsBehavior.NavigationPane.AdjustWidthByPopup :=
    SwitchOption(Sender, nbMain.OptionsBehavior.NavigationPane.AdjustWidthByPopup);
end;

procedure TfmMain.nbOptionsAllowCustomizeClick(
  Sender: TObject);
begin
  nbMain.OptionsBehavior.NavigationPane.AllowCustomizing :=
    SwitchOption(Sender, nbMain.OptionsBehavior.NavigationPane.AllowCustomizing);
end;

procedure TfmMain.nbOptionsCollapsibleClick(Sender: TObject);
begin
  nbMain.OptionsBehavior.NavigationPane.Collapsible :=
    SwitchOption(Sender, nbMain.OptionsBehavior.NavigationPane.Collapsible);
end;

procedure TfmMain.nbOptionsTabStopClick(Sender: TObject);
begin
  nbMain.TabStop := SwitchOption(Sender, nbMain.TabStop);
  nbMyComputer.TabStop := SwitchOption(Sender, nbMyComputer.TabStop);
  nbOptions.TabStop := SwitchOption(Sender, nbOptions.TabStop);
  if not nbOptions.TabStop then
    lvMain.SetFocus;
end;

procedure TfmMain.tvMyComputerAdvancedCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  bgMyComputerControl.DrawSizeGrip(Sender.Canvas, bgMyComputerControl.GetSizeGripRect(Sender));
end;

procedure TfmMain.tvMyComputerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AControl: TControl;
  ASizeGripRect: TRect;
  APoint: TPoint;
begin
  AControl := TControl(Sender);
  ASizeGripRect := bgMyComputerControl.GetSizeGripRect(AControl);
  APoint := Point(X, Y);
  if cxRectPtIn(ASizeGripRect, APoint) then
    bgMyComputerControl.BeginResize(AControl, Button, Shift, APoint);
end;

procedure TfmMain.tvMyComputerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  AControl: TControl;
  ASizeGripRect: TRect;
  APoint: TPoint;
begin
  AControl := TControl(Sender);
  ASizeGripRect := bgMyComputerControl.GetSizeGripRect(AControl);
  APoint := Point(X, Y);
  if cxRectPtIn(ASizeGripRect, APoint) then
    AControl.Cursor := crSizeWE
  else
    AControl.Cursor := crDefault;
end;

end.
