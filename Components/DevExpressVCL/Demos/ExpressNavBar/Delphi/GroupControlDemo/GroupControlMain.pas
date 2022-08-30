unit GroupControlMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ToolWin, ShlObj, ImgList, Menus, ExtCtrls,
  dxNavBarBase, dxNavBarCollns, dxNavBar, Grids, cxClasses;

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

  TfmGroupControlMain = class(TForm)
    lvMain: TListView;
    CoolBar1: TCoolBar;
    ToolBar2: TToolBar;
    ToolbarImages: TImageList;
    btnLargeIcons: TToolButton;
    btnSmallIcons: TToolButton;
    btnList: TToolButton;
    btnReport: TToolButton;
    ToolButton3: TToolButton;
    btnBack: TToolButton;
    nbMain: TdxNavBar;
    bgPlaces: TdxNavBarGroup;
    bgSearch: TdxNavBarGroup;
    bgSearchControl: TdxNavBarGroupControl;
    btnSearch: TButton;
    StatusBar1: TStatusBar;
    ilSmall: TImageList;
    ilLarge: TImageList;
    biDesktop: TdxNavBarItem;
    biMyDocuments: TdxNavBarItem;
    biNetwork: TdxNavBarItem;
    edSearch: TEdit;
    bgFavorites: TdxNavBarGroup;
    bgMyComputer: TdxNavBarGroup;
    bgMyComputerControl: TdxNavBarGroupControl;
    bgFavoritesControl: TdxNavBarGroupControl;
    lvMyFavorites: TListView;
    tvMyComputer: TTreeView;
    Splitter1: TSplitter;
    ToolButton1: TToolButton;
    cbViews: TComboBox;
    Panel1: TPanel;
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
    procedure biDesktopClick(Sender: TObject);
    procedure biMyDocumentsClick(Sender: TObject);
    procedure biNetworkClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbViewsChange(Sender: TObject);
    procedure lvMyFavoritesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure tvMyComputerChange(Sender: TObject; Node: TTreeNode);
  private
    FIDList: TList;
    FSearchShellID: PItemIDList;
    FShellID: PItemIDList;
    FDesktopFolder: IShellFolder;
    FSearching: Boolean;

    function GetShellFolder: IShellFolder;
    function GetShellItemCount: Integer;
    function GetShellItem(Index: Integer): PShellItem;

    procedure ClearIDList;
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
  fmGroupControlMain: TfmGroupControlMain;

implementation

{$R *.dfm}

uses dxNavBarViewsFact, ShellAPI, ActiveX, ComObj, CommCtrl;

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
  Result := SmallInt(fmGroupControlMain.ShellFolder.CompareIDs(0,
    PShellItem(Item1).ID, PShellItem(Item2).ID));
end;

{TForm1}

procedure TfmGroupControlMain.FormCreate(Sender: TObject);
var
  I: Integer;
  FileInfo: TSHFileInfo;
  ImageListHandle: THandle;
  NewPIDL: PItemIDList;
begin
  cbViews.Items.Clear;
  for I := 0 to dxNavBarViewsFactory.Count - 1 do
     cbViews.Items.Add(dxNavBarViewsFactory.Names[I]);
  cbViews.ItemIndex := dxNavBarViewsFactory.IndexOfID(nbMain.View);

  OLECheck(SHGetDesktopFolder(FDesktopFolder));
  FIDList := TList.Create;

  ImageListHandle := SHGetFileInfo('C:\', 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  SendMessage(lvMain.Handle, LVM_SETIMAGELIST, LVSIL_SMALL, ImageListHandle);
  SendMessage(lvMyFavorites.Handle, LVM_SETIMAGELIST, LVSIL_SMALL, ImageListHandle);
  SendMessage(tvMyComputer.Handle, TVM_SETIMAGELIST, TVSIL_NORMAL, ImageListHandle);

  ImageListHandle := SHGetFileInfo('C:\', 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  SendMessage(lvMain.Handle, LVM_SETIMAGELIST, LVSIL_NORMAL, ImageListHandle);
  SendMessage(lvMyFavorites.Handle, LVM_SETIMAGELIST, LVSIL_NORMAL, ImageListHandle);

  OLECheck(SHGetSpecialFolderLocation(Application.Handle, CSIDL_DRIVES, NewPIDL));
  FShellID := NewPIDL;
  SetPath(NewPIDL);
  PopulateMyFavoritesList(GetIDBySpetialFolder(CSIDL_FAVORITES));
  PopulateMyComputerTree(NewPIDL);
end;

procedure TfmGroupControlMain.btnLargeIconsClick(Sender: TObject);
var
  FileInfo: TSHFileInfo;
  ImageListHandle: THandle;
begin
  lvMain.ViewStyle := TViewStyle((Sender as TComponent).Tag);
  ImageListHandle := SHGetFileInfo('C:\', 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  SendMessage(lvMain.Handle, LVM_SETIMAGELIST, LVSIL_SMALL, ImageListHandle);
  ImageListHandle := SHGetFileInfo('C:\', 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  SendMessage(lvMain.Handle, LVM_SETIMAGELIST, LVSIL_NORMAL, ImageListHandle);
end;

procedure TfmGroupControlMain.lvMainDblClick(Sender: TObject);
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

function TfmGroupControlMain.GetShellFolder: IShellFolder;
begin
  Result := GetShellFolderByID(FShellID);
end;

function TfmGroupControlMain.GetShellItemCount: Integer;
begin
  Result := FIDList.Count;
end;

function TfmGroupControlMain.GetShellItem(Index: Integer): PShellItem;
begin
  Result := PShellItem(FIDList[Index]);
end;

procedure TfmGroupControlMain.lvMainKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      lvMainDblClick(Sender);
    VK_BACK:
      btnBackClick(Sender);  
  end;
end;

procedure TfmGroupControlMain.ClearIDList;
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

procedure TfmGroupControlMain.PopulateIDList(AID: PItemIDList);
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

procedure TfmGroupControlMain.PopulateSearchIDList(ASearchID: PItemIDList; Pattern: string);

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

procedure TfmGroupControlMain.PopulateMyFavoritesList(AID: PItemIDList);
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

procedure TfmGroupControlMain.PopulateMyComputerTree(AID: PItemIDList);
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

function TfmGroupControlMain.GetIDByPath(APath: string): PItemIDList;
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

function TfmGroupControlMain.GetIDBySpetialFolder(ASpetialFolder: Integer): PItemIDList;
begin
  OLECheck(SHGetSpecialFolderLocation(Application.Handle, ASpetialFolder, Result));
end;

function TfmGroupControlMain.GetShellFolderByID(AID: PItemIDList): IShellFolder;
begin
   if AID <> nil then
     OLECheck(DesktopFolder.BindToObject(AID, nil, IID_IShellFolder, Pointer(Result)))
   else Result := nil;
end;

function TfmGroupControlMain.GetEnumIDListByFolder(AFolder: IShellFolder): IEnumIDList;
const
  Flags = SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN;
begin
  if AFolder <> nil then
    OleCheck(AFolder.EnumObjects(Application.Handle, Flags, Result))
  else Result := nil;
end;

function TfmGroupControlMain.CompareNames(Path, Pattern: string): Boolean;
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

procedure TfmGroupControlMain.SetSearch(AID: PItemIDList; const Pattern: string);
begin
  lvMain.Items.BeginUpdate;
  try
    FSearching := True;
    btnSearch.Caption := 'Stop';
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
      btnSearch.Caption := 'Search';
      FSearching := False;
    end;
  finally
    lvMain.Items.EndUpdate;
  end;
end;

procedure TfmGroupControlMain.SetPath(const Value: string);
var
  NewPIDL: PItemIDList;
begin
  NewPIDL := GetIDByPath(Value);
  SetPath(NewPIDL);
end;

procedure TfmGroupControlMain.SetPath(ID: PItemIDList);
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

procedure TfmGroupControlMain.CheckShellItems(StartIndex, EndIndex: Integer);

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

procedure TfmGroupControlMain.lvMainDataHint(Sender: TObject; StartIndex,
  EndIndex: Integer);
begin
  if (StartIndex > ShellItemCount) or (EndIndex > ShellItemCount) then Exit;
  CheckShellItems(StartIndex, EndIndex);
end;

procedure TfmGroupControlMain.lvMainData(Sender: TObject; Item: TListItem);
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

procedure TfmGroupControlMain.lvMainDataFind(Sender: TObject; Find: TItemFind;
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

procedure TfmGroupControlMain.lvMainCustomDrawItem(Sender: TCustomListView;
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

procedure TfmGroupControlMain.lvMainCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if SubItem = 0 then Exit;
  lvMain.Canvas.Font.Color := GetSysColor(COLOR_WINDOWTEXT);
end;

procedure TfmGroupControlMain.btnBackClick(Sender: TObject);
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
end;

procedure TfmGroupControlMain.Form1Close(Sender: TObject; var Action: TCloseAction);
begin
  FSearching := False;
end;

procedure TfmGroupControlMain.btnSearchClick(Sender: TObject);
begin
  if not FSearching then
    SetSearch(FShellID, edSearch.Text)
  else FSearching := False;
end;

procedure TfmGroupControlMain.biDesktopClick(Sender: TObject);
begin
  SetPath(GetIDBySpetialFolder(CSIDL_DESKTOPDIRECTORY));
end;

procedure TfmGroupControlMain.biMyDocumentsClick(Sender: TObject);
begin
  SetPath(GetIDBySpetialFolder(CSIDL_PERSONAL));
end;

procedure TfmGroupControlMain.biNetworkClick(Sender: TObject);
begin
  SetPath(GetIDBySpetialFolder(CSIDL_NETWORK));
end;

procedure TfmGroupControlMain.FormDestroy(Sender: TObject);
begin
  ClearIDList;
  FIDList.Free;
end;

procedure TfmGroupControlMain.cbViewsChange(Sender: TObject);
begin
  nbMain.View := dxNavBarViewsFactory.IDs[cbViews.ItemIndex];
end;

procedure TfmGroupControlMain.lvMyFavoritesChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if (lvMyFavorites.Selected <> nil) and (lvMyFavorites.Selected.Data <> nil) then
     SetPath(PItemIDList(lvMyFavorites.Selected.Data));
end;

procedure TfmGroupControlMain.tvMyComputerChange(Sender: TObject;
  Node: TTreeNode);
begin
   if (tvMyComputer.Selected <> nil) and (tvMyComputer.Selected.Data <> nil) then
     SetPath(PItemIDList(tvMyComputer.Selected.Data));
end;

end.
