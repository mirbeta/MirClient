{**************************************************************************}
{ TColumnListBox component                                                 }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ Copyright © 2000 - 2014                                                  }
{   TMS Software                                                           }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit ColListb;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Forms
  , Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 3; // Release nr.
  BLD_VER = 3; // Build nr.

  // version history
  // v1.2.1.0 : Added Items.LoadFromFile, Items.SaveToFile methods
  // v1.2.1.1 : Fixed issue with DeleteSelected
  // v1.2.1.2 : Fixed issue with DeleteSelected & multiselect
  // v1.2.1.3 : Fixed issue with OnDrawItem event
  // v1.2.2.0 : New : exposed mouse events
  // v1.2.2.1 : Fixed : issue with setting item with ColumnItems[]
  // v1.2.2.2 : Fixed : painting issue when width of last column is zero
  // v1.2.3.0 : Improved : Images drawn vertically centered
  // v1.2.3.1 : Fixed : drawing issue after unselecting item
  // v1.2.3.2 : Fixed : Issue with handling OnKeyPress event
  // v1.2.3.3 : Fixed : Issue with directly setting index on list items & column items


type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TColumnListBox = class;

  TColumnType = (ctText,ctImage);

  TEllipsisType = (etAtEnd, etInMiddle, etNone);

  TListBoxColumnItem = class(TCollectionItem)
  private
    FWidth: Integer;
    FAlignment:TAlignment;
    FFont:TFont;
    FColor:TColor;
    FColumnType:TColumnType;
    FEllipsis: TEllipsisType;
    procedure SetWidth(const value: Integer);
    procedure SetAlignment(const value:tAlignment);
    procedure SetFont(const value:TFont);
    procedure SetColor(const value:TColor);
    procedure SetColumnType(const Value: TColumnType);
    procedure SetEllipsis(const Value: TEllipsisType);
  protected
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
  public
    constructor Create(Collection:TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color:TColor read fColor write SetColor;
    property ColumnType:TColumnType read fColumnType write SetColumnType;
    property Ellipsis: TEllipsisType read FEllipsis write SetEllipsis;
    property Width: Integer read fWidth write SetWidth;
    property Alignment:TAlignment read fAlignment write SetAlignment;
    property Font:TFont read fFont write SetFont;
  end;

  TListBoxColumnCollection = class(TCollection)
  private
    FOwner:TColumnListBox;
    function GetItem(Index: Integer): TListBoxColumnItem;
    procedure SetItem(Index: Integer; const Value: TListBoxColumnItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add:TListBoxColumnItem;
    function Insert(index: Integer): TListBoxColumnItem;
    property Items[Index: Integer]: TListBoxColumnItem read GetItem write SetItem; default;
    constructor Create(aOwner:TColumnListBox);
    function GetOwner: tPersistent; override;
  end;

  TListBoxItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    fStrings:TStringList;
    fTag: Integer;
    procedure SetImageIndex(const value: Integer);
    procedure SetStrings(const Value: TStringList);
    procedure StringsChanged(sender:TObject);
  protected
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
  public
    constructor Create(Collection:TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Strings:TStringList read fStrings write SetStrings;
    property Tag: Integer read fTag write fTag;
  end;

  TListBoxItemCollection = class(TCollection)
  private
    FOwner:TColumnListBox;
    function GetItem(Index: Integer): TListBoxItem;
    procedure SetItem(Index: Integer; const Value: TListBoxItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add:TListBoxItem;
    function Insert(index: Integer): TListBoxItem;
    property Items[Index: Integer]: TListBoxItem read GetItem write SetItem; default;
    constructor Create(aOwner:TColumnListBox);
    function GetOwner: tPersistent; override;
    function IndexOf(s:string):tpoint;
    function IndexInColumnOf(col: Integer;s:string): Integer;
    function IndexInRowOf(row: Integer;s:string): Integer;

    {$IFDEF DELPHI_UNICODE}
    procedure SaveToFile(FileName: string; Unicode: boolean = true);
    {$ENDIF}
    {$IFNDEF DELPHI_UNICODE}
    procedure SaveToFile(FileName: string);
    {$ENDIF}
    procedure LoadFromFile(FileName: string);
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TColumnListBox = class(TCustomListBox)
  private
    FImages:TImageList;
    FColumns:TListBoxColumnCollection;
    FListBoxItems:TListBoxItemCollection;
    FGridLines:Boolean;
    FItemIndex: Integer;
    FUpdateCount: Integer;
    FSortColumn: Integer;
    FSortedEx: Boolean;
    FLookupIncr: Boolean;
    FLookupColumn: Integer;
    FLookup: string;
    FShowItemHint: Boolean;
    FLastHintIdx: Integer;
    procedure SetImages(const Value: TImageList);
    procedure SetItemIndexP(const Value : Integer);
    function GetItemIndexP: Integer;
    procedure SetGridLines(const Value: Boolean);
    procedure BuildItems;
    function GetColumnItems(i, j: Integer): String;
    procedure SetColumnItems(i, j: Integer; const Value: String);
    function GetSortedEx: Boolean;
    procedure SetSortedEx(const Value: Boolean);
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    function GetDelimiter: char;
    procedure SetDelimiter(const Value: char);
    procedure QuickSortList(List:TStringList;left,right: Integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer; virtual;
  protected
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    property Items;
    procedure DoEnter; override;
    procedure DoKeypress(var Key: Char); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    {$IFDEF DELPHI6_LVL}
    procedure DeleteSelected; override;
    {$ENDIF}
    procedure TestFill;
    property Text;
    procedure Sort;
    procedure BeginUpdate;
    procedure EndUpdate;
    property ColumnItems[i,j: Integer]: String read GetColumnItems write SetColumnItems;
    property Delimiter: char read GetDelimiter write SetDelimiter;
    procedure InitSample;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DragKind;
    property BorderStyle;
    property Color;
    property Cursor;
    property Ctl3D;
    property Columns: TListBoxColumnCollection read FColumns write FColumns;
    property ListBoxItems: TListBoxItemCollection read fListBoxItems write FListBoxItems;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property GridLines: Boolean read FGridLines write SetGridLines;
    property Images: TImageList read FImages write SetImages;
    property IntegralHeight;
    property ItemHeight;
    property ItemIndex: Integer read GetItemIndexP write SetItemIndexP;
    property LookupIncr: Boolean read fLookupIncr write fLookupIncr;
    property LookupColumn: Integer read fLookupColumn write fLookupColumn;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowItemHint: Boolean read FShowItemHint write FShowItemHint;
    property SortColumn: Integer read fSortColumn write fSortColumn;
    property Sorted: Boolean read GetSortedEx write SetSortedEx;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMeasureItem;
    property OnStartDrag;
    property OnStartDock;
    property OnEndDock;
    property OnContextPopup;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

uses
  ExtCtrls, ShellApi, Commctrl , ImgList ;

const
  COLUMN_DELIMITER = '|';
  FDelimiter : Char = COLUMN_DELIMITER;

var
  SortCol: Integer;

function GetColumnString(var s:string):string;
begin
  if (Pos(FDelimiter,s)>0) then
  begin
    Result := Copy(s,1,Pos(FDelimiter,s)-1);
    Delete(s,1,Pos(FDelimiter,s));
  end
  else
  begin
    Result := s;
    s := '';
  end;
end;

function GetColumn(i: Integer; s:string): String;
var
  k: Integer;
begin
  k := 0;
  repeat
    Result := GetColumnString(s);
    inc(k);
  until (k > i);
end;


procedure TColumnListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  r,dr:TRect;
  s,su:string;
  align:DWORD;
  col,imgidx,err,dx: Integer;
  ct:TColumnType;
begin
  if (Index < 0) then
    Exit;

  r := rect;

  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State);

  if (odSelected in State) then
    Canvas.Rectangle(r.left,r.top,r.right,r.bottom);

  if Index = Self.Items.Count -1 then
    if r.Bottom < Height then
    begin
      r.Top := r.Bottom;
      r.bottom := Height;
      Canvas.Brush.Color := self.Color;
      Canvas.Pen.Color := self.Color;
      Canvas.Rectangle(r.Left,r.Top,r.Right,r.Bottom);
    end;

  r := rect;

  s := Items[index];
  val(GetColumnString(s),imgidx,err);

  for col := 1 to FColumns.Count do
  begin
    ct := FColumns.Items[col - 1].ColumnType;

    if (ct = ctText) then
      su := GetColumnString(s)
    else
      su := '';

    Canvas.Font.Assign(FColumns.Items[col - 1].Font);

    if (odSelected in State) then
    begin
      Canvas.Brush.Color := clHighLight;
      Canvas.Pen.Color := clHighLight;
      Canvas.Font.Color := clHighLightText;
    end
    else
    begin
      Canvas.Brush.Color := FColumns.Items[col - 1].Color;
      Canvas.Pen.Color := Canvas.brush.Color;
    end;

    dr := r;
    dr.right := dr.left + FColumns.Items[col - 1].Width;

    case FColumns.Items[col - 1].Alignment of
    taLeftJustify:align := DT_LEFT;
    taRightJustify:align := DT_RIGHT;
    taCenter:align := DT_CENTER;
    else
      align := DT_LEFT;
    end;

    if not (odSelected in State) then
      Canvas.Rectangle(dr.left,dr.top,dr.right,dr.bottom);

    dr.left := dr.left + 2;
    dr.top := r.top + 1;

    if (ct = ctImage) and Assigned(FImages) then
    begin
      dx := ItemHeight - FImages.Height;
      if dx > 0 then
      begin
        dr.Top := dr.Top + dx div 2;
      end;

      FImages.Draw(Canvas,dr.left,dr.top,imgidx);
    end
    else
    begin
      dr.right := dr.right - 2;
      case FColumns.Items[col-1].Ellipsis of
      etAtEnd: align := align or DT_END_ELLIPSIS or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
      etInMiddle: align := align or DT_PATH_ELLIPSIS or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
      etNone: align := align or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
      end;

      DrawTextEx(Canvas.handle,pchar(su),length(su),dr,align,nil);
      dr.right := dr.right + 2;
    end;
    r.left := dr.right;
  end;

  if (r.Left < Width) and not (odSelected in State) then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Rectangle(r.Left, r.Top, width, r.Bottom);
  end;


  if FGridLines then
  begin
    Canvas.Pen.Color := clGray;

    Canvas.MoveTo(rect.left,rect.bottom - 1);
    Canvas.LineTo(rect.right,rect.bottom - 1);

    imgidx := rect.left;

    for col := 1 to fColumns.Count-1 do
    begin
      imgidx := imgidx + FColumns.Items[col - 1].Width;
      Canvas.moveto(imgidx,dr.top);
      Canvas.lineto(imgidx,dr.bottom);
    end;
  end;
end;

procedure TColumnListBox.CreateWnd;
begin
  inherited CreateWnd;
end;

constructor TColumnListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawFixed;
  FColumns := TListBoxColumnCollection.Create(self);
  FListBoxItems := TListBoxItemCollection.Create(self);
  FUpdateCount := 0;
  FDelimiter := COLUMN_DELIMITER;
end;

destructor TColumnListBox.Destroy;
begin
  FColumns.Free;
  FListBoxItems.Free;
  inherited Destroy;
end;

procedure TColumnListBox.MeasureItem(Index: Integer; var Height: Integer);
var
  Res: Integer;
  Canvas: TCanvas;
begin
  Height := 40;

  if (Index >= 0) then
  begin
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := GetDC(self.Handle);
      res := Canvas.TextHeight('gh') + 4; {some overlap on fonts}
      ReleaseDC(Handle,Canvas.Handle);
    finally
      Canvas.free;
    end;

//   if (index=0) and (fShowHeader) then res:=res*2;
    SendMessage(Handle,CB_SETITEMHEIGHT,Index,Res);
  end
  else
    Res := 20;

  Height := Res;
end;

procedure TColumnListBox.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;


function TColumnListBox.GetItemIndexP: Integer;
begin
  Result := SendMessage(handle,LB_GETCURSEL,0,0);
end;

procedure TColumnListBox.SetItemIndexP(const Value: Integer);
begin
  FItemIndex := value;
  if MultiSelect then
  begin
    SendMessage(handle,LB_SELITEMRANGE,Value,MakeLParam(Value,Value));
  end;
  SendMessage(handle,LB_SETCURSEL,value,0);
end;

procedure TColumnListBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (aOperation = opRemove) and (aComponent = FImages) then
    FImages := nil;
  inherited;
end;

procedure TColumnListBox.SetGridLines(const Value: Boolean);
begin
  fGridLines := Value;
  Invalidate;
end;


procedure TColumnListBox.BuildItems;
var
  i,j: Integer;
  s:string;
begin
  if (csLoading in ComponentState) then
    Exit;

  if (FUpdateCount > 0) then
    Exit;

  // remove if more items in list  
  while (Items.Count > FListBoxItems.Count) do
    Items.Delete(Items.Count - 1);

  for i := 1 to FListBoxItems.Count do
  begin {image index is always first}
   s := IntToStr(FListBoxItems.Items[i-1].FImageIndex);

   for j := 1 to FColumns.Count do
     if (j <= FListBoxItems.Items[i-1].Strings.Count) then
       s := s + FDelimiter + FListBoxItems.Items[i - 1].Strings[j - 1]
     else
       s := s + FDelimiter;

   if (Items.Count >= i) then
      Items[i-1] := s
    else
      Items.Add(s);
  end;
end;

procedure TColumnListBox.Loaded;
begin
  inherited;
  BuildItems;
  ItemIndex := FItemIndex;
  if fSortedEx then Sort;
  DoubleBuffered := True;
end;

procedure TColumnListBox.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TColumnListBox.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      BuildItems;
      if FSortedEx then
        Sort;
    end;
  end;
end;

function TColumnListBox.GetColumnItems(i, j: Integer): String;
var
 k: Integer;
begin
  if (i >= Items.Count) then raise Exception.Create('Item index out of range');

  for k := 1 to j do
   if fColumns.Items[k-1].ColumnType<>ctText then dec(j);

  Result := GetColumn(succ(j), Items[i]);
end;

procedure TColumnListBox.SetColumnItems(i, j: Integer; const Value: String);
var
  s,n,l: String;
  k: Integer;

begin
  if (i >= Items.Count) then
    raise Exception.Create('Item index out of range');

  for k := 1 to j do
    if FColumns.Items[k - 1].ColumnType <> ctText then
      dec(j);

  inc(j);

  s := Items[i];
  k := 0;
  n := '';

  repeat
    if n <> '' then n := n + FDelimiter;
    l := GetColumnString(s);
    if (k <> j) then
      n := n + l
    else
      n := n + Value;
    inc(k);
  until (k > j);

  if (s <> '') then
  begin
    n := n + FDelimiter + s;
  end;

  while j + 1 > ListBoxItems.Items[i].Strings.Count do
  begin
    ListBoxItems.Items[i].Strings.Add('');
  end;

  ListBoxItems.Items[i].Strings[j - 1] := Value;

  Items[i] := n;
end;

function TColumnListBox.GetSortedEx: Boolean;
begin
 Result := fSortedEx;
end;

procedure TColumnListBox.QuickSortList(List:TStringList;left,right: Integer);
var
  i,j,tag,idx: Integer;
  s,sw: string;
  sl:string;

begin
  i := left;
  j := right;

  //get middle item here
  s := List.Strings[(left+right) shr 1];

  repeat
    while (AnsiStrComp(pchar(GetColumn(SortCol,s)),pchar(GetColumn(SortCol,List.Strings[i])))>0) and (i<right) do inc(i);
    while (AnsiStrComp(pchar(GetColumn(SortCol,s)),pchar(GetColumn(SortCol,List.Strings[j])))<0) and (j>left) do dec(j);
    if (i<=j) then
    begin
      if (i<>j) then
      begin
        if AnsiStrComp(pchar(GetColumn(SortCol,List.Strings[i])),pchar(GetColumn(SortCol,List.Strings[j])))<>0 then
        begin
          sw := List.Strings[i];
          List.Strings[i] := List.Strings[j];
          List.Strings[j] := sw;

          sl := ListBoxItems.Items[i].Strings.Text;
          tag := ListBoxItems.Items[i].Tag;
          idx := ListBoxItems.Items[i].ImageIndex;

          ListBoxItems.Items[i].Strings.Text := ListBoxItems.Items[j].Strings.Text;
          ListBoxItems.Items[i].Tag := ListBoxItems.Items[j].Tag;
          ListBoxItems.Items[i].ImageIndex := ListBoxItems.Items[j].ImageIndex;

          ListBoxItems.Items[j].Tag := tag;
          ListBoxItems.Items[j].ImageIndex := idx;
          ListBoxItems.Items[j].Strings.Text := sl;


        end;
      end;
      inc(i);
      dec(j);
    end;
  until (i>j);

  if left < j then QuicksortList(List,left,j);
  if i < right then QuickSortList(List,i,right);
end;


procedure TColumnListBox.Sort;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Assign(Items);
  SortCol := fSortColumn;

  inc(FUpdateCount);

  if sl.Count>1 then
    QuickSortList(sl,0,sl.Count-1);

  dec(FUpdateCount);
  Items.Assign(sl);
  sl.Free;
end;

procedure TColumnListBox.TestFill;
var
  i,j: integer;
  li: TListBoxItem;
begin
  Columns.Clear;
  Columns.Add;
  Columns.Add;
  Columns.Add;

  Columns[0].Width := 80;
  Columns[1].Width := 80;
  Columns[2].Width := 80;

  for i := 1 to 5 do
  begin
    li := ListBoxItems.Add;
    for j := 1 to 3 do
    begin
      li.Strings.Add('Col '+inttostr(j)+': Row '+ inttostr(i));
    end;
  end;
end;

procedure TColumnListBox.SetSortedEx(const Value: Boolean);
begin
  FSortedEx := Value;
  if Value then
    if not (csLoading in ComponentState) then Sort;
end;

procedure TColumnListBox.DoEnter;
begin
  inherited;
  fLookup:='';
end;

procedure TColumnListBox.DoKeypress(var Key: Char);
begin
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);
end;

procedure TColumnListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if key in [vk_up,vk_down,vk_left,vk_right,vk_next,vk_prior,vk_home,vk_end,vk_escape] then
    begin
      fLookup := '';
      Exit;
    end;

  if (key=vk_back) and (length(fLookup)>0) then delete(fLookup,length(fLookup),1);
end;

procedure TColumnListBox.KeyPress(var Key: Char);
var
  i: Integer;
  s: string;

  function Max(a,b: Integer): Integer;
  begin
   if (a > b) then Result := a else Result := b;
  end;

begin
  if not fLookupIncr then
    fLookup := key
  else
    fLookup := fLookup + key;

  if (ItemIndex >= 0) or (FLookupIncr) then
   begin
      for i := Max(1,ItemIndex+1) to Items.Count do
       begin
        s := ColumnItems[i-1,fLookupColumn];
        if (s <> '') then
        if (pos(uppercase(fLookup),uppercase(s)) = 1) then
          begin
           ItemIndex := i - 1;
           Invalidate;
           DoKeyPress(Key);
           Exit;
          end;
       end;
   end;

  for i := 1 to Items.Count do
   begin
    s := ColumnItems[i-1,fLookupColumn];

    if (s <> '') then
    if (pos(uppercase(fLookup),uppercase(s))=1) then
      begin
       ItemIndex := i - 1;
       DoKeyPress(Key);
       Exit;
      end;
   end;

  if fLookupIncr then
   begin
    fLookup:=key;
    for i := 1 to Items.Count do
     begin
      s := ColumnItems[i-1,fLookupColumn];
      if (s<>'') then
      if (pos(uppercase(fLookup),uppercase(s))=1) then
       begin
        ItemIndex := i - 1;
        DoKeyPress(Key);
        Exit;
       end;
     end;
   end;

  inherited;
end;

procedure TColumnListBox.CMHintShow(var Msg: TMessage);
var
  hi: PHintInfo;
  s: string;
  i, idx: Integer;
begin
  hi := PHintInfo(Msg.LParam);

  Idx := SendMessage(Handle,LB_ITEMFROMPOINT,0,makelparam(hi^.cursorpos.x,hi^.cursorpos.y));
  FLastHintIdx := Idx;

  if (Idx >= 0) and (Idx < Items.Count) and FShowItemHint then
  begin

    hi^.HintStr := Items[Idx];
    s := '';
    for i := 1 to Columns.Count do
    begin
      s := s + ' ' + GetColumn(i, hi^.HintStr);
    end;
    hi^.HintStr := s;
  end
  else
    hi^.HintStr := Hint;
end;


procedure TColumnListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Idx: Integer;
begin
  inherited;
  Idx := SendMessage(Handle,LB_ITEMFROMPOINT,0,makelparam(X,Y));
  if Idx <> FLastHintIdx then
  begin
    Application.CancelHint;
    FLastHintIdx := Idx;
  end;
end;


function TColumnListBox.GetDelimiter: char;
begin
  Result := FDelimiter;
end;

procedure TColumnListBox.SetDelimiter(const Value: char);
begin
  FDelimiter := Value;
end;

function TColumnListBox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TColumnListBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TColumnListBox.InitSample;
begin
  Columns.Clear;
  Columns.Add;
  Columns.Add;
  Columns.Add;

  ListBoxItems.Clear;

  ListBoxItems.Add.Strings.CommaText := '0,A,Item X';
  ListBoxItems.Add.Strings.CommaText := '1,B,Item Y';
  ListBoxItems.Add.Strings.CommaText := '2,C,Item Z';
end;

procedure TColumnListBox.SetVersion(const Value: string);
begin

end;

{ TListBoxColumnItem }

procedure TListBoxColumnItem.Assign(Source: TPersistent);
begin
 if Source is TListBoxColumnItem then
  begin
    Color:=TListBoxColumnItem(source).Color;
    ColumnType:=TListBoxColumnItem(source).ColumnType;
    Width:=TListBoxColumnItem(source).Width;
    Alignment:=TListBoxColumnItem(source).Alignment;
    Font.Assign(TListBoxColumnItem(source).Font);
  end;
end;

constructor TListBoxColumnItem.Create(Collection: TCollection);
begin
  inherited;
  FFont := TFont.Create;
  FWidth := 100;
  FColor := clWindow;
end;

destructor TListBoxColumnItem.Destroy;
begin
  FFont.Free;
  Inherited;
end;

function TListBoxColumnItem.GetDisplayName: string;
begin
  Result := 'Column'+inttostr(index);
end;

procedure TListBoxColumnItem.SetAlignment(const value: tAlignment);
begin
  FAlignment := Value;
  TListBoxColumnCollection(collection).FOwner.Invalidate;
end;

procedure TListBoxColumnItem.SetColor(const value: TColor);
begin
  FColor := Value;
  TListBoxColumnCollection(collection).FOwner.Invalidate;
end;

procedure TListBoxColumnItem.SetColumnType(const Value: TColumnType);
begin
  FColumnType := Value;
  TListBoxColumnCollection(collection).FOwner.Invalidate;
end;

procedure TListBoxColumnItem.SetEllipsis(const Value: TEllipsisType);
begin
  FEllipsis := Value;
  TListBoxColumnCollection(collection).FOwner.Invalidate;
end;

procedure TListBoxColumnItem.SetFont(const value: TFont);
begin
  FFont.Assign(value);
  TListBoxColumnCollection(collection).FOwner.Invalidate;
end;

procedure TListBoxColumnItem.SetIndex(Value: Integer);
begin
  inherited;
  TListBoxColumnCollection(collection).FOwner.Invalidate;
end;

procedure TListBoxColumnItem.SetWidth(const value: Integer);
begin
  FWidth := value;
  TListBoxColumnCollection(collection).FOwner.Invalidate;
end;

{ TListBoxColumnCollection }

function TListBoxColumnCollection.Add: TListBoxColumnItem;
begin
 Result:=TListBoxColumnItem(inherited Add);
end;

constructor TListBoxColumnCollection.Create(aOwner: TColumnListBox);
begin
 inherited Create(TListBoxColumnItem);
 FOwner:=aOwner;
end;

function TListBoxColumnCollection.GetItem(Index: Integer): TListBoxColumnItem;
begin
 Result:=TListBoxColumnItem(inherited Items[index]);
end;

function TListBoxColumnCollection.GetOwner: tPersistent;
begin
 Result:=FOwner;
end;

function TListBoxColumnCollection.Insert(index: Integer): TListBoxColumnItem;
begin
 Result:=TListBoxColumnItem(inherited Insert(index));
end;

procedure TListBoxColumnCollection.SetItem(Index: Integer;
  const Value: TListBoxColumnItem);
begin
 inherited SetItem(Index, Value);
end;

procedure TListBoxColumnCollection.Update(Item: TCollectionItem);
begin
  inherited;
end;


{ TListBoxItemCollection }

function TListBoxItemCollection.Add: TListBoxItem;
begin
 Result:=TListBoxItem(inherited Add);
end;

constructor TListBoxItemCollection.Create(aOwner: TColumnListBox);
begin
 inherited Create(TListBoxItem);
 FOwner:=aOwner;
end;

function TListBoxItemCollection.GetItem(Index: Integer): TListBoxItem;
begin
 Result:=TListBoxItem(inherited Items[index]);
end;

function TListBoxItemCollection.GetOwner: tPersistent;
begin
 Result:=FOwner;
end;

function TListBoxItemCollection.Insert(index: Integer): TListBoxItem;
begin
 Result:=TListBoxItem(inherited Insert(index));
end;

function TListBoxItemCollection.IndexInColumnOf(col: Integer;
  s: string): Integer;
var
 i: Integer;
begin
 Result:=-1;
 for i:=1 to Count do
  begin
   if Items[i-1].Strings.Count>col then
     if Items[i-1].Strings[col]=s then
       begin
        Result := i-1;
        break;
       end;
  end;

end;

function TListBoxItemCollection.IndexInRowOf(row: Integer;
  s: string): Integer;
var
 i: Integer;
begin
 Result:=-1;
 if (Count>Row) then

 for i:=1 to Items[row].Strings.Count do
  begin
   if Items[row].Strings[i-1]=s then
     begin
      Result:=i-1;
      break;
     end;
  end;

end;


function TListBoxItemCollection.IndexOf(s: string): tpoint;
var
 i,j: Integer;

begin
 Result:=point(-1,-1);

 for i:=1 to Count do
  begin
   for j:=1 to Items[i-1].Strings.Count do

     if Items[i-1].Strings[j-1]=s then
       begin
        Result.y:=i-1;
        Result.x:=j-1;
        break;
       end;
  end;

end;


procedure TListBoxItemCollection.SetItem(Index: Integer;
  const Value: TListBoxItem);
begin
 inherited SetItem(Index, Value);
end;

procedure TListBoxItemCollection.Update(Item: TCollectionItem);
begin
  inherited;
end;

procedure TListBoxItemCollection.LoadFromFile(FileName: string);
var
  {$IFNDEF DELPHI_UNICODE}
  tf: textfile;
  s:string;
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  sl: TStringList;
  i: integer;
  {$ENDIF}
begin
  Clear;
  {$IFNDEF DELPHI_UNICODE}
  Assignfile(tf, FileName);
  {$i-}
  Reset(tf);
  {$i+}
  if (ioresult = 0) then
  begin
    BeginUpdate;
    while not eof(tf) do
    begin
      readln(tf, s);
      Add.Strings.CommaText := s;
    end;
    EndUpdate;
    closefile(tf);
  end;
  {$ENDIF}

  {$IFDEF DELPHI_UNICODE}
  sl := TStringList.Create;

  try
    sl.LoadFromFile(FileName);
    BeginUpdate;
    for i := 0 to sl.Count - 1 do
    begin
      Add.Strings.CommaText := sl.Strings[i];
    end;
    EndUpdate;

  finally
    sl.Free;
  end;
  {$ENDIF}

end;

{$IFNDEF DELPHI_UNICODE}
procedure TListBoxItemCollection.SaveToFile(FileName: string);
{$ENDIF}
{$IFDEF DELPHI_UNICODE}
procedure TListBoxItemCollection.SaveToFile(FileName: string; Unicode: boolean = true);
{$ENDIF}
var
  i: integer;
  {$IFNDEF DELPHI_UNICODE}
  tf: textfile;
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  sl: TStringList;
  {$ENDIF}

begin
  {$IFNDEF DELPHI_UNICODE}
  assignfile(tf, FileName);
  rewrite(tf);
  for i := 1 to Count do
  begin
    writeln(tf, Items[i - 1].Strings.CommaText);
  end;
  closefile(tf);
  {$ENDIF}

  {$IFDEF DELPHI_UNICODE}
  sl := TStringList.Create;
  for i := 1 to Count do
  begin
    sl.Add(Items[i - 1].Strings.CommaText);
  end;

  if Unicode then
    sl.SaveToFile(FileName, TEncoding.Unicode)
  else
    sl.SaveToFile(FileName);

  sl.Free;
  {$ENDIF}
end;

{ TListBoxItem }

procedure TListBoxItem.Assign(Source: TPersistent);
begin
  if Source is TListBoxItem then
  begin
    ImageIndex := TListBoxItem(Source).ImageIndex;
    Strings.Assign(TListBoxItem(Source).Strings);
    TListBoxItemCollection(collection).FOwner.BuildItems;
  end;
end;

constructor TListBoxItem.Create(Collection: TCollection);
begin
  inherited;
  FStrings := TStringList.Create;
  FImageIndex := -1;
  FStrings.OnChange := StringsChanged;
end;

destructor TListBoxItem.Destroy;
var
  AOwner: TColumnListBox;
begin
  AOwner := TListBoxItemCollection(collection).FOwner;
  fStrings.Free;
  inherited;
  if AOwner.HandleAllocated then AOwner.BuildItems;
end;

function TListBoxItem.GetDisplayName: string;
begin
  Result := 'Item' + IntToStr(Index);
end;

procedure TListBoxItem.SetImageIndex(const value: Integer);
begin
  FImageIndex := Value;

  with TListBoxItemCollection(Collection).FOwner do
  begin
    if FUpdateCount > 0 then Exit;
    if (csDesigning in ComponentState) then BuildItems;
    Invalidate;
  end;
end;

procedure TListBoxItem.SetIndex(Value: Integer);
begin
  inherited;
  StringsChanged(Self);
end;

procedure TListBoxItem.SetStrings(const Value: TStringList);
begin
  FStrings.Assign(Value);
  TListBoxItemCollection(collection).FOwner.Invalidate;
end;

procedure TListBoxItem.StringsChanged(Sender: TObject);
var
  idx: Integer;
begin
  if TListBoxItemCollection(Collection).FOwner.FUpdateCount > 0 then
    Exit;

  idx := TListBoxItemCollection(Collection).FOwner.ItemIndex;
  TListBoxItemCollection(Collection).FOwner.BuildItems;
  TListBoxItemCollection(Collection).FOwner.ItemIndex := idx;
end;

procedure TColumnListBox.WMSize(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

{$IFDEF DELPHI6_LVL}
procedure TColumnListBox.DeleteSelected;
var
  i: integer;
begin
  if MultiSelect then
  begin
    BeginUpdate;
    for i := Items.Count - 1 downto 0 do
    begin
      if Selected[i] then
        ListBoxItems[i].Free;
    end;
    EndUpdate;
  end
  else
    if ItemIndex >= 0 then
      ListBoxItems[ItemIndex].Free;
end;
{$ENDIF}

{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}

end.
