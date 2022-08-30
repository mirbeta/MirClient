{*************************************************************************}
{ TImagePicker component                                                  }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ Copyright © 2000-2012                                                   }
{   TMS Software                                                          }
{   Email : info@tmssoftware.com                                          }
{   Web : http://www.tmssoftware.com                                      }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit ImagePicker;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, AdvCombo,
  Forms, Dialogs
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  COLUMN_DELIMITER = '|';
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // 1.1.0.0 : Added property ShowCaption to show caption always in the edit part of the dropdown
  // 1.1.0.1 : Fixed : issue with EditHeight
  // 1.1.1.0 : Added new method SelectByTag
  // 1.1.2.0 : Improved : ImageIndex not reset after calling EndUpdate
  // 1.1.2.1 : Fixed : Handling SelectByTag in sorted list


type
  TImagePicker = class;

  TImagePickerItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FCaption: string;
    FTag: Integer;
    procedure SetImageIndex(const Value: Integer);
    procedure SetCaption(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection:TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Caption: string read FCaption write SetCaption;
    property Tag: Integer read FTag write FTag;
  end;

  TImagePickerItemCollection = class(TCollection)
  private
    FOwner:TImagePicker;
    function GetItem(Index: Integer): TImagePickerItem;
    procedure SetItem(Index: Integer; const Value: TImagePickerItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add:TImagePickerItem;
    function Insert(index: Integer): TImagePickerItem;
    property Items[Index: Integer]: TImagePickerItem read GetItem write SetItem; default;
    constructor Create(AOwner:TImagePicker);
    function GetOwner: tPersistent; override;
    function IndexOf(s:string): Integer;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TImagePicker = class(TAdvCustomCombo)
  private
    FImages:TImageList;
    FDropHeight: Integer;
    FEditHeight: Integer;
    FImagePickerItems:TImagePickerItemCollection;
    FEditColumn: Integer;
    FItemIndex: Integer;
    FUpdateCount: Integer;
    FLookup: string;
    FLookupIncr: Boolean;
    FSortedEx: Boolean;
    FDropped: Boolean;
    FShowCaption: Boolean;
    procedure SetEditHeight(Value: Integer);
    function GetEditHeight: Integer;
    procedure SetImages(const Value: TImageList);
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMLButtonUp(var Msg:TWMLButtonDown); message WM_LBUTTONUP;
    procedure WMChar(var Msg:TWMChar); message WM_CHAR;
    procedure WMSize(var Msg:TWMSize); message WM_SIZE;
    procedure SetItemIndexEx(const Value : Integer);
    function GetItemIndexEx: Integer;
    procedure BuildItems;
    function GetSortedEx: boolean;
    procedure SetSortedEx(const Value: boolean);
    procedure Sort;
    function GetDelimiter: Char;
    procedure SetDelimiter(const Value: Char);
    function GetComboItems: TStrings;
    procedure SetComboItems(const Value: TStrings);
    function GetSelection: TImagePickerItem;
    function GetCaption(Value: string):string;
    procedure SetShowCaption(const Value: Boolean);
  protected
    function GetVersionNr: Integer; override;
    procedure SetStyle(Value: TComboBoxStyle); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    property ComboItems: TStrings read GetComboItems write SetComboItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    property Text;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SelectByCaption(const Value: string);
    procedure SelectByImageIdx(const Value: Integer);
    procedure SelectByTag(const Value: Integer);
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property Selection: TImagePickerItem read GetSelection;
  published
    property Anchors;
    property Constraints;
    property DragKind;
    property Color;
    property Ctl3D;
    property Items: TImagePickerItemCollection read FImagePickerItems write FImagePickerItems;
    property DragMode;
    property DragCursor;
    property EditHeight: Integer read GetEditheight write SetEditHeight;
    property DropHeight: Integer read fDropHeight write fDropHeight;
    property DropWidth;
    property Images: TImageList read FImages write SetImages;
    property Enabled;
    property Etched;
    property Flat;
    property FlatLineColor;
    property FocusBorder;
    property Font;
  //    property ItemHeight;
    property ItemIndex: Integer read GetItemIndexEx write SetItemIndexEx;
    property ItemHeight;
    property LookupIncr: Boolean read FLookupIncr write FLookupIncr default False;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default False;
    property Sorted: Boolean read GetSortedEx write SetSortedEx;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
    property OnStartDock;
    property OnEndDock;
    property OnContextPopup;
  end;

implementation
uses
  ExtCtrls,ShellApi,CommCtrl  ,ImgList ;

const
  FDelimiter : Char = COLUMN_DELIMITER;

var
  SortCol: Integer;



procedure TImagePicker.SetStyle(Value: TComboBoxStyle);
begin
  inherited SetStyle(csOwnerDrawFixed);
end;

function GetColumnString(var s:string):string;
begin
  if (Pos(FDelimiter,s) > 0) then
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

procedure TImagePicker.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  r,dr:TRect;
  s,su:string;
  ImgIdx,Err: Integer;
  isEdit:boolean;
begin
  isEdit := odComboBoxEdit in State;

  r := Rect;

  if odSelected in State then
    Canvas.rectangle(r.left,r.top,r.right,r.bottom);


  if odSelected in State then
  begin
    Canvas.Brush.Color := clHighLight;
    Canvas.Pen.Color := clHighLight;
    Canvas.Font.Color := clHighLightText;
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.color := Canvas.Brush.Color;
  end;

  dr := r;

  Canvas.Rectangle(dr.Left,dr.Top,dr.Right,dr.Bottom);

  if index < 0 then
    Exit;


  dr.Left := dr.Left + 2;

  s := ComboItems[Index];

  su := Copy(s,1,Pos(FDelimiter,s)-1);

  Val(su,ImgIdx,Err);

  Delete(s,1,Pos(FDelimiter,s));

  su := Copy(s,1,Pos(FDelimiter,s)-1);

  if Assigned(FImages) then
    FImages.Draw(Canvas,dr.left,dr.top,imgidx);

  if not isEdit or ShowCaption then
  begin
    if Assigned(FImages) then
      dr.left := dr.left + FImages.Width + 4;

    DrawTextEx(Canvas.handle,pchar(su),length(su),dr,DT_LEFT or DT_END_ELLIPSIS or DT_VCENTER or DT_SINGLELINE,nil);

  end;

  Canvas.Brush.color := Color;
  Canvas.Pen.color := Color;
end;

procedure TImagePicker.CreateWnd;
begin
  inherited CreateWnd;
  if EditHeight = 0 then
    EditHeight := 18;
end;

constructor TImagePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDropheight := 200;
  FEditColumn := -1;
  FImagePickerItems := TImagePickerItemCollection.Create(self);
  Flat := false;
  FDelimiter := COLUMN_DELIMITER;
  Width := 48;
  Style := csOwnerDrawFixed;
end;

destructor TImagePicker.Destroy;
begin
  FImagePickerItems.Free;
  inherited Destroy;
end;

procedure TImagePicker.MeasureItem(Index: Integer; var Height: Integer);
var
  Res: Integer;
  Canvas: TCanvas;
begin
  Height := 40;
  if (index >= 0) then
  begin
    Canvas := TCanvas.Create;
    Canvas.Handle := GetDC(self.Handle);
    res := Canvas.TextHeight('gh') + 4; {avoid overlap on fonts}
    ReleaseDC(handle,Canvas.handle);
    Canvas.Free;
    Sendmessage(self.Handle,CB_SETITEMHEIGHT,index,res);
  end
  else
  begin
    res := EditHeight;
  end;

  Height := res;
end;


function TImagePicker.GetEditHeight: Integer;
begin
//  if (csLoading in ComponentState) then
    Result := FEditHeight
//  else
//    Result := SendMessage(Handle,CB_GETITEMHEIGHT,-1,0)
end;

procedure TImagePicker.SetEditHeight(Value: Integer);
begin
  FEditHeight := Value;
  SendMessage(Handle,CB_SETITEMHEIGHT,-1,Value);
  SendMessage(Handle,CB_SETITEMHEIGHT,0,Value);
end;

procedure TImagePicker.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;

procedure TImagePicker.CNCommand(var Message: TWMCommand);
begin
  inherited;

  case message.NotifyCode of
  CBN_DROPDOWN:
    begin
      MoveWindow(self.Handle,self.Left,self.Top,width,EditHeight + FDropheight,True);
      DropDown;
      FDropped := True;
      ItemIndex := SendMessage(self.Handle,CB_GETCURSEL,0,0);
      message.Result := 0;
      if Assigned(OnClick) then
        OnClick(Self);
    end;
  CBN_SELCHANGE:
    begin
      FDropped := False;
      FItemIndex := SendMessage(self.Handle,CB_GETCURSEL,0,0);
      if Assigned(OnChange) then
        OnChange(Self);
    end;
  else
    inherited;
  end;

end;

function TImagePicker.GetItemIndexEx: Integer;
begin
  Result := SendMessage(Handle,CB_GETCURSEL,0,0);
end;

procedure TImagePicker.SetItemIndexEx(const Value: Integer);
begin
  if FDropped then FItemIndex := Value;
  SendMessage(Handle,CB_SETCURSEL,Value,0);
end;

procedure TImagePicker.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (aOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;
  inherited;
end;

procedure TImagePicker.BuildItems;
var
  i: Integer;
  s: string;
begin
  if (csLoading in ComponentState) then
    Exit;

  while ComboItems.Count > FImagePickerItems.Count do
    ComboItems.Delete(ComboItems.Count - 1);

  for i := 1 to FImagePickerItems.Count do
  begin
    with FImagePickerItems.Items[i - 1] do
    s := Inttostr(FImageIndex) + FDelimiter + FCaption + FDelimiter + IntToStr(i-1);

    if ComboItems.Count >= i then
    begin
      ComboItems[i-1] := s;
      ComboItems.Objects[i - 1] := FImagePickerItems.Items[i-1];
    end
    else
    begin
      ComboItems.AddObject(s, FImagePickerItems.Items[i-1]);
    end;
  end;
end;

procedure TImagePicker.Loaded;
var
  eh: integer;
begin
  inherited;
  eh := FEditHeight;
  BuildItems;
  ItemIndex := FItemIndex;
  if FSortedEx then
    Sort;

  EditHeight := eh;
end;

procedure TImagePicker.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TImagePicker.EndUpdate;
var
 idx: integer;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      idx := ItemIndex;
      BuildItems;
      ItemIndex := idx;
    end;
  end;
end;

procedure TImagePicker.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if key in [vk_up,vk_down,vk_left,vk_right,vk_next,vk_prior,vk_home,vk_end,vk_escape] then
  begin
    FLookup := '';
    Exit;
  end;

  if (Key = vk_back) and (Length(FLookup)>0) then
    Delete(FLookup,Length(FLookup),1);
end;

function TImagePicker.GetSortedEx: boolean;
begin
  Result := FSortedEx;
end;

function ColumnCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
 Result := AnsiStrComp(pchar(GetColumn(SortCol, List.Strings[Index1] )),pchar(GetColumn(SortCol, List.Strings[Index2])));
end;

procedure TImagePicker.Sort;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Assign(ComboItems);
  SortCol := 1;

  sl.CustomSort(ColumnCompare);

  ComboItems.Assign(sl);
  sl.Free;
end;

procedure TImagePicker.SetSortedEx(const Value: boolean);
begin
  fSortedEx := Value;
  if Value then
    if not (csLoading in ComponentState) then Sort;
end;

procedure TImagePicker.WMLButtonUp(var Msg:TWMLButtonDown);
begin
  inherited;
  if FDropped then
  begin
    ItemIndex := fItemIndex;
    if SendMessage(self.Handle,CB_GETDROPPEDSTATE,0,0)=0 then
        FDropped := false;
  end;
end;

function TImagePicker.GetDelimiter: Char;
begin
  Result := FDelimiter;
end;

procedure TImagePicker.SetDelimiter(const Value: Char);
begin
  FDelimiter := Value;
end;

procedure TImagePicker.WMChar(var Msg: TWMChar);
var
  i: Integer;
  s: string;
  Key: Char;

  function Max(a,b: Integer): Integer;
  begin
   if (a > b) then
     Result := a
   else
     Result := b;
  end;

begin
  inherited;

  Key := Chr(Msg.CharCode);

  if not FLookupIncr then
    FLookup := Key
  else
    FLookup := FLookup + Key;

  if (ItemIndex >= 0) or (FLookupIncr) then
  begin
    for i := Max(1,ItemIndex+1) to Items.Count do
    begin
      s := GetCaption(ComboItems[i-1]);
      if s <> '' then
        if (Pos(AnsiUpperCase(FLookup),AnsiUpperCase(s)) = 1) then

        begin
          ItemIndex := i-1;
          Exit;
        end;

    end;
  end;

  for i := 1 to Items.Count do
  begin
    s := GetCaption(ComboItems[i-1]);
    if s <> '' then
      if (Pos(Uppercase(FLookup),Uppercase(s))=1) then
      begin
        ItemIndex := i-1;
        Exit;
      end;
  end;

  if FLookupIncr then
  begin
    FLookup := Key;
    for i := 1 to Items.Count do
    begin
      s := GetCaption(ComboItems[i-1]);
      if s <> '' then
        if (pos(AnsiUpperCase(FLookup),AnsiUpperCase(s))=1) then
        begin
          ItemIndex := i-1;
          Exit;
        end;
    end;
  end;
end;

function TImagePicker.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

{ TImagePickerItemCollection }

function TImagePickerItemCollection.Add: TImagePickerItem;
begin
  Result := TImagePickerItem(inherited Add);
end;

constructor TImagePickerItemCollection.Create(AOwner: TImagePicker);
begin
  inherited Create(TImagePickerItem);
  FOwner := AOwner;
end;

function TImagePickerItemCollection.GetItem(Index: Integer): TImagePickerItem;
begin
  Result := TImagePickerItem(inherited Items[index]);
end;

function TImagePickerItemCollection.GetOwner: tPersistent;
begin
  Result := FOwner;
end;

function TImagePickerItemCollection.IndexOf(s: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 1 to Count do
  begin
    if Items[i - 1].Caption = s then
    begin
      Result := i - 1;
      Break;
    end;
  end;
end;

function TImagePickerItemCollection.Insert(Index: Integer): TImagePickerItem;
begin
  Result := TImagePickerItem(inherited Insert(Index));
End;

procedure TImagePickerItemCollection.SetItem(Index: Integer;
  const Value: TImagePickerItem);
begin
 inherited SetItem(Index, Value);
end;

procedure TImagePickerItemCollection.Update(Item: TCollectionItem);
begin
  inherited;
end;

{ TImagePickerItem }

procedure TImagePickerItem.Assign(Source: TPersistent);
begin
  if Source is TImagePickerItem then
  begin
    ImageIndex := TImagePickerItem(Source).ImageIndex;
    Caption := TImagePickerItem(Source).Caption;
    Tag := TImagePickerItem(Source).Tag;
    TImagePickerItemCollection(collection).FOwner.BuildItems;
  end;
end;

constructor TImagePickerItem.Create(Collection: TCollection);
begin
  inherited;
  FCaption := '';
  FImageIndex := -1;
end;

destructor TImagePickerItem.Destroy;
var
  AOwner: TImagePicker;
begin
  AOwner := TImagePickerItemCollection(Collection).FOwner;
  inherited;
  if AOwner.HandleAllocated then AOwner.BuildItems;
end;

function TImagePickerItem.GetDisplayName: string;
begin
  Result := 'Item'+inttostr(index);
end;

procedure TImagePickerItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  TImagePickerItemCollection(collection).FOwner.Invalidate;
end;

procedure TImagePickerItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    TImagePickerItemCollection(Collection).FOwner.Invalidate;
  end;
end;


function TImagePicker.GetComboItems: TStrings;
begin
  Result := inherited Items;
end;

procedure TImagePicker.SetComboItems(const Value: TStrings);
begin
  with inherited Items do
    Assign(Value);
end;

function TImagePicker.GetSelection: TImagePickerItem;
var
  Idx,Err: Integer;
  s: string;
begin
  Result := nil;
  if ItemIndex > -1 then
  begin
    s := ComboItems[ItemIndex];
    Delete(s,1,Pos(FDelimiter,s));
    Delete(s,1,Pos(FDelimiter,s));
    val(s,Idx,Err);
    if (Err = 0) and (Idx < Items.Count) then
      Result := Items.Items[Idx];
  end;
end;

procedure TImagePicker.SelectByCaption(const Value: string);
var
  i: Integer;
  s: string;
begin
  for i := 1 to Items.Count do
  begin
    s := GetCaption(ComboItems[i - 1]);
    if s = Value then
    begin
      ItemIndex := i - 1;
      Break;
    end;
  end;

end;

procedure TImagePicker.SelectByImageIdx(const Value: Integer);
var
  i: Integer;
  s: string;
begin
  for i := 1 to Items.Count do
  begin
    s := ComboItems[i - 1];
    s := Copy(s,1,Pos(FDelimiter,s) - 1);
    if s = IntToStr(Value) then
    begin
      ItemIndex := i - 1;
      Break;
    end;
  end;
end;

procedure TImagePicker.SelectByTag(const Value: Integer);
var
  i: integer;
begin
  for i := 0 to ComboItems.Count - 1 do
  begin
    if TImagePickerItem(ComboItems.Objects[i]).Tag = Value then
    begin
      ItemIndex := i;
      Break;
    end;
  end;
end;

function TImagePicker.GetCaption(Value: string): string;
begin
  Delete(Value,1,Pos(FDelimiter,Value));
  Result := Copy(Value,1,Pos(FDelimiter,Value) - 1);
end;


procedure TImagePicker.WMSize(var Msg: TWMSize);
begin
  inherited;

  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    EditHeight := FEditHeight;
end;

procedure TImagePicker.SetShowCaption(const Value: Boolean);
begin
  FShowCaption := Value;
  Invalidate;
end;

end.
