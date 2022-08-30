{***************************************************************************}
{ TDBAdvSmoothImageListBox component                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2010                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

{$I TMSDEFS.INC}

unit DBAdvSmoothImageListBox;

interface

uses
  Windows, Classes, StdCtrls, SysUtils, Graphics, DB, DBCtrls, Messages, Controls, Math,
  AdvSmoothImageListBox;

type
  TDBAdvSmoothImageListBox = class;

  TImageListBoxDataBinding = class(TPersistent)
  private
    FOwner: TDBAdvSmoothImageListBox;
    FOnChange: TNotifyEvent;
    FCaptionField: String;
    FHintField: string;
    FImageField: string;
    FCaptionTemplate: String;
    procedure SetCaptionField(const Value: String);
    procedure SetImageField(const Value: string);
    procedure SetHintField(const Value: string);
    function GetDBImageListBox: TDBAdvSmoothImageListBox;
    procedure SetCaptionTemplate(const Value: String);
  protected
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TDBAdvSmoothImageListBox);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DBAdvSmoothImageListBox: TDBAdvSmoothImageListBox read GetDBImageListBox;
  published
    property CaptionField: String read FCaptionField write SetCaptionField;
    property CaptionTemplate: String read FCaptionTemplate write SetCaptionTemplate;
    property ImageField: string read FImageField write SetImageField;  // must be graphic
    property HintField: string read FHintField write SetHintField;
  end;

  TAdvListBoxDataLink = class(TDataLink)
  private
    FListBox: TDBAdvSmoothImageListBox;
    FModified: Boolean;
    FInUpdateData: Boolean;
    FLockEffects: Boolean;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure EditingChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create(AListBox: TDBAdvSmoothImageListBox);
    destructor Destroy; override;
    procedure Modified;
    procedure Reset;
    property ListBox: TDBAdvSmoothImageListBox read FListBox;
  end;

  TDataBindEvent = procedure (Sender: TObject; Item: TAdvSmoothImageListBoxItem; DBFields: TFields) of object;
  THTMLTemplateDataEvent = procedure(Sender: TObject; ItemIndex: Integer; FieldName: string; var Data: string) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvSmoothImageListBox = class(TAdvSmoothImageListBox)
  private
    FDataLink: TAdvListBoxDataLink;
    FDataBinding: TImageListBoxDataBinding;
    FPageMode: Boolean;
    FInternalChange: Boolean;
    FInternalSelectItem: Boolean;
    FOnDataBind: TDataBindEvent;
    FOnGetCaptionTemplateData: THTMLTemplateDataEvent;
    //--- DataLink
    procedure ActiveChange(Value: Boolean);
    procedure EditingChanged;
    procedure DataScroll(Distance: Integer);
    procedure RecordChanged(Field: TField);
    procedure DataChange;
    procedure UpdateData;
    //---
    procedure OnDataBindingChanged(Sender: TObject);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure UpdateActiveItem;
//    procedure UpdateLeftTopIndex;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetDataBinding(const Value: TImageListBoxDataBinding);
    procedure SetPageMode(const Value: Boolean);
    function GetSelectedIndex: Integer;
    function IsCachedNormalEmpty(Item: TAdvSmoothImageListBoxItem): Boolean;
    procedure UpdateDataBuffer;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure LoadItemData(Item: TAdvSmoothImageListBoxItem);
    procedure DoItemText(Item: TAdvSmoothImageListBoxItem); override;
    procedure DoItemImage(Item: TAdvSmoothImageListBoxItem); override;
    procedure DoSelectItem(NewItemIndex: Integer); override;
    procedure SelectImage(Key: Word; var itemindex: integer; itemselect: Boolean); override;

    procedure SetMultiSelect(Value: Boolean); override;
    procedure DoItemPropertyChange(Item: TAdvSmoothImageListBoxItem; PropID: Integer; var Value: Boolean); override;

    procedure DoSmoothScroll(CurrentPosition, EndPosition: Double); override;
    procedure DoLookup(ItemIndex: Integer); override;
    procedure ClearImageCache(FromIndex: Integer);

    function CheckDataSet: Boolean;
    function GetRecordCount: Integer;
    procedure UpdateItemCount;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default True; // Can be shifted to parent class
    function HTMLDBReplace(s: string; Dataset: Tdataset; ItemIndex: Integer; DataEvent: THTMLTemplateDataEvent): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure MouseWheelHandler(var Message: TMessage); override;
    procedure LoadFromDataSet;
    procedure Reload;
  published
    property DataBinding: TImageListBoxDataBinding read FDataBinding write SetDataBinding;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property PageMode: Boolean read FPageMode write SetPageMode default true;
    property OnDataBind: TDataBindEvent read FOnDataBind write FOnDataBind;
    property OnGetCaptionTemplateData: THTMLTemplateDataEvent read FOnGetCaptionTemplateData write FOnGetCaptionTemplateData;
  end;


implementation

uses
  VDBConsts, Consts;

type
   TProAdvSmoothImageListBoxItem = class(TAdvSmoothImageListBoxItem);

  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}
  IntPtr = Pointer;


//------------------------------------------------------------------------------

{ TAdvListBoxDataLink }

constructor TAdvListBoxDataLink.Create(AListBox: TDBAdvSmoothImageListBox);
begin
  inherited Create;
  FListBox := AListBox;
  VisualControl := True;
end;

//------------------------------------------------------------------------------

destructor TAdvListBoxDataLink.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.ActiveChanged;
begin
{$IFDEF DELPHI6_LVL}
  if Active and Assigned(DataSource) then
    if Assigned(DataSource.DataSet) then
      if DataSource.DataSet.IsUnidirectional then
        raise exception.Create('UniDirectional DataSet');
{$ENDIF}
  if not FLockEffects then
    FListBox.ActiveChange(Active);
  FModified := False;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.Modified;
begin
  FModified := True;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.DataSetChanged;
begin
  if not FLockEffects then
    FListBox.DataChange;
  FModified := False;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.DataSetScrolled(Distance: Integer);
begin
  if not FLockEffects then
    FListBox.DataScroll(Distance);
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.LayoutChanged;
begin
  inherited LayoutChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.EditingChanged;
begin
  if not FLockEffects then
    FListBox.EditingChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.RecordChanged(Field: TField);
begin
  if not FLockEffects then
    FListBox.RecordChanged(Field);
  FModified := False;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.UpdateData;
begin
  FInUpdateData := True;
  try
    if FModified and not FLockEffects then
      FListBox.UpdateData;
    FModified := False;
  finally
    FInUpdateData := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvListBoxDataLink.Reset;
begin
  if FModified then
    RecordChanged(nil)
  else
    Dataset.Cancel;
end;

//------------------------------------------------------------------------------

{ TDBAdvSmoothImageListBox }

constructor TDBAdvSmoothImageListBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TAdvListBoxDataLink.Create(Self);
  FDataLink.FListBox := Self;

  FDataBinding := TImageListBoxDataBinding.Create(Self);
  FDataBinding.OnChange := OnDataBindingChanged;
  ReadOnly := True;
  FPageMode := True;
end;

//------------------------------------------------------------------------------

destructor TDBAdvSmoothImageListBox.Destroy;
begin
  FDataBinding.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.MouseWheelHandler(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.DoLookup(ItemIndex: Integer);
var
  i: Integer;
begin
  inherited;

  if CheckDataSet and PageMode then
  begin
    FDataLink.DataSet.DisableControls;
    try
      i := GetRightBottomIndex;
      if (i >= 0) and (i < Items.Count) then
        SelectedItemIndex := i;
      i := GetLeftTopIndex;
      if (i >= 0) then
        SelectedItemIndex := i;
      SelectedItemIndex := ItemIndex;
    finally
      FDataLink.DataSet.EnableControls;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.DoSmoothScroll(CurrentPosition, EndPosition: Double);
var
  i: Integer;
  ForDir: Boolean;
begin
  inherited;

  if CheckDataSet and PageMode and not FInternalSelectItem then
  begin
    ForDir := CurrentPosition < EndPosition;
    //OutputDebugString(PChar('Dir: ' + BoolToStr(DirDown) ));

    if (CurrentPosition <> EndPosition) then
    begin
      if ForDir then
      begin
        i := GetRightBottomIndex;
        if (i >= 0) and (i < Items.Count) then
          SelectedItemIndex := i;
      end
      else
      begin
        i := GetLeftTopIndex;
        if (i >= 0) and (i < Items.Count) then
          SelectedItemIndex := i;
      end;
    end;
    //OutputDebugString(PChar('Top Index === ' + Inttostr(GetTopIndex) + ' Moved =' + inttostr(i)));
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.DoItemPropertyChange(Item: TAdvSmoothImageListBoxItem;
  PropID: Integer; var Value: Boolean);
begin
  inherited;
  
  if PageMode then
  begin
    case PropID of
      1, 2, 3: Value := True;
    end;
  end;
end;

//------------------------------------------------------------------------------

{
procedure TDBAdvSmoothImageListBox.UpdateLeftTopIndex;
var
  i, SI, j: Integer;
begin
  if not CheckDataSet or not PageMode then
    Exit;

  i := GetLeftTopIndex;
  SI := GetSelectedIndex;
  if Columns = 0 then
  begin
    if (SI >= 0) and (SI - i <> FDataLink.ActiveRecord) then
    begin
      i := i + ((SI - i) - FDataLink.ActiveRecord);
      SetLeftTopIndex(i);
    end;

    if (SI >= 0) and (not GetItemIsVisible(TProAdvSmoothImageListBoxItem(Items[SI]).ItemRect)) then
    begin
      j := 0;
      while not GetItemIsVisible(TProAdvSmoothImageListBoxItem(Items[SI]).ItemRect) do
      begin
        i := GetLeftTopIndex;
        if (SI > i) then
          SetLeftTopIndex(i + Rows)
        else if (SI < i) then
          SetLeftTopIndex(i - Rows);

        Inc(j);
        if (j >= Rows * 10) then
          Break;
      end;
    end;

  end
  else
  begin
    if (SI >= 0) and (SI - i <> FDataLink.ActiveRecord) then
    begin
      i := i + ((SI - i) - FDataLink.ActiveRecord);
      SetLeftTopIndex(i);
    end;

    if (SI >= 0) and (not GetItemIsVisible(TProAdvSmoothImageListBoxItem(Items[SI]).ItemRect)) then
    begin
      j := 0;
      while not GetItemIsVisible(TProAdvSmoothImageListBoxItem(Items[SI]).ItemRect) do
      begin
        i := GetLeftTopIndex;
        if (SI > i) then
          SetLeftTopIndex(i + Columns)
        else if (SI < i) then
          SetLeftTopIndex(i - Columns);

        Inc(j);
        if (j >= Columns * 10) then
          Break;
      end;
    end;
  end;
end;

}
//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.DoSelectItem(NewItemIndex: Integer);
var
  SI: Integer;
begin
  inherited;

  if not CheckDataSet or not PageMode then
    Exit;

  if FInternalChange then
    Exit;

  SI := NewItemIndex + 1;

  if (SI > 0) then
  begin
    FInternalChange := True;
    FDataLink.DataSet.MoveBy(SI - FDataLink.DataSet.RecNo);
    FInternalChange := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.SelectImage(Key: Word;
  var itemindex: integer; itemselect: Boolean);
begin
  inherited;
  UpdateDataBuffer;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.UpdateDataBuffer;
var
  i, OldSI: Integer;
begin
  if CheckDataSet and PageMode then
  begin
    OldSI := GetSelectedIndex;
    i := GetRightBottomIndex;
    if (i >= 0) and (i < Items.Count) then
      SelectedItemIndex := i;
    i := GetLeftTopIndex;
    if (i >= 0) then
      SelectedItemIndex := i;
    SelectedItemIndex := OldSI;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.DataScroll(Distance: Integer);
var
  RN: Integer;
begin
  if not CheckDataSet or not PageMode or FInternalChange then
    Exit;

  RN := FDataLink.DataSet.RecNo - 1;
  if (RN < Items.Count) and (RN >= 0) then
  begin
    FInternalChange := True;
    try
      SelectedItemIndex := RN;
      //UpdateLeftTopIndex;
    finally
      FInternalChange := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.UpdateActiveItem;
var
  RN, SI: Integer;
begin
  if not CheckDataSet or not PageMode or FInternalChange then
    Exit;

  SI := GetSelectedIndex;
  RN := FDataLink.DataSet.RecNo - 1;
  if (RN < Items.Count) and (RN >= 0) and (SI <> RN) then
  begin
    FInternalChange := True;
    try
      SelectedItemIndex := RN;
      //UpdateLeftTopIndex;
    finally
      FInternalChange := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.DoItemText(Item: TAdvSmoothImageListBoxItem);
begin
  inherited;
  
  LoadItemData(Item);
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.DoItemImage(
  Item: TAdvSmoothImageListBoxItem);
begin
  inherited;
  if not ItemAppearance.TextVisible then  // otherwise already loaded in DoItemText
    LoadItemData(Item);
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.LoadItemData(Item: TAdvSmoothImageListBoxItem);
var
  aField: TField;
  blobf: TBlobField;
  OldAR, SI, NAR, ItemIndex: Integer;
  s: TStream;
  itemcaption: String;
begin
  if PageMode and CheckDataSet then
  begin
    OldAR := FDataLink.ActiveRecord;

    SI := SelectedItemIndex;
    if (SI < 0) then
      Exit;
    ItemIndex := Item.Index;

    NAR := FDataLink.ActiveRecord;
    if (itemindex > SI) then
      NAR := FDataLink.ActiveRecord + ItemIndex - SI
    else if (itemindex < SI) then
      NAR := FDataLink.ActiveRecord + ItemIndex - SI;

    if (NAR >= FDataLink.BufferCount) then
    begin
      if Columns = 0 then
        FDataLink.BufferCount := FDataLink.BufferCount + Rows - 1
      else
        FDataLink.BufferCount := FDataLink.BufferCount + Columns - 1;
    end;

    if (NAR < 0) or (NAR >= FDataLink.BufferCount) then
      Exit;

    FDataLink.ActiveRecord := NAR;
    try
      if (DataBinding.CaptionTemplate <> '') then
      begin
        itemCaption := HTMLDBReplace(DataBinding.CaptionTemplate, FDataLink.DataSet, item.Index, FOnGetCaptionTemplateData);
        TProAdvSmoothImageListBoxItem(item).SetInternalCaptionText(itemcaption);
      end
      else if (DataBinding.CaptionField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.CaptionField);
        if Assigned(aField) then
          TProAdvSmoothImageListBoxItem(item).SetInternalCaptionText(aField.Text);
      end;

      if (DataBinding.HintField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.HintField);
        if Assigned(aField) then
          TProAdvSmoothImageListBoxItem(item).SetInternalHint(aField.Text);
      end;

      if (DataBinding.ImageField <> '') then
      begin
        aField := FDataLink.DataSet.Fieldbyname(DataBinding.ImageField);
        if Assigned(aField) and (aField.DataType in [ftBlob, ftGraphic]) and (IsCachedNormalEmpty(item)) then
        begin
          blobf := aField as TBlobField;
          s := FDataLink.DataSet.CreateBlobStream(blobf, bmRead);
          try
            s.Position := 0;
            TProAdvSmoothImageListBoxItem(item).LoadImageFromStream(s);
          finally
            s.Free;
          end;
        end;
      end;

    finally
      FDataLink.ActiveRecord := OldAR;
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothImageListBox.IsCachedNormalEmpty(
  Item: TAdvSmoothImageListBoxItem): Boolean;
begin
  Result := not (Assigned(TProAdvSmoothImageListBoxItem(item).CachedNormal) and (TProAdvSmoothImageListBoxItem(item).CachedNormal.Width > 0) and (TProAdvSmoothImageListBoxItem(item).CachedNormal.Height > 0) );
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.DataChange;
begin
  if not Assigned(FDataLink.DataSet) or not PageMode then
    Exit;

  UpdateItemCount;
  UpdateActiveItem;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.ActiveChange(Value: Boolean);
var
  rc, vic: Integer;
begin
  if Assigned(FDataLink) then
  begin
    if Assigned(FDataLink.DataSet) then
    begin
      if FDataLink.DataSet.Active then
      begin
        Items.Clear;
        if not FDataLink.DataSet.IsSequenced or not PageMode then
        begin
          PageMode := False;      // PageMode = true is not allowed with NonSequenced DataSets
          LoadFromDataSet;
        end
        else
        begin
          rc := GetRecordCount;
          ItemCount := rc;
          vic := GetVisibleItemCount;
          FDataLink.BufferCount := vic + 1;

          UpdateActiveItem;
        end;
      end
      else  // Active = False
      begin
        Items.Clear;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothImageListBox.CheckDataSet: Boolean;
begin
  Result := Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.ClearImageCache(FromIndex: Integer);
var
  i: Integer;
begin
  if PageMode then
  begin
    for i := FromIndex to Items.Count - 1 do
    begin
      TProAdvSmoothImageListBoxItem(Items[i]).LoadImageFromStream(nil);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.UpdateData;
begin
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.RecordChanged(Field: TField);
begin

end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.SetPageMode(const Value: Boolean);
begin
  if (FPageMode <> Value) then
  begin
    FPageMode := Value;
    if CheckDataSet then
      ActiveChange(FDataLink.DataSet.Active);
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.LoadFromDataSet;
var
  aField: TField;
  blobf: TBlobField;
  s: TStream;
  aItem: TAdvSmoothImageListBoxItem;
  itemCaption: String;
begin
  if CheckDataSet {and not PageMode} then
  begin
    FDataLink.DataSet.DisableControls;
    try
      Items.Clear;
      FDataLink.DataSet.First;
      while not FDataLink.DataSet.Eof do
      begin
        aItem := Items.Add;
        with aItem do
        begin
          if (DataBinding.CaptionTemplate <> '') then
          begin
            itemCaption := HTMLDBReplace(DataBinding.CaptionTemplate, FDataLink.DataSet, aItem.Index, FOnGetCaptionTemplateData);
            TProAdvSmoothImageListBoxItem(aitem).SetInternalCaptionText(itemcaption);
          end
          else if (DataBinding.CaptionField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.CaptionField);
            if Assigned(aField) then
              TProAdvSmoothImageListBoxItem(aitem).SetInternalCaptionText(aField.Text);
          end;

          if (DataBinding.HintField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.HintField);
            if Assigned(aField) then
              TProAdvSmoothImageListBoxItem(aitem).SetInternalHint(aField.Text);
          end;

          if (DataBinding.ImageField <> '') then
          begin
            aField := FDataLink.DataSet.Fieldbyname(DataBinding.ImageField);
            if Assigned(aField) and (aField.DataType in [ftBlob, ftGraphic]) then
            begin
              blobf := aField as TBlobField;
              s := FDataLink.DataSet.CreateBlobStream(blobf, bmRead);
              try
                s.Position := 0;
                TProAdvSmoothImageListBoxItem(aitem).LoadImageFromStream(s);
              finally
                s.Free;
              end;
            end;
          end;

        end;

        if Assigned(OnDataBind) then
          OnDataBind(Self, aItem, FDataLink.DataSet.Fields);
        FDataLink.DataSet.Next;
      end;
    finally
      FDataLink.DataSet.EnableControls;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.Reload;
begin
  //if not PageMode then
    LoadFromDataSet;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.EditingChanged;
begin
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothImageListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothImageListBox.GetDataSource: TDataSource;
begin
  if not (csDestroying in ComponentState) and Assigned(FDataLink) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothImageListBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothImageListBox.GetRecordCount: Integer;
begin
  Result := 0;
  if CheckDataSet then
    Result := FDataLink.DataSet.RecordCount;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothImageListBox.GetSelectedIndex: Integer;
begin
  Result := SelectedItemIndex;
end;

function TDBAdvSmoothImageListBox.HTMLDBReplace(s: string; Dataset: Tdataset;
  ItemIndex: Integer; DataEvent: THTMLTemplateDataEvent): string;
var
  beforetag, aftertag, fld, dbfld: string;
  i, j: integer;
begin
  beforetag := '';
  while Pos('<#', s) > 0 do
  begin
    i := pos('<#', s);
    beforetag := beforetag + copy(s, 1, i - 1); //part prior to the tag
    aftertag := copy(s, i, length(s)); //part after the tag
    j := pos('>', aftertag);
    fld := copy(aftertag, 1, j - 1);
    Delete(fld, 1, 2);
    Delete(s, 1, i + j - 1);

    dbfld := '';
    if Assigned(DataSet) then
    begin
      if DataSet.Active then
      begin
        dbfld := DataSet.FieldByName(fld).DisplayText;
        if Assigned(DataEvent) then
          DataEvent(self, ItemIndex, fld, dbfld);
      end
      else
        dbfld := '(' + fld + ')';
    end
    else dbfld := '(' + fld + ')';

    beforetag := beforetag + dbfld;
  end;

  Result := beforetag + s;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key in [VK_PRIOR, VK_NEXT, VK_END, VK_HOME, VK_LEFT, VK_UP,
    VK_RIGHT, VK_DOWN] then
    if not FDataLink.Edit then Key := 0;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    #32..#255:
      if not FDataLink.Edit then Key := #0;
    #27:
      FDataLink.Reset;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil)
      and (AComponent = DataSource) then
    DataSource := nil;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.OnDataBindingChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    Invalidate;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if Assigned(FDataLink) and PageMode then
    if Assigned(FDataLink.DataSet) then
      if FDataLink.DataSet.Active then
      begin
        FDataLink.BufferCount := GetVisibleItemCount + 1;
      end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.SetDataBinding(const Value: TImageListBoxDataBinding);
begin
  FDataBinding.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.SetMultiSelect(Value: Boolean);
begin
  if PageMode then  
    inherited SetMultiSelect(False)
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvSmoothImageListBox.UpdateItemCount;
var
  NC, i: Integer;
begin
  if CheckDataSet and PageMode then
  begin
    NC := GetRecordCount;
    if (NC <> ItemCount) then
    begin
      i := Max(0, GetSelectedIndex - 1);
      ItemCount := GetRecordCount;
      ClearImageCache(i);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvSmoothImageListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

//------------------------------------------------------------------------------

{ TImageListBoxDataBinding }

procedure TImageListBoxDataBinding.Assign(Source: TPersistent);
begin
  if (Source is TImageListBoxDataBinding) then
  begin
    CaptionField := (Source as TImageListBoxDataBinding).CaptionField;
    ImageField := (Source as TImageListBoxDataBinding).ImageField;
    HintField := (Source as TImageListBoxDataBinding).HintField;
    CaptionTemplate := (Source as TImageListBoxDataBinding).CaptionTemplate;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

constructor TImageListBoxDataBinding.Create(AOwner: TDBAdvSmoothImageListBox);
begin
  inherited Create;
  FOwner := AOwner;
  FCaptionField := '';
  FCaptionTemplate := '';
  FImageField := '';
  FHintField := '';
end;

//------------------------------------------------------------------------------

destructor TImageListBoxDataBinding.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

function TImageListBoxDataBinding.GetDBImageListBox: TDBAdvSmoothImageListBox;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

procedure TImageListBoxDataBinding.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TImageListBoxDataBinding.SetCaptionField(const Value: String);
begin
  if (FCaptionField <> Value) then
  begin
    FCaptionField := Value;
    Changed;
  end;
end;

procedure TImageListBoxDataBinding.SetCaptionTemplate(const Value: String);
begin
  if FCaptionTemplate <> value then
  begin
    FCaptionTemplate := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TImageListBoxDataBinding.SetImageField(const Value: string);
begin
  if (FImageField <> Value) then
  begin
    FImageField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TImageListBoxDataBinding.SetHintField(const Value: string);
begin
  if (FHintField <> Value) then
  begin
    FHintField := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

end.
